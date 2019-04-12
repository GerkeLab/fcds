#' Convert Age Group Column to Low and High Age Range
#'
#' Takes the age group stored in `age_var` and creates two variables,
#' `age_low` and `age_high` with the age boundaries of the group.
#'
#' @param .data A data frame
#' @param age_var Unquoted column name containing the age grouping.
#' @inheritParams tidyr::separate
#' @family age processors
#' @export
expand_age_groups <- function(.data, age_var = age_group, sep = "\\s*-\\s*") {
  age_var <- rlang::enquo(age_var)
  .data %>%
    separate(!!age_var, into = paste0("age_", c("low", "high")),
             sep = sep, remove = FALSE, fill = "right") %>%
    mutate(
      age_low = sub("+", "", age_low, fixed = TRUE),
      age_low = ifelse(is.na(age_low), "0", age_low)
    ) %>%
    mutate_at(vars(age_low, age_high), as.numeric) %>%
    mutate(age_high = if_else(is.na(age_high) & !is.na(age_low), Inf, age_high))
}

#' Filter Data by Age Range
#'
#' Filters data to include persons with ages in the range between `age_low` and
#' `age_high`.
#'
#' @inheritParams expand_age_groups
#' @param age_low Youngest age (inclusive)
#' @param age_high Eldest age (inclusive)
#' @family age processors
#' @export
filter_age <- function(.data, age_low = 0, age_high = Inf, age_var = age_group) {
  age_var <- rlang::enquo(age_var)
  age_var_name <- rlang::quo_name(age_var)
  if (!"age_low" %in% names(.data)) {
    stopifnot(age_var_name %in% names(.data))
    .data <- expand_age_groups(.data, age_var = !!age_var)
  }
  stopifnot("age_low" %in% names(.data))
  stopifnot("age_high" %in% names(.data))
  .data %>%
    filter(age_low >= !!age_low, age_high <= !!age_high)
}

#' Complete Age Groups
#'
#' Completes age groups by adding missing age groups, either within the age
#' range from `low` to `high` or using the full age list from the SEER data.
#'
#' @param .data A data frame
#' @param low Low age boundary (inclusive)
#' @param high High age boundary (inclusive)
#' @inheritParams tidyr::complete
#' @param include_unknown Should the "Unknown" age group be included?
#' @family age processors
#' @export
complete_age_groups <- function(
  .data,
  low = NULL,
  high = NULL,
  fill = list(n = 0),
  include_unknown = FALSE
) {
  stopifnot("age_group" %in% names(.data))
  ages <- tibble(age_group = fcds_const("age_group")) %>%
    { if (include_unknown) . else filter(., age_group != "Unknown") } %>%
    expand_age_groups()
  if (!is.null(low)) ages <- filter(ages, age_low >= low)
  if (!is.null(high)) ages <- filter(ages, age_high <= high)
  .data %>%
    complete(age_group = ages$age_group, fill = fill) %>%
    select(colnames(.data))
}

#' Complete Year Groups
#'
#' Completes year groups with expected years.
#'
#' @family year processors
#' @export
complete_year_groups <- function(.data, start = NULL, end = NULL, year_var = "dx_year", fill = list(n = 0)) {
  stopifnot(year_var %in% names(.data))
  years <- tibble(year = fcds_const("year")) %>%
    separate(year, c("start", "end"), sep = "\\s*-\\s*", remove = FALSE) %>%
    mutate_at(vars(start, end), as.integer)

  if (!is.null(start)) {
    start <- as.integer(start)
    years <- filter(years, start >= !!start)
  }
  if (!is.null(end)) {
    end <- as.integer(end)
    years <- filter(years, end >= !!end)
  }

  data_groups <- groups(.data) %>% set_names(group_vars(.data))
  data_groups <- data_groups[setdiff(names(data_groups), year_var)]

  .data %>%
    ungroup() %>%
    complete(!!year_var := years$year, nesting(!!!data_groups), fill = fill) %>%
    group_by(!!!data_groups) %>%
    select(colnames(.data))
}

#' Add Mid-Year Column
#'
#' Adds a new column containing the midpoint year of the range given in the
#' column indicated by `year_var`.
#'
#' @family year processors
#' @export
add_mid_year <- function(.data, year_var = dx_year, sep = "-") {
  year_var <- rlang::enquo(year_var)
  year_var_name <- paste0(rlang::quo_name(year_var), "_mid")

  if (year_var_name %in% names(.data)) return(.data)
  stopifnot(rlang::quo_name(year_var) %in% names(.data))

  mutate(.data, !!year_var_name := mid_year(!!year_var))
}

mid_year <- function(years, sep = "-", offset = 2) {
  low_year_regex <- paste0("(\\d{2,4}).*", sep, ".*")
  paste(as.integer(sub(low_year_regex, "\\1", years)) + offset)
}

get_data <- function(x) {
  if (!exists(x)) {
    rds_file <- here::here("data", paste0(x, ".rds"))
    if (!file.exists(rds_file)) {
      rlang::abort(
        glue::glue("Unable to find RDS data file for {x} in data/")
      )
    }
    readRDS(rds_file)
  } else {
    get(x)
  }
}

age_adjust <- function(
  .data,
  outcome_var = n,
  year_var = dx_year_mid,
  fl_pop = get_data("seer_pop_fl"),
  std_pop_data = get_data("seer_std_ages"),
  keep_age_group = FALSE
) {
  outcome_var <- rlang::enquo(outcome_var)
  outcome_var_name <- rlang::quo_name(outcome_var)
  year_var <- rlang::enquo(year_var)
  year_var_name <- rlang::quo_name(year_var)

  # Age and Year are required in source data
  stopifnot(year_var_name %in% names(.data))
  stopifnot("age_group" %in% names(.data))
  .data <- filter(.data, age_group != "Unknown")
  data_groups <- groups(.data)

  #  Get groups from .data
  #  Roll up FL and std pop data for those groups
  #  Merge into fcds
  #  Calculate age-adjusted rate
  .data <- merge_fl_pop(.data, !!year_var, fl_pop) %>%
    mutate(population = map(population, "population") %>% map_int(sum)) %>%
    summarize_at(vars(!!outcome_var, population), sum) %>%
    group_by(!!!data_groups) # restore groups

  data_groups <- setdiff(group_vars(.data), year_var_name)
  .data <- .data %>%
    group_by(!!!rlang::syms(data_groups))

  if (keep_age_group) return(.data)
  .data %>% age_adjust_finalize(!!outcome_var, std_pop_data)
}

merge_fl_pop <- function(
  .data,
  year_var = dx_year_mid,
  fl_pop = get_data("seer_pop_fl")
) {
  year_var <- rlang::enquo(year_var)
  year_var_name <- rlang::quo_name(year_var)
  stopifnot(year_var_name %in% names(.data))

  fl_pop <- fl_pop %>%
    rename(!!year_var_name := year) %>%
    nest(setdiff(names(.), c(common_names(.data, fl_pop), year_var_name)),
         .key = "population")

  # TODO: message here about common_names()?

  left_join(.data, fl_pop, by = common_names(.data, fl_pop))
}

age_adjust_finalize <- function(.data,
                                outcome_var = n,
                                std_pop_data = get_data("seer_std_ages")) {
  outcome_var <- rlang::enquo(outcome_var)

  age_groups <- unique(.data$age_group)

  # Get standard population
  std_pop_relevant <-
    std_pop_data %>%
    filter(age_group %in% age_groups) %>%
    select(age_group, std_pop) %>%
    mutate(w = std_pop / sum(std_pop))

  .data <- left_join(.data, std_pop_relevant, by = "age_group")

  # Get groups from the data other than "age_group" because we'll be
  # summing over the "age_group" column
  data_groups <- group_vars(.data) %>% setdiff("age_group") %>% rlang::syms()

  .data %>%
    ungroup() %>%
    mutate(
      rate = !!outcome_var / population * w
    ) %>%
    group_by(!!!data_groups) %>%
    summarize_at(vars(n, population, rate), sum) %>%
    # restore all groups
    group_by(!!!data_groups) %>%
    mutate(rate = rate * 100000)
}

summarize_fcds <- function(
  fcds,
  ...,
  sex = NULL,
  race = NULL,
  year = NULL,
  county_name = NULL,
  hispanic = NULL,
  default_groups = c("dx_year", "age_group")
) {

  filters <- list(sex = sex, race = race, year = year,
                  county_name = county_name, hispanic = hispanic)
  filters <- imap(filters, valid_fcds_const)
  for (var in names(filters)) {
    fcds <- filter_fcds(fcds, var, filters[[var]])
  }

  if (!"dx_year_mid" %in% names(fcds)) {
    fcds <- add_mid_year(fcds)
  }

  # Initial counting has to be by year and age_group
  groups <- group_vars(fcds)
  default_groups <- union(default_groups, c("dx_year_mid", "age_group"))
  groups <- union(default_groups, groups)

  fcds %>%
    group_by(!!!rlang::syms(groups)) %>%
    count()
}

filter_fcds <- function(fcds, var_name, values) {
  values <- valid_fcds_const(values, var_name)
  if (is.null(values) || !length(values)) return(fcds)
  var <- rlang::sym(var_name)
  fcds %>%
    filter(!!var %in% values) %>%
    group_by(!!var, add = TRUE)
}


fcds_const <- function(const = c("year", "county_name", "sex", "race",
                                 "hispanic", "age_group")) {
  switch(
    match.arg(const),
    "year" = c("1981-1985", "1986-1990", "1991-1995", "1996-2000",
               "2001-2005", "2006-2010", "2011-2015"),
    "county_name" = c("Alachua", "Baker", "Bay", "Bradford", "Brevard", "Broward",
                      "Calhoun", "Charlotte", "Citrus", "Clay", "Collier", "Columbia",
                      "DeSoto", "Dixie", "Duval", "Escambia", "Flagler", "Franklin",
                      "Gadsden", "Gilchrist", "Glades", "Gulf", "Hamilton", "Hardee",
                      "Hendry", "Hernando", "Highlands", "Hillsborough", "Holmes",
                      "Indian River", "Jackson", "Jefferson", "Lafayette", "Lake",
                      "Lee", "Leon", "Levy", "Liberty", "Madison", "Manatee", "Marion",
                      "Martin", "Miami-Dade", "Monroe", "Nassau", "Okaloosa", "Okeechobee",
                      "Orange", "Osceola", "Palm Beach", "Pasco", "Pinellas", "Polk",
                      "Putnam", "Santa Rosa", "Sarasota", "Seminole", "St. Johns",
                      "St. Lucie", "Sumter", "Suwannee", "Taylor", "Union", "Volusia",
                      "Wakulla", "Walton", "Washington"),
    "sex" = c("Female", "Male", "Unknown"),
    "race" = c("Black", "Other", "Unknown", "White"),
    "hispanic" = c("Hispanic", "Not Hispanic", "Unknown"),
    "age_group" = c("0 - 4", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34",
                "35 - 39", "40 - 44", "45 - 49", "5 - 9", "50 - 54", "55 - 59",
                "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85+",
                "Unknown")
  )
}

valid_fcds_const <- function(value, value_name, var_name = value_name, stop = TRUE) {
  if (is.null(value)) return(NULL)
  value <- paste(value)
  not_in_const <- vapply(value, function(x) !x %in% fcds_const(value_name), logical(1))
  if (any(not_in_const)) {
    if (stop) rlang::abort(
      glue::glue("Invalid '{var_name}': \"{value[not_in_const][1]}\" is not one of the options ",
                 "in `fcds_const(\"{value_name}\")`")
    ) else {
      rlang::warn(paste("Ignoring invalid values in", var_name))
    }
    value <- value[!not_in_const]
  }
  value
}

common_names <- function(x, y) intersect(names(x), names(y))

group_drop <- function(.data, ..., .remove = FALSE) {
  group_var <- rlang::enquos(...)
  group_var_name <- map_chr(group_var, rlang::quo_name)
  are_in_groups <- map_lgl(group_var_name, ~ . %in% group_vars(.data))
  if (!any(are_in_groups)) return(.data)
  .groups <- groups(.data) %>% set_names(group_vars(.data))
  .groups <- .groups[setdiff(names(.groups), group_var_name)]
  .data <- .data %>% ungroup() %>% group_by(!!!.groups)
  if (!.remove) return(.data)
  select(.data, -group_var_name)
}

merge_fl_counties <- function(.data) {
  requires_package("USAboundaries", "merge_fl_counties()")
  florida_counties <- USAboundaries::us_counties(states = "Florida") %>%
    select(fips_code = countyfp, geometry)

  if (!"county_fips" %in% names(.data)) {
    county_fips <- get_data("county_fips_code")
    .data <-
      .data %>%
      left_join(county_fips, by = "county_name") %>%
      mutate(fips_code = sprintf("%03d", as.integer(fips_code)))
  }

  left_join(.data, florida_counties, by = "fips_code")
}
