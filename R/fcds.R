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
