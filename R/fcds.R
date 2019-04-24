#' @export
join_population <- function(
  data,
  population = fcds::seer_pop_fl,
  by_year = NULL
) {
  if (!is.null(by_year)) {
    return(join_population_by_year(data, population, by_year))
  }

  common_names_ <- common_names(data, population)
  pop_vars <- setdiff(names(population), common_names_)

  # Nest population specific variables
  population <- population %>% tidyr::nest(pop_vars, .key = "population")

  dplyr::left_join(data, population, by = common_names_)
}

join_population_by_year <- function(
  data,
  population = fcds::seer_pop_fl,
  by_year = c("year_mid" = "year")
) {
  if (!is.character(by_year) || length(by_year) != 1) {
    abort(glue(
      "`by_year` must be a length-1 character vector: ",
      "either the name of the year column in both `data` and `population`, or ",
      "a named vector where the name provides the year column name in `data`."
    ))
  }
  if (is.null(names(by_year))) by_year <- setNames(nm = by_year)
  validate_all_have_var(names(by_year), data = data)
  validate_all_have_var(unname(by_year), population = population)

  population <- population %>% dplyr::rename(!!!by_year)
  join_population(data, population, by_year = NULL)
}

#' @export
summarize_fcds <- function(
  fcds,
  ...,
  sex = NULL,
  race = NULL,
  year = NULL,
  county_name = NULL,
  hispanic = NULL,
  default_groups = c("year_mid", "age_group")
) {

  filters <- list(sex = sex, race = race, year = year,
                  county_name = county_name, hispanic = hispanic)
  filters <- purrr::imap(filters, valid_fcds_const)
  for (var in names(filters)) {
    fcds <- filter_fcds(fcds, var, filters[[var]])
  }

  if (!"year_mid" %in% names(fcds)) {
    fcds <- add_mid_year(fcds)
  }

  # Initial counting has to be by year and age_group
  groups <- group_vars(fcds)
  default_groups <- union(default_groups, c("year_mid", "age_group"))
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

merge_fl_counties <- function(data) {
  requires_package("USAboundaries", "merge_fl_counties()")
  florida_counties <- USAboundaries::us_counties(states = "Florida") %>%
    select(fips_code = countyfp, geometry)

  if (!"county_fips" %in% names(data)) {
    county_fips <- get_data("county_fips_code")
    data <-
      data %>%
      left_join(county_fips, by = "county_name") %>%
      mutate(fips_code = sprintf("%03d", as.integer(fips_code)))
  }

  left_join(data, florida_counties, by = "fips_code")
}


# FCDS Variable Names -----------------------------------------------------

#' Select Common FCDS Variable Groups
#'
#' The FCDS data set includes a number of thematically-groups variables. This
#' function helps select those groups by returning variable names.
#'
#' @examples
#' fcds_vars("demo")
#' fcds_vars("demographics")
#'
#' fcds_vars("id", "demo", "pop")
#'
#' fcds_vars("seer", "tobacco", "seer")
#'
#' dplyr::tibble(
#'   patient_id = 1:5,
#'   year = 2000,
#'   county_name = "Pinellas",
#'   age_group = c("65 - 69", "10 - 14", "25 - 29", "70 - 74", "40 - 44"),
#'   cancer_status = c("Evidence of tumor", "Unknown", "No evidence of tumor",
#'                     "No evidence of tumor", "No evidence of tumor")
#' ) %>%
#'   fcds_vars(.data = ., "id", "pop", "demo")
#'
#' @param ... Variable group names, as characters. Partial matching is allowed
#'   and the matching is not case-sensitive. Possible variable groups include
#'   `"id"`, `"demographics"`, `"cancer"`, `"icdo3"`, `"population"`, `"seer"`,
#'   `"tobacco"`.
#'
#'   A group may be input multiple times, but only the first appearance will be
#'   used. The final variable list is ordered by the requested group, unless
#'   `.data` is supplied.
#' @param .data If `.data`` is included, then the variables in the input data
#'   are subset to only those appearing in the selected groups, in the order
#'   they appear in the original data.
#' @return A character vector of column names, or, if `.data` is provided, a
#'   data frame subset to include columns matching the requested groups.
#' @export
fcds_vars <- function(..., .data = NULL) {
  choices <- c("id", "demographics", "cancer", "icdo3",
               "population", "seer", "tobacco")
  group <- tolower(c(...))
  group_match <- purrr::map_chr(group, ~ {
    tryCatch({
      match.arg(.x, several.ok = TRUE, choices = choices)
    }, error = function(e) abort(glue(
      "'{.x}' doesn't match any valid variable groups. ",
      "Valid groups include: ",
      "{paste(choices, collapse = ', ')}"
    )))
  })

  var_names <-
    purrr::map(unique(group_match), fcds_var_group) %>%
    purrr::reduce(union)

  if (is.null(.data)) {
    return(var_names)
  } else {
    .data[, intersect(names(.data), var_names)]
  }
}

fcds_var_group <- function(
  group = c("id", "demographics", "cancer", "icdo3",
            "population", "seer", "tobacco")
) {
  switch(
    match.arg(group),
    id = c(
      "patient_id",
      "year",
      "year_mid"
    ),
    demographics = c(
      "age_group",
      "race",
      "sex",
      "hispanic",
      "marital_status",
      "birth_country",
      "birth_state",
      "primary_payer"
    ),
    cancer = c(
      "cancer_status",
      "cancer_site_group",
      "cancer_site_specific",
      "cancer_confirmation",
      "cancer_reporting_source",
      "cancer_laterality",
      "cancer_grade",
      "cancer_ICDO3_histology",
      "cancer_ICDO3_behavior",
      "cancer_ICDO3_morphology",
      "cancer_ICDO3_conversion"
    ),
    icdo3 = c(
      "cancer_ICDO3_histology",
      "cancer_ICDO3_behavior",
      "cancer_ICDO3_morphology",
      "cancer_ICDO3_conversion"
    ),
    population = c(
      "year",
      "year_mid",
      "county_name",
      "county_fips",
      "state",
      "florida_resident",
      "country"
    ),
    seer = c(
      "seer_stage_1977",
      "seer_stage_2000",
      "seer_stage",
      "seer_stage_derived_1977",
      "seer_stage_derived_2000"
    ),
    tobacco = c(
      "tobacco_cigarette",
      "tobacco_other",
      "tobacco_smokeless",
      "tobacco_nos"
    )
  )
}

