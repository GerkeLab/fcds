# Join FCDS with Data -----------------------------------------------------

join_population <- function(
  data,
  population = fcds::seer_pop_fl,
  by_year = NULL,
  keep_pop_vars = FALSE
) {
  if (!is.null(by_year)) {
    return(join_population_by_year(data, population, by_year))
  }

  common_names_ <- common_names(data, population)
  pop_vars <- if (keep_pop_vars) {
    setdiff(names(population), common_names_)
  } else "population"

  pop_groups <- setdiff(common_names_, pop_vars)

  # Nest population specific variables
  population <- population %>%
    quiet_semi_join(ungroup(data), by = pop_groups) %>%
    select(pop_groups, pop_vars) %>%
    tidyr::nest(pop_vars, .key = "population")

  with_ungroup(data, ~ quiet_left_join(., population, by = common_names_))
}

join_population_by_year <- function(
  data,
  population = fcds::seer_pop_fl,
  by_year = "year"
) {
  if (!is.character(by_year) || length(by_year) != 1) {
    abort(glue(
      "`by_year` must be a length-1 character vector: ",
      "either the name of the year column in both `data` and `population`, or ",
      "a named vector where the name provides the year column name in `data`."
    ))
  }
  if (is.null(names(by_year))) by_year <- set_names(by_year)
  validate_all_have_var(names(by_year), data = data)
  validate_all_have_var(unname(by_year), population = population)

  population <- population %>% dplyr::rename(!!!by_year)
  join_population(data, population, by_year = NULL)
}

#' Count FCDS Cases
#'
#' Helper function to count cancer cases in the FCDS data by year and age group,
#' in addition to any groups already present in the data. For convenience, you
#' may additionally filter to include particular values of sex, race, year,
#' county name and hispanic ethnicity. See [fcds_const()] for more information
#' about possible values for these variables. By default, `count_fcds()` ensures
#' that `age_group`, `year_group`, and `year` are included in the grouping
#' variables if they are present in the data. If they are not, or if they are
#' not present in the FCDS data, then it would be better to use [dplyr::count()]
#' directly.
#'
#' @examples
#'
#' fcds_example %>%
#'   dplyr::filter(county_name == "Pinellas") %>%
#'   count_fcds(cancer_site_group, sex = "Male")
#'
#' @return A grouped data frame with counts. The output groups includes the
#'   union of the groups of the original input `data`, the groups specified by
#'   the columns indicated in `...`, and the `default_groups` added by
#'   `count_fcds()` (modifyable by the `default_groups` argument).
#'
#'   All factor levels will be modified to include only those levels that appear
#'   in the final output across all groups.
#' @param data A data frame
#' @param ... Unquoted column names to be added to the grouping of the output
#'   and subsequent counting.
#' @param sex Character vector of values of `sex` to be included in count
#' @param race Character vector of values of `race` to be included in count
#' @param origin Character vector of values of `origin` to be included in count
#' @param moffitt_catchment Limit counties to those in the catchment area of the
#'   [Moffitt Cancer Center](https://moffitt.org).
#' @param default_groups Variables that should be included in the grouping,
#'   prior to counting cancer cases. Set to `NULL` to use only the groups
#'   already present in the input data.
#' @export
count_fcds <- function(
  data,
  ...,
  sex = NULL,
  race = NULL,
  origin = NULL,
  moffitt_catchment = FALSE,
  default_groups = c("year_group", "year", "age_group")
) {
  filters <- list(
    sex = sex,
    race = race,
    origin = origin
  )
  if (moffitt_catchment) filters$county_name <- fcds_const("moffitt_catchment")

  filters <- purrr::compact(filters)

  for (var in names(filters)) {
    data <- filter_fcds(data, var, filters[[var]])
  }

  if (!"year" %in% names(data)) {
    data <- add_mid_year_groups(data)
  }

  # Initial counting has to be by year and age_group
  groups <- union(group_vars(data), intersect(default_groups, names(data)))

  data %>%
    group_by(..., !!!rlang::syms(groups)) %>%
    dplyr::count() %>%
    with_ungroup(discard_unobserved_levels)
}

filter_fcds <- function(fcds, var_name, values) {
  values <- valid_fcds_const(var_name, values)
  if (is.null(values) || !length(values)) return(fcds)
  var <- rlang::sym(var_name)
  fcds %>%
    filter(!!var %in% values) %>%
    group_by(!!var, add = TRUE)
}

discard_unobserved_levels <- function(data) {
  discard_levels <- function(fct) {
    fct_observed <- intersect(levels(fct), as.character(unique(fct)))
    factor(as.character(fct), fct_observed)
  }

  data %>%
    dplyr::mutate_if(is.factor, discard_levels)
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
      "{double_quote(choices)}"
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
      "year_group",
      "year"
    ),
    demographics = c(
      "age_group",
      "race",
      "sex",
      "origin",
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
      "year_group",
      "year",
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

