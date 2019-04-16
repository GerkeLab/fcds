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
  by_year = c("dx_year_mid" = "year")
) {
  if (length(by_year) != 1) {
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
