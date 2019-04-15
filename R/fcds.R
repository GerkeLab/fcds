
merge_fl_pop <- function(
  data,
  year_var = dx_year_mid,
  fl_pop = get_data("seer_pop_fl")
) {
  year_var <- rlang::enquo(year_var)
  year_var_name <- rlang::quo_name(year_var)
  stopifnot(year_var_name %in% names(data))

  fl_pop <- fl_pop %>%
    rename(!!year_var_name := year) %>%
    nest(setdiff(names(.), c(common_names(data, fl_pop), year_var_name)),
         .key = "population")

  # TODO: message here about common_names()?

  left_join(data, fl_pop, by = common_names(data, fl_pop))
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
