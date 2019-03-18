FCDS Age Adjustment Notes
================
Thursday, Jan 24, 2019

<!-- Start Document Here -->

## U.S. Cancer Statistics Data Visualizations Tool Technical Notes

<https://www.cdc.gov/cancer/uscs/pdf/uscs-datavisualizationtool-technicalnotes-508.pdf>

## County-Level Population Files

<https://seer.cancer.gov/popdata/download.html>

> The U.S. Census Bureau annually releases unbridged population
> estimates for five-year age groups and race at the county level. The
> Census Bureau does not release bridged race estimates by single year
> of age at the county level due to concerns about the reliability of
> these estimates. However, these estimates are provided to the National
> Cancer Institute to meet programmatic needs such as the creation of
> age groupings that differ from the standard groupings used by the
> Census Bureau. Users of the single-year-of-age county-level bridged
> race population estimates should carefully consider the limited
> reliability of these estimates.

## Age Adjustment

An age-adjusted rate is a weighted average of the age-specific (crude)
rates according to the proportion of the population in the corresponding
age group of a standard population.

The age adjusted rates are calculated according
to

\[\left. r_{adj} \right\vert_x^y = \sum_{\text{age}=x}^y \left\lbrack \frac{count_{\text{age}}}{pop_{\text{age}}} \times \frac{stdpop_{\text{age}}}{\sum_{j=x}^{y} stdpop_j} \right\rbrack\]

This was implemented in code as:

``` r
age_adjust <- function(
  .data,
  outcome_var = n,
  year_var = dx_year_mid,
  fl_pop = get_data("seer_fl_pop"),
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

  #  Get groups from .data
  #  Roll up FL and std pop data for those groups
  #  Merge into fcds
  #  Calculate age-adjusted rate
  .data <- merge_fl_pop(.data, !!year_var, fl_pop)

  data_groups <- setdiff(group_vars(.data), year_var_name)
  .data <- .data %>%
    group_by(!!!rlang::syms(data_groups))

  if (keep_age_group) return(.data)
  .data %>% age_adjust_finalize(!!outcome_var, std_pop_data)
}

merge_fl_pop <- function(
  .data,
  year_var = dx_year_mid,
  fl_pop = get_data("seer_fl_pop")
) {
  year_var <- rlang::enquo(year_var)
  year_var_name <- rlang::quo_name(year_var)
  stopifnot(year_var_name %in% names(.data))

  data_groups <- setdiff(group_vars(.data), year_var_name)
  fl_groups <- c(intersect(data_groups, names(fl_pop)), "year")
  if ("age_group" %in% fl_groups) {
    fl_groups <- rev(union("age_group", rev(fl_groups)))
  }

  fl_pop <-
    fl_pop %>%
    group_by(., !!!rlang::syms(fl_groups)) %>%
    summarize(population = sum(population)) %>%
    rename(!!year_var_name := year)

  left_join(.data, fl_pop, by = common_names(.data, fl_pop))
}

age_adjust_finalize <- function(
  .data,
  outcome_var = n,
  std_pop_data = get_data("seer_std_ages")
) {
  outcome_var <- rlang::enquo(outcome_var)

  age_groups <- unique(.data$age_group)

  # Get standard population
  .data <-
    std_pop_data %>%
    filter(age_group %in% age_groups) %>%
    select(age_group, std_pop) %>%
    mutate(w = std_pop / sum(std_pop)) %>%
    left_join(.data, ., by = "age_group")

  # Get data groups and make sure "age_group" is last for summary
  data_groups <- group_vars(.data)
  data_groups <- setdiff(data_groups, "age_group")

  .data %>%
    ungroup() %>%
    mutate(
      rate = !!outcome_var / population * w
    ) %>%
    group_by(!!!rlang::syms(data_groups)) %>%
    summarize_at(vars(n, population, rate), sum) %>%
    group_by(!!rlang::sym(data_groups[length(data_groups)]), add = TRUE) %>%
    mutate(rate = rate * 100000)
}
```