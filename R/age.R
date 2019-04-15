
# Age Groups --------------------------------------------------------------

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


# Age Adjustment ----------------------------------------------------------

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


age_adjust_finalize <- function(
  .data,
  outcome_var = n,
  std_pop_data = get_data("seer_std_ages")
) {
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
