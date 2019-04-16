
# Age Groups --------------------------------------------------------------

#' Convert Age Group Column to Low and High Age Range
#'
#' Takes the age group stored in `age_var` and creates two variables,
#' `age_low` and `age_high` with the age boundaries of the group. By default,
#' `expand_age_groups()` assumes that the age group definitions are separated
#' by a dash (`"-"`), possibly with whitespace on either side. The separator
#' can be specified using the `sep` argument.
#'
#' @examples
#' d_age_group <- tibble::tibble(
#'   id = 1:4,
#'   age_group = c("0 - 4", "10 - 14", "65 - 69", "85+")
#' )
#'
#' expand_age_groups(d_age_group)
#'
#' @param data A data frame.
#' @param age_var Unquoted column name containing the age grouping.
#' @inheritParams tidyr::separate
#' @family age processors
#' @export
expand_age_groups <- function(
  data,
  age_var = age_group,
  sep = "\\s*-\\s*",
  ...,
  age_low_var = age_low,
  age_high_var = age_high
) {
  age_var           <- enquo(age_var)
  age_low_var       <- enquo(age_low_var)
  age_low_var_name  <- quo_name(age_low_var)
  age_high_var      <- enquo(age_high_var)
  age_high_var_name <- quo_name(age_high_var)

  data %>%
    separate(!!age_var, into = c(age_low_var_name, age_high_var_name),
             sep = sep, remove = FALSE, fill = "right") %>%
    mutate(
      !!age_low_var_name := sub("+", "", !!age_low_var, fixed = TRUE),
      !!age_low_var_name := ifelse(
        (is.na(!!age_low_var) | !!age_low_var == "") & !is.na(!!age_high_var),
        "0", !!age_low_var)
    ) %>%
    mutate_at(c(age_low_var_name, age_high_var_name), as.numeric) %>%
    mutate(!!age_high_var_name := if_else(
      is.na(!!age_high_var) & !is.na(!!age_low_var), Inf, !!age_high_var))
}

#' Filter Data by Age Range
#'
#' Filters data to include persons with ages in the range between `age_low` and
#' `age_high`. If `age_var` has not been expanded into low and high ages of the
#' range, the input data is first passed to [expand_age_groups()]. If the
#' boundary age lies within a group, that group is _not included_ in the output.
#'
#' @examples
#' d_age_group <- tibble::tibble(
#'   id = 1:4,
#'   age_group = c("0 - 4", "10 - 14", "65 - 69", "85+")
#' )
#'
#' d_age_group %>%
#'   filter_age(age_low = 0, age_high = 15)
#'
#' d_age_group %>%
#'   filter_age(age_low = 65)
#'
#' # Notice that the "65 - 69" group is *not* included
#' d_age_group %>%
#'   filter(age_high = 66)
#'
#' @inheritParams expand_age_groups
#' @param age_low Youngest age (inclusive).
#' @param age_high Eldest age (inclusive).
#' @family age processors
#' @export
filter_age <- function(
  data,
  age_low = 0,
  age_high = Inf,
  age_var = age_group
) {
  age_var <- enquo(age_var)
  age_var_name <- quo_name(age_var)
  if (!"age_low" %in% names(data)) {
    stopifnot(age_var_name %in% names(data))
    data <- expand_age_groups(data, age_var = !!age_var)
  }
  stopifnot("age_low" %in% names(data))
  stopifnot("age_high" %in% names(data))
  data %>%
    filter(age_low >= !!age_low, age_high <= !!age_high)
}

#' Complete Age Groups
#'
#' Completes age groups by adding missing age groups, either within the age
#' range from `age_low` to `age_high` or using the full age list from
#' [seer_std_ages].
#'
#' @examples
#' d_age_group <- tibble::tibble(
#'   age_group = c("10 - 14", "15 - 19", "25 - 29"),
#'   n = 10:12
#' ) %>%
#'   complete_age_groups(10, 35)
#'
#' @inheritParams filter_age
#' @inheritParams tidyr::complete
#' @param include_unknown Should the "Unknown" age group be included?
#' @param ... If `age_low` and `age_high` are missing from
#' @family age processors
#' @export
complete_age_groups <- function(
  data,
  age_low = NULL,
  age_high = NULL,
  ...,
  age_var = age_group,
  fill = list(n = 0),
  include_unknown = FALSE,
  std_age_groups = fcds_const("age_group")
) {
  age_var <- enquo(age_var)
  age_var_name <- quo_name(age_var)
  stopifnot(age_var_name %in% names(data))

  ages <- tibble(age_group = std_age_groups)
  if (!include_unknown) ages <- filter(ages, age_group != "Unknown")
  ages <- suppressWarnings(expand_age_groups(ages))

  if (!is.null(age_low)) ages <- filter(ages, age_low >= !!age_low)
  if (!is.null(age_high)) ages <- filter(ages, age_high <= !!age_high)

  common_age_groups <- union(ages$age_group, paste(data[[age_var_name]]))

  data %>%
    mutate(
      !!age_var_name := paste(!!age_var),
      !!age_var_name := factor(!!age_var, common_age_groups, ordered = TRUE)
    ) %>%
    complete(!!age_var, fill = fill) %>%
    select(colnames(data))
}

#' Standardize Age Groups
#'
#' Standardizes age groups to match [seer_std_ages]. Adds an `age_group` column,
#' or overwites the existing `age_group` column if present. The `age_var` column
#' can be either an actual age or an age group. If the column is numeric, it
#' assumed to be an actual age, otherwise if it contains any non-numeric
#' characters it is assumed to be an age group that will be expanded using
#' [expand_age_groups()]. If multiple standardized age groups match a given age,
#' the function will throw an error.
#'
#' @examples
#' tibble::tibble(
#'   id = 1:4,
#'   age_group = c("0 - 4", "10-14", "65-69", "85+")
#' ) %>%
#'   standardize_age_groups()
#' @inheritParams expand_age_groups
#' @param age_var The column containing age or age group. This column will be
#'   overwritten if named `age_group`. If the column is numeric or can be
#'   coerced to numeric, it is treated as actual age. Otherwise it is assumed to
#'   contain age groups.
#' @param std_age_groups Standard age groups, in the desired order.
#' @inheritDotParams expand_age_groups
#' @family age processors
#' @export
standardize_age_groups <- function(
  data = NULL,
  age_var = age_group,
  std_age_groups = fcds_const("age_group"),
  ...
) {
  age_var <- enquo(age_var)
  age_var_name <- quo_name(age_var)

  std_ages <-
    tibble(age_group = std_age_groups) %>%
    filter(age_group != "Unknown") %>%
    expand_age_groups() %>%
    mutate(
      age_low = if_else(is_neg_infinite(age_low), 0, age_low),
      age_high = if_else(is_pos_infinite(age_high), 125, age_high),
      ...age = seq2(age_low, age_high)
    ) %>%
    tidyr::unnest() %>%
    select(-age_low, -age_high)

  age_is_numeric <- is.numeric(data[[age_var_name]])
  if (!age_is_numeric) {
    if (!any(grepl("[^0-9.]", data[[age_var_name]]))) {
      # probably numeric because only digits and
      data[["...age"]] <- as.numeric(data[[age_var_name]])
      age_is_numeric <- TRUE
    }
  }

  if (age_is_numeric) {
    if (!"...age" %in% names(data)) data <- data %>% mutate(...age = !!age_var)
    data <- data %>%
      mutate(
        ...age = floor(...age),
        ...age := if_else(is_neg_infinite(...age), 0, ...age),
        ...age := if_else(is_pos_infinite(...age), 125, ...age)
      )

    # age_group will be replaced if it exists
    if ("age_group" %in% names(data)) data <- data %>% select(-age_group)

    data <- dplyr::left_join(data, std_ages, by = "...age") %>%
      select(-...age)
  } else {
    # break apart groups
    # match on low, match on high:
    #   - same answer? return first
    #   - different? error (non-overlapping groups)
    data <- data %>% expand_age_groups(!!age_var, ...,
                                       age_low_var = ...age_low,
                                       age_high_var = ...age_high)

    data_low <- data %>%
      select(age_group = ...age_low) %>%
      standardize_age_groups(std_age_groups = std_age_groups)
    data_high <- data %>%
      select(age_group = ...age_high) %>%
      standardize_age_groups(std_age_groups = std_age_groups)

    if (!identical(data_low$age_group, data_high$age_group)) {
      abort(
        glue("Age groupings in {age_var_name} ",
             "are not consistent with `std_age_groups`. ",
             "Try using actual age, if available.")
      )
    }

    if ("age_group" %in% names(data)) data <- data %>% select(-age_group)
    data <- data %>%
      mutate(age_group = paste(data_low$age_group)) %>%
      select(-...age_low, -...age_high)
  }

  data %>%
    tidyr::replace_na(list(age_group = "Unknown")) %>%
    mutate(age_group = factor(age_group, std_age_groups, ordered = TRUE))
}

format_age_groups <- function(
  data,
  age_low_var = age_low,
  age_high_var = age_high
) {
  age_low_var  <- enquo(age_low_var)
  age_high_var <- enquo(age_high_var)

  data %>%
    mutate(
      age_group_low = if_else(!!age_low_var < 0, "0", paste(!!age_low_var)),
      age_group_high = if_else(
        !!age_high_var > 0 & is.infinite(!!age_high_var),
        "+",
        paste(" -", !!age_high_var)),
      age_group = paste0(age_group_low, age_group_high),
      age_group = factor(age_group, fcds_const("age_group"), ordered = TRUE)
    ) %>%
    select(-age_group_low, -age_group_high)
}


# Age Adjustment ----------------------------------------------------------

age_adjust <- function(
  data,
  outcome_var = n,
  year_var = dx_year_mid,
  fl_pop = get_data("seer_pop_fl"),
  std_pop_data = get_data("seer_std_ages"),
  keep_age_group = FALSE
) {
  outcome_var <- enquo(outcome_var)
  outcome_var_name <- quo_name(outcome_var)
  year_var <- enquo(year_var)
  year_var_name <- quo_name(year_var)

  # Age and Year are required in source data
  stopifnot(year_var_name %in% names(data))
  stopifnot("age_group" %in% names(data))
  data <- filter(data, age_group != "Unknown")
  data_groups <- groups(data)

  #  Get groups from data
  #  Roll up FL and std pop data for those groups
  #  Merge into fcds
  #  Calculate age-adjusted rate
  data <- merge_fl_pop(data, !!year_var, fl_pop) %>%
    mutate(population = map(population, "population") %>% map_int(sum)) %>%
    summarize_at(vars(!!outcome_var, population), sum) %>%
    group_by(!!!data_groups) # restore groups

  data_groups <- setdiff(group_vars(data), year_var_name)
  data <- data %>%
    group_by(!!!rlang::syms(data_groups))

  if (keep_age_group) return(data)
  data %>% age_adjust_finalize(!!outcome_var, std_pop_data)
}


age_adjust_finalize <- function(
  data,
  outcome_var = n,
  std_pop_data = get_data("seer_std_ages")
) {
  outcome_var <- enquo(outcome_var)

  age_groups <- unique(data$age_group)

  # Get standard population
  std_pop_relevant <-
    std_pop_data %>%
    filter(age_group %in% age_groups) %>%
    select(age_group, std_pop) %>%
    mutate(w = std_pop / sum(std_pop))

  data <- left_join(data, std_pop_relevant, by = "age_group")

  # Get groups from the data other than "age_group" because we'll be
  # summing over the "age_group" column
  data_groups <- group_vars(data) %>% setdiff("age_group") %>% rlang::syms()

  data %>%
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
