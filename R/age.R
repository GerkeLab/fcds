
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
  data_cols <- names(data)

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
    select(union(data_cols, "age_group")) %>%
    tidyr::replace_na(list(age_group = "Unknown")) %>%
    mutate(age_group = factor(age_group, std_age_groups, ordered = TRUE))
}

# nocov start
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
# nocov end


# Age Adjustment ----------------------------------------------------------

#' Age-Adjust Rates
#'
#' Provides age-adjusted incident rates with respect to a reference population,
#' by default [seer_std_ages].
#'
#' @section Age-Adjusted Rates:
#'
#' Calculating age-adjusted rates requires three primary inputs:
#'
#' 1. Raw age-specific count of event or outcome, possibly observed or
#'    summarized at repeated, consistent time intervals.
#' 2. Population data with the same demographic resolution as the age-specific
#'    counts, as in, for example, the population for the same geographic region,
#'    sex, race, year, and age.
#' 3. The standard reference population that is used to weight incidence among
#'    the observed age-specific count.
#'
#' Note that each input is required to have a column `age_group` containing
#' matching age groups across all three inputs.
#'
#' @return A data frame with age-adjusted incidence rates. Note that `age_group`
#'   will no longer be included in the output because the age-adjusted rate
#'   summarizes the observed incidence across all ages.
#'
#' @param data A data frame, containing counts
#' @param count The unquoted column name containing raw event or outcome counts.
#' @param year The unquoted column name containing the year that will be matched
#'   to the population data.
#' @param population Population data specific to the population described by
#'   `data`. By default, uses the county-specific Florida population data
#'   provided by SEER (see [seer_pop_fl]).
#' @param population_standard The standard age-specific population used to
#'   calculate the age-adjusted rates. By default, uses the 2000 U.S. standard
#'   population provided by SEER (see [seer_std_ages]).
#' @param keep_age Age-adjustment by definition summarizes event or outcome
#'   counts (incidence) over _all_ included ages. Set `keep_age` to join the
#'   source data with all age-specific population without completing the age
#'   adjustment calculation. This option is primarily provided for debugging
#'   purposes.
#' @export
age_adjust <- function(
  data,
  count = n,
  population = fcds::seer_pop_fl,
  population_standard = fcds::seer_std_ages,
  by_year = c("dx_year_mid" = "year"),
  age = age_group,
  keep_age = FALSE
) {
  count <- enquo(count)
  age <- enquo(age)
  age_name <- quo_name(age)

  # All inputs need to have the age_grouping variable
  # but allow matching to age_group in population defaults
  have_age <- validate_all_have_var(
    age_name, .abort = FALSE,
    data = data,
    population = population,
    population_standard = population_standard
  )
  if (!all(have_age)) {
    if (age_name == "age_group") {
      abort(attr(have_age, "msg_missing"))
    }

    # Stop if age_group not in both population data frames
    # (but use original error message)
    pop_have_age_group <- validate_all_have_var(
      "age_group", .abort = FALSE,
      population = population,
      population_standard = population_standard
    )
    if (!all(pop_have_age_group)) {
      abort(attr(have_age, "msg_missing"))
    }

    # rename age_group in population data
    population <- population %>%
      dplyr::rename(!!age_name := age_group)

    population_standard <- population_standard %>%
      dplyr::rename(!!age_name := age_group)
  }

  # `population` needs a "population" column
  validate_all_have_var("population", population = population)

  if (is.null(groups(data))) {
    # warn if no groups provided but non count/age columns have multiple values
    data_meta <-
      data %>%
      select(-!!count, -!!age) %>%
      dplyr::distinct()

    if (ncol(data_meta) && nrow(data_meta) > 1) {
      warn(glue(
        "Incidence data appears to include {nrow(data_meta)} groups, ",
        "but no groups were provided. Did you forget to declare groups ",
        "with `group_by()`?"
      ))
    }
  }

  data <- filter(data, !!age != "Unknown") %>%
    # data needs age to be in the groups
    group_by(!!age, add = TRUE)
  data_groups <- groups(data)

  data <- data %>%
    join_population(population, by_year = by_year) %>%
    with_ungroup(~ {
      mutate(
        .x, population = purrr::map(population, "population") %>% purrr::map_dbl(sum)
      )
    }) %>%
    with_retain_groups(~ dplyr::summarize_at(., quos(!!count, population), sum))

  # Should keep_age_group exit here? Or should we add std pop data first?

  # data_groups <- setdiff(dplyr::group_vars(data), year_var_name)
  # data <- data %>% group_by(!!!rlang::syms(data_groups))

  age_adjust_finalize(
    data, !!count,
    population_standard = population_standard,
    age = !!age,
    keep_age = keep_age
  )
}


age_adjust_finalize <- function(
  data,
  count = n,
  population_standard = fcds::seer_std_ages,
  age = age_group,
  keep_age = FALSE
) {
  count <- enquo(count)
  age <- rlang::enquo(age)
  age_name <- rlang::quo_name(age)

  if (!age_name %in% names(population_standard)) {
    abort(glue(
      "age_adjust() requires `population_standard` to have the column '{age_name}'."
    ))
  }

  age_groups <- data %>% dplyr::pull(!!age) %>% unique()
  pop_std_age_groups <- population_standard %>% dplyr::pull(!!age) %>% unique()
  if (length(intersect(age_groups, pop_std_age_groups)) == 0) {
    abort("The age groups in `data` do not match any age groups in `population_standard`.")
  }
  age_groups_missing <- setdiff(age_groups, pop_std_age_groups)
  if (length(age_groups_missing) > 0) {
    abort(glue(
      "Not all age groups in `data` have corresponding age groups in `population_standard`. ",
      "Missing age groups: {and_more(age_groups_missing)}"
    ))
  }

  # Get standard population
  std_pop_relevant <-
    population_standard %>%
    filter(!!age %in% age_groups) %>%
    select(!!age, std_pop) %>%
    mutate(w = std_pop / sum(std_pop))

  data <- dplyr::left_join(data, std_pop_relevant, by = age_name)

  if (keep_age) return(data)

  # Get groups from the data other than "age_group" because we'll be
  # summing over the "age_group" column
  data_groups <- group_vars(data) %>% setdiff("age_group") %>% rlang::syms()

  data %>%
    # Drop "age_group" from groups so that summarization is over ages
    group_drop(!!age) %>%
    with_ungroup(~ mutate(., rate = !!count / population * w)) %>%
    with_retain_groups(~ dplyr::summarize_at(., quos(!!count, population, rate), sum)) %>%
    mutate(rate = rate * 100000)
}
