# Declare tidy eval default arguments as global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "n", "age_group", "age_min", "age_max"
))

# Age Groups --------------------------------------------------------------

#' Separate Age Group Column into Min and Max Age of Range
#'
#' Takes the age group stored in `age_group` and creates two variables,
#' `age_min` and `age_max` with the age boundaries of the group. By default,
#' `separate_age_groups()` assumes that the age group definitions are separated
#' by a dash (`"-"`), possibly with whitespace on either side. The separator
#' can be specified using the `sep` argument.
#'
#' @examples
#' d_age_group <- dplyr::tibble(
#'   id = 1:4,
#'   age_group = c("0 - 4", "10 - 14", "65 - 69", "85+")
#' )
#'
#' separate_age_groups(d_age_group)
#'
#' @param data A data frame.
#' @param age_group Unquoted column name containing the age grouping.
#' @param ... Not used other than to require explicit naming of arguments.
#' @param age_min Unquoted column name for lower bondary of age group
#' @param age_max Unquoted column name for upper boundary of age group
#' @inheritParams tidyr::separate
#' @family age processors
#' @export
separate_age_groups <- function(
  data,
  age_group = age_group,
  sep = "\\s*-\\s*",
  ...,
  age_min = age_min,
  age_max = age_max
) {
  age_group    <- enquo(age_group)
  age_min      <- enquo(age_min)
  age_min_name <- quo_name(age_min)
  age_max      <- enquo(age_max)
  age_max_name <- quo_name(age_max)

  data %>%
    separate(!!age_group, into = c(age_min_name, age_max_name),
             sep = sep, remove = FALSE, fill = "right") %>%
    mutate(
      !!age_min_name := sub("+", "", !!age_min, fixed = TRUE),
      !!age_min_name := ifelse(
        (is.na(!!age_min) | !!age_min == "") & !is.na(!!age_max),
        "0", !!age_min)
    ) %>%
    mutate_at(c(age_min_name, age_max_name), as.numeric) %>%
    mutate(!!age_max_name := if_else(
      is.na(!!age_max) & !is.na(!!age_min), Inf, !!age_max))
}

#' Filter Data by Age Range
#'
#' Filters data to include persons with ages in the range between `age_gt` and
#' `age_lt`. If `age_group` has not been expanded into min and max ages of
#' the range, the input data is first passed to [separate_age_groups()]. If the
#' boundary age lies within a group, that group is _not included_ in the output.
#'
#' @examples
#' d_age_group <- dplyr::tibble(
#'   id = 1:4,
#'   age_group = c("0 - 4", "10 - 14", "65 - 69", "85+")
#' )
#'
#' d_age_group %>%
#'   filter_age_groups(age_gt = 0, age_lt = 15)
#'
#' d_age_group %>%
#'   filter_age_groups(age_gt = 65)
#'
#' # Notice that the "65 - 69" group is *not* included
#' d_age_group %>%
#'   filter_age_groups(age_lt = 66)
#'
#' @inheritParams separate_age_groups
#' @param age_gt Youngest age (inclusive).
#' @param age_lt Eldest age (inclusive).
#' @family age processors
#' @export
filter_age_groups <- function(
  data,
  age_gt = 0,
  age_lt = Inf,
  age_group = age_group
) {
  age_group <- enquo(age_group)
  age_group_name <- quo_name(age_group)

  data_original_cols <- colnames(data)

  if (!"age_min" %in% names(data)) {
    stopifnot(age_group_name %in% names(data))
    data <- separate_age_groups(data, age_var = !!age_group)
  }

  stopifnot("age_min" %in% names(data))
  stopifnot("age_max" %in% names(data))

  data %>%
    filter(age_min >= age_gt, age_max <= age_lt) %>%
    select(data_original_cols)
}

#' Complete Age Groups
#'
#' Completes age groups by adding missing age groups, either within the age
#' range from `age_gt` to `age_lt` or using the full age list from
#' [seer_std_ages]. If the columns `age_min` or `age_max` are missing from the
#' input data, [separate_age_groups()] is first called to expand the age group
#' variable.
#'
#' @examples
#' dplyr::tibble(
#'   age_group = c("10 - 14", "15 - 19", "25 - 29"),
#'   n = 10:12
#' ) %>%
#'   complete_age_groups(10, 35)
#'
#' @param include_unknown Should the "Unknown" age group be included?
#' @param std_age_groups Character vector containing expected (or standard) age
#'   groups.
#' @param ... Not used other than to require explicit naming of arguments.
#' @inheritParams separate_age_groups
#' @inheritParams filter_age_groups
#' @inheritParams tidyr::complete
#' @family age processors
#' @export
complete_age_groups <- function(
  data,
  age_gt = NULL,
  age_lt = NULL,
  ...,
  age_group = age_group,
  fill = list(n = 0),
  include_unknown = FALSE,
  std_age_groups = fcds_const("age_group")
) {
  age_group <- enquo(age_group)
  age_group_name <- quo_name(age_group)
  stopifnot(age_group_name %in% names(data))

  ages <- tibble(age_group = std_age_groups)
  if (!include_unknown) ages <- filter(ages, age_group != "Unknown")
  ages <- suppressWarnings(separate_age_groups(ages))

  if (!is.null(age_gt)) ages <- filter(ages, age_min >= !!age_gt)
  if (!is.null(age_lt)) ages <- filter(ages, age_max <= !!age_lt)

  common_age_groups <- union(ages$age_group, paste(data[[age_group_name]]))

  add_age_group <- age_group_name %in% group_vars(data)

  original_columns <- colnames(data)

  quiet_complete <- quietly(complete)

  data <- data %>%
    group_drop(age_group_name) %>%
    refactor_single_level_groups() %>%
    mutate(
      !!age_group_name := paste(!!age_group),
      !!age_group_name := factor(!!age_group, common_age_groups, ordered = TRUE)
    ) %>%
    quiet_complete(!!age_group, fill = fill)

  if (add_age_group) {
    data <- group_by(data, !!age_group, add = TRUE)
  }

  data[, original_columns]
}

#' Standardize Age Groups
#'
#' Standardizes age groups to match [seer_std_ages]. Adds an `age_group` column,
#' or overwites the existing `age_group` column if present. The `age_var` column
#' can be either an actual age or an age group. If the column is numeric, it
#' assumed to be an actual age, otherwise if it contains any non-numeric
#' characters it is assumed to be an age group that will be expanded using
#' [separate_age_groups()]. If multiple standardized age groups match a given age,
#' the function will throw an error.
#'
#' @examples
#' dplyr::tibble(
#'   id = 1:4,
#'   age_group = c("0 - 4", "10-14", "65-69", "85+")
#' ) %>%
#'   standardize_age_groups()
#' @param age_group The column containing age or age group. This column will be
#'   overwritten if named `age_group`. If the column is numeric or can be
#'   coerced to numeric, it is treated as actual age. Otherwise it is assumed to
#'   contain age groups.
#' @param std_age_groups Standard age groups, in the desired order.
#' @inheritDotParams separate_age_groups sep
#' @inheritParams filter_age_groups
#' @family age processors
#' @export
standardize_age_groups <- function(
  data = NULL,
  age_group = age_group,
  std_age_groups = fcds_const("age_group"),
  ...
) {
  age_group <- enquo(age_group)
  age_group_name <- quo_name(age_group)

  data_cols <- names(data)
  data_groups <- group_vars(data) %>% rlang::syms()
  data <- ungroup(data)

  std_ages <-
    tibble(age_group = std_age_groups) %>%
    filter(age_group != "Unknown") %>%
    separate_age_groups() %>%
    mutate(
      age_min = if_else(is_neg_infinite(age_min), 0, age_min),
      age_max = if_else(is_pos_infinite(age_max), 125, age_max),
      ...age = seq2(age_min, age_max)
    ) %>%
    tidyr::unnest() %>%
    select(-age_min, -age_max)

  age_is_numeric <- is.numeric(data[[age_group_name]])
  if (!age_is_numeric) {
    if (!any(grepl("[^0-9.]", data[[age_group_name]]))) {
      # probably numeric because only contains digits and dots
      data[["...age"]] <- as.numeric(data[[age_group_name]])
      age_is_numeric <- TRUE
    }
  }

  if (!age_is_numeric) {
    data_age_values <- unique(data[[age_group_name]])
    not_in_fcds_const <- setdiff(data_age_values, fcds_const("age_group"))
    age_is_same_as_fcds <- length(not_in_fcds_const) == 0
  }

  if (age_is_numeric) {
    if (!"...age" %in% names(data)) data <- data %>% mutate(...age = !!age_group)
    data <- data %>%
      mutate(
        ...age = floor(.data$...age),
        ...age = if_else(is_neg_infinite(.data$...age), 0, .data$...age),
        ...age = if_else(is_pos_infinite(.data$...age), 125, .data$...age)
      )

    # age_group will be replaced if it exists
    if ("age_group" %in% names(data)) data <- data %>% select(-"age_group")

    data <- quiet_left_join(data, std_ages, by = "...age") %>%
      select(-"...age")
  } else if (age_is_same_as_fcds) {
    data <- mutate(
      data,
      !!age_group_name := factor(!!age_group, fcds_const("age_group"), ordered = TRUE)
    )
  } else {
    # break apart groups
    # match on min, match on max:
    #   - same answer? return first
    #   - different? error (non-overlapping groups)
    data <- data %>% separate_age_groups(!!age_group, ...,
                                       age_min  = .data$...age_min,
                                       age_max = .data$...age_max)

    data_age_min <- data %>%
      select(age_group = .data$...age_min) %>%
      standardize_age_groups(std_age_groups = std_age_groups)
    data_age_max <- data %>%
      select(age_group = .data$...age_max) %>%
      standardize_age_groups(std_age_groups = std_age_groups)

    if (!identical(data_age_min$age_group, data_age_max$age_group)) {
      abort(
        glue("Age groupings in {age_group_name} ",
             "are not consistent with `std_age_groups`. ",
             "Try using actual age, if available.")
      )
    }

    if ("age_group" %in% names(data)) data <- data %>% select(-"age_group")
    data <- data %>%
      mutate(age_group = paste(data_age_min$age_group)) %>%
      select(-"...age_min", -"...age_max")
  }

  data %>%
    select(union(data_cols, "age_group")) %>%
    tidyr::replace_na(list(age_group = "Unknown")) %>%
    mutate(age_group = factor(.data$age_group, std_age_groups, ordered = TRUE)) %>%
    group_by(!!!data_groups)
}

# nocov start
format_age_groups <- function(
  data,
  age_min_var = age_min,
  age_max_var = age_max
) {
  age_min_var  <- enquo(age_min_var)
  age_max_var <- enquo(age_max_var)

  data %>%
    mutate(
      age_group_min = if_else(!!age_min_var < 0, "0", paste(!!age_min_var)),
      age_group_max = if_else(
        !!age_max_var > 0 & is.infinite(!!age_max_var),
        "+",
        paste(" -", !!age_max_var)),
      age_group = paste0(.data$age_group_min, .data$age_group_max),
      age_group = factor(.data$age_group, fcds_const("age_group"), ordered = TRUE)
    ) %>%
    select(-"age_group_min", -"age_group_max")
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
#'   Calculating age-adjusted rates requires three primary inputs:
#'
#'   1. Raw age-specific count of event or outcome, possibly observed or
#'      summarized at repeated, consistent time intervals.
#'
#'   2. Population data with the same demographic resolution as the age-specific
#'      counts, as in, for example, the population for the same geographic
#'      region, sex, race, year, and age.
#'
#'   3. The standard reference population that is used to weight incidence among
#'      the observed age-specific count.
#'
#'   Each input is required to contain matching age information. The default
#'   data supplied with the package for `population` ([seer_pop_fl]) and
#'   `population_standard` ([seer_std_ages]) use the column name `age_group`.
#'   You can specify the name of the column containing age information with the
#'   `age` argument. If the column name in `data` is not present in the
#'   population data, `age_adjust()` will fall back to `age_group` for those
#'   data sets.
#'
#'   As described in the
#'   [SEER*Stat Tutorial: Calculating Age-adjusted Rates](https://seer.cancer.gov/seerstat/tutorials/aarates/definition.html):
#'   _The age-adjusted rate for an age group comprised of the ages x through y
#'   is calculated using the following formula_:
#'
#'   \if{html}{\figure{seer_ar-aarate.png}{ }}
#'   \if{latex}{\figure{seer_ar-aarate.png}{options: width=0.5in}}
#'
#' @return A data frame with age-adjusted incidence rates in the column `rate`.
#'   Note that the `age` column will no longer be included in the output because
#'   the age-adjusted rate summarizes the observed incidence across all ages.
#'
#'   If `keep_age` is `TRUE`, the age column is retained, but the final rate is
#'   not calculated, adding the columns `population`, `std_pop`, and `w` for the
#'   specific population, standard population and standardizing population
#'   weight, respectively.
#'
#' @references
#'   <https://seer.cancer.gov/seerstat/tutorials/aarates/definition.html>
#'
#' @examples
#'
#' # This example is drawn from the SEER*Stat Age-adjusted Rate Tutorial:
#' # https://seer.cancer.gov/seerstat/tutorials/aarates/definition.html
#' d_incidence <- dplyr::tribble(
#'   ~age_group,   ~n,
#'      "0 - 4",  116,
#'      "5 - 9",   67,
#'    "10 - 14",   71,
#'    "15 - 19",   87,
#'    "20 - 24",  177,
#'    "25 - 29",  290,
#'    "30 - 34",  657,
#'    "35 - 39", 1072,
#'    "40 - 44", 1691,
#'    "45 - 49", 2428,
#'    "50 - 54", 2931,
#'    "55 - 59", 2881,
#'    "60 - 64", 2817,
#'    "65 - 69", 2817,
#'    "70 - 74", 2744,
#'    "75 - 79", 2634,
#'    "80 - 84", 1884,
#'        "85+", 1705
#' ) %>%
#'   dplyr::mutate(year = 2013) %>%
#'   standardize_age_groups()
#'
#' d_population <- dplyr::tribble(
#'   ~age_group, ~population,
#'      "0 - 4",      693068,
#'      "5 - 9",      736212,
#'    "10 - 14",      770999,
#'    "15 - 19",      651390,
#'    "20 - 24",      639159,
#'    "25 - 29",      676354,
#'    "30 - 34",      736557,
#'    "35 - 39",      724826,
#'    "40 - 44",      700200,
#'    "45 - 49",      617437,
#'    "50 - 54",      516541,
#'    "55 - 59",      361170,
#'    "60 - 64",      259440,
#'    "65 - 69",      206204,
#'    "70 - 74",      172087,
#'    "75 - 79",      142958,
#'    "80 - 84",       99654,
#'        "85+",       92692,
#' ) %>%
#'   dplyr::mutate(year = 2013) %>%
#'   standardize_age_groups()
#'
#' # Because the example data do not include the year of observation, we set
#' # by_year = NULL so that age_adjust() does not attempt to join
#' # d_incidence with d_population by a year column.
#'
#' age_adjust(d_incidence, population = d_population, by_year = NULL)
#'
#' age_adjust(d_incidence,
#'            population = d_population,
#'            by_year = NULL,
#'            keep_age = TRUE)
#'
#' @param data A data frame, containing counts
#' @param count The unquoted column name containing raw event or outcome counts.
#' @param year The unquoted column name containing the year that will be matched
#'   to the population data.
#' @param population Population data specific to the population described by
#'   `data`. By default, uses the county-specific Florida population data
#'   provided by SEER (see [seer_pop_fl_1990]). If `data` describes years
#'   prior to 1990, then [seer_pop_fl] is used instead.
#'
#'   Note that `origin` (Spanish/Hispanic Origin) is not avaiable for years
#'   prior to 1990 and an error will occur if `origin` is included in the input
#'   `data` but the data appear to cover years before 1990. You can override
#'   this error by explicitly setting `population = fcds::seer_pop_fl_1990`.
#' @param population_standard The standard age-specific population used to
#'   calculate the age-adjusted rates. By default, uses the 2000 U.S. standard
#'   population provided by SEER (see [seer_std_ages]).
#' @param keep_age Age-adjustment by definition summarizes event or outcome
#'   counts (incidence) over _all_ included ages. Set `keep_age = TRUE` to join
#'   the source data with all age-specific population without completing the age
#'   adjustment calculation. This option is primarily provided for debugging
#'   purposes.
#' @param by_year The column or columns by which `data` and `population` should
#'   be joined, by default `"year"`. The syntax follows from the `by` argument
#'   of [dplyr::left_join()], where the name of each entry is the column name in
#'   `data` and the value is the column name in `population`. If both are the
#'   same, a single value is sufficient.
#' @param age The unquoted column name containing the age or age group. The
#'   default expects that the column `age_group` exists in `data`, `population`,
#'   and `population_standard`. If the `age` column used in `data` does not
#'   exist in the population data sets, `age_adjust()` will fall back to use the
#'   columns `age_group` from the population data but the custom column from
#'   `data`.
#' @export
age_adjust <- function(
  data,
  count = n,
  population = NULL,
  population_standard = fcds::seer_std_ages,
  by_year = "year",
  age = age_group,
  keep_age = FALSE
) {
  count <- enquo(count)
  age <- enquo(age)
  age_name <- quo_name(age)

  if (!quo_name(count) %in% names(data)) {
    abort(glue("`data` does not contain `count` column '{quo_name(count)}'"))
  }

  population <- population %||% choose_seer_population(data)

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

  # store original groups for last step
  data_groups <- group_vars(data) %>%
    setdiff(c(if (!keep_age) age_name)) %>%
    rlang::syms()

  if (length(data_groups)) {
    validate_same_number_of_age_groups(data)
  }

  data <- filter(data, !!age != "Unknown") %>%
    # data needs age to be in the groups
    group_by(!!age, add = TRUE)

  data <- data %>%
    with_ungroup(~ {
      join_population(.x, population, by_year = by_year) %>%
        mutate(
          population = purrr::map(population, "population") %>%
            purrr::map_dbl(sum)
        )
    }) %>%
    with_retain_groups(~ dplyr::summarize_at(., quos(!!count, population), sum))

  age_adjust_finalize(
    data, !!count,
    population_standard = population_standard,
    age = !!age,
    keep_age = keep_age
  ) %>%
    group_by(!!!data_groups)
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
    select(!!age, "std_pop") %>%
    mutate(w = .data$std_pop / sum(.data$std_pop))

  data <- quiet_left_join(data, std_pop_relevant, by = age_name)

  if (keep_age) return(data)

  # Get groups from the data other than "age_group" because we'll be
  # summing over the "age_group" column
  data_groups <- group_vars(data) %>% setdiff("age_group") %>% rlang::syms()

  data %>%
    # Drop "age_group" from groups so that summarization is over ages
    group_drop(!!age) %>%
    with_ungroup(~ mutate(., rate = !!count / .data$population * .data$w)) %>%
    with_retain_groups(~ dplyr::summarize_at(., quos(!!count, population, rate), sum)) %>%
    mutate(rate = .data$rate * 100000)
}

validate_same_number_of_age_groups <- function(data) {
  # validate that all groups have same number of age groups
  d_age_groups <- data[, union("age_group", group_vars(data))] %>%
    group_drop(age_group) %>%
    dplyr::count(sort = TRUE) %>%
    ungroup() %>%
    tidyr::nest(-n)

  if (nrow(d_age_groups) == 0) {
    abort("No age groups found in data")
  }

  if (nrow(d_age_groups) > 1) {
    d_age_groups$n_groups <- purrr::map_int(d_age_groups$data, nrow)
    max_n <- d_age_groups$n[[1]]
    max_group_labels <- d_age_groups$data[[1]] %>%
      apply(1, collapse) %>%
      and_more()

    not_max_group_labels <- d_age_groups[-1, ]$data %>%
      dplyr::bind_rows() %>%
      apply(1, collapse) %>%
      and_more()

    warn(glue(
      "Mismatched number of age_groups in data, you may need to use ",
      "complete_age_groups() first.\n{max_group_labels} ",
      "{ifelse(d_age_groups$n_groups[1] > 1, 'have', 'has')} ",
      "{max_n} age groups, but ",
      "{not_max_group_labels} ",
      "{ifelse(sum(d_age_groups$n_groups[-1]) > 1, 'have', 'has')} ",
      "fewer age groups."
    ))

    return(invisible(FALSE))
  }

  invisible(TRUE)
}

choose_seer_population <- function(data) {
  guess_year_var <- c("year", "year_group")
  guess_year_var <- guess_year_var[guess_year_var %in% names(data)]
  if (!length(guess_year_var)) {
    abort("Please manually specify the reference `population`.")
  }

  years <- data[[guess_year_var[1]]] %>% unique() %>% paste()
  years <- grep("\\d{4}", years, value = TRUE)
  years <- sub("(\\d{4}).+", "\\1", years)

  if (min(as.integer(years), na.rm = TRUE) < 1990) {
    if ("origin" %in% names(data)) {
      abort(glue(
        "`origin` is included in input data, but SEER population data prior to ",
        "1990 does not contain Spanish/Hispanic origin. You may set ",
        "`population = fcds::seer_pop_fl_1990` manually to override this check."
      ))
    }
    return(fcds::seer_pop_fl)
  }
  fcds::seer_pop_fl_1990
}

refactor_single_level_groups <- function(data) {
  factor_cols <- names(data)[purrr::map_lgl(data, is.factor)]

  for (f_col in factor_cols) {
    n_fct_levels_present <- data[[f_col]] %>% unique() %>% length()
    if (n_fct_levels_present == 1) {
      data[[f_col]] <- factor(data[[f_col]], paste(data[[f_col]][1]))
    }
  }

  data
}
