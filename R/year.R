#' Complete Year Groups
#'
#' Completes year groups in the data frame with the expected year values,
#' see [fcds_const()].
#'
#' @examples
#' d_year <- tidyr::crossing(
#'   sex = "Female",
#'   race = fcds_const("race"),
#'   year = fcds_const("year")
#' )
#'
#' # These two versions are equivalent. The first version completes all variables
#' # included in the grouping and the second explicitly declares the variables
#' # that should be completed.
#'
#' d_year %>%
#'   dplyr::group_by(sex, race) %>%
#'   complete_year_groups() %>%
#'   dplyr::arrange(sex, race, year)
#'
#' d_year %>%
#'   complete_year_groups(sex, race) %>%
#'   dplyr::arrange(sex, race, year)
#'
#' @param data A data frame
#' @param year The column containing the `year_group`.
#' @param year_min Optional earliest year to include (inclusive)
#' @param year_max Optional latest year to include (inclusive)
#' @param fill Default values for rows in columns added to the data
#' @param ... Ignored if `data` is a grouped data frame. If not grouped,
#'   additional arguments are passed to [tidyr::complete()]. Use these arguments
#'   specify which columns are included in the expansion and how. See
#'   [tidyr::complete()] for more information.
#' @family year processors
#' @export
complete_year_groups <- function(
  data,
  ...,
  year_min = NULL,
  year_max = NULL,
  year = year,
  fill = list(n = 0)
) {
  year <- rlang::enquo(year)
  year_name <- rlang::quo_name(year)

  stopifnot(year_name %in% names(data))

  # Get known years data set, and subset to requested yrs to get expected values
  years <- tibble(year = fcds_const("year")) %>%
    separate_year_groups() %>%
    mutate_at(quos(year_min, year_max), as.integer)

  if (!is.null(year_min)) {
    year_min <- as.integer(year_min)
    years <- filter(years, year_min >= !!year_min)
  }
  if (!is.null(year_max)) {
    year_max <- as.integer(year_max)
    years <- filter(years, year_max <= !!year_max)
  }


  if (!is.null(groups(data))) {
    data_groups <- group_vars(data)
    data_groups <- setdiff(data_groups, year_name)

    data %>%
      ungroup() %>%
      with_ungroup(~ {
        complete(.x,
                 !!year_name := years$year,
                 tidyr::nesting(!!!rlang::syms(data_groups)),
                 fill = fill)
      }) %>%
      select(colnames(data))
  } else {
    data %>%
      complete(!!year_name := years$year, ...) %>%
      select(colnames(data))
  }
}

#' Add Mid-Year of Year Group
#'
#' Adds a new column containing the midpoint year of the range given in the
#' column indicated by the `year` argument, in a new column with "`_mid`"
#' appended to the `year` column name. The `year` column is assumed to contain
#' a _min_ year and a _max_ year, separated by `sep`. The midpoint is calculated
#' as `floor((year_max - year_min) / 2)` unless `offset` is explicitly provided.
#'
#' @examples
#' dplyr::tibble(year = fcds_const("year")) %>%
#'   add_mid_year_groups()
#'
#' @inheritParams complete_year_groups
#' @param sep Characters separating years in `year`. Whitespace on either side
#'   of `sep` will be automatically removed. Passed to [tidyr::separate()].
#' @param offset If supplied, the number of years to be added to the lower bound
#'   of the year group to calculate the mid-year value. By default uses
#'   `floor((year_max - year_min) / 2)`.
#' @family year processors
#' @export
add_mid_year_groups <- function(
  data,
  year = year,
  sep = "-",
  offset = NULL
) {
  year <- enquo(year)
  year_name <- paste0(quo_name(year), "_mid")

  if (year_name %in% names(data)) {
    warn(glue("`{year_name}` already exists in data"))
    return(data)
  }
  stopifnot(quo_name(year) %in% names(data))

  mutate(data, !!year_name := mid_year(!!year, sep = sep, offset = offset))
}

#' Separate Year Groups
#'
#' Separates a column containing year groups, such as "`1983-1985`", into
#' two separate columns with "`_min`" and "`_max`" appended to the column names.
#'
#' @examples
#' dplyr::tibble(year = fcds_const("year")) %>%
#'   separate_year_groups()
#'
#' @inheritParams add_mid_year_groups
#' @family year processors
#' @export
separate_year_groups <- function(
  data,
  year = year,
  sep = "-"
) {
  year <- rlang::enquo(year)
  year_name <- rlang::quo_name(year)

  # remove any whitespace around separator
  sep <- glue("\\s*{sep}\\s*")

  tidyr::separate(
    data,
    col = !!year,
    into = paste0(year_name, "_", c("min", "max")),
    remove = FALSE
  )
}

mid_year <- function(years, sep = "-", offset = NULL) {
  # rcmdcheck
  year_min <- NULL
  year_max <- NULL

  years <- separate_year_groups(tibble(year = years), sep = sep) %>%
    dplyr::mutate_at(quos(year_min, year_max), as.integer)

  offset <- offset %||% floor((years$year_max - years$year_min) / 2)

  # return mid year
  ifelse(
    is.na(years$year_max),
    NA_character_,
    paste(years$year_min + offset)
  )
}
