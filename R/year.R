#' Complete Year Groups
#'
#' Completes year groups with expected years.
#'
#' @family year processors
#' @export
complete_year_groups <- function(data, start = NULL, end = NULL, year_var = "dx_year", fill = list(n = 0)) {
  stopifnot(year_var %in% names(data))
  years <- tibble(year = fcds_const("year")) %>%
    separate(year, c("start", "end"), sep = "\\s*-\\s*", remove = FALSE) %>%
    mutate_at(vars(start, end), as.integer)

  if (!is.null(start)) {
    start <- as.integer(start)
    years <- filter(years, start >= !!start)
  }
  if (!is.null(end)) {
    end <- as.integer(end)
    years <- filter(years, end >= !!end)
  }

  data_groups <- groups(data) %>% set_names(group_vars(data))
  data_groups <- data_groups[setdiff(names(data_groups), year_var)]

  data %>%
    ungroup() %>%
    complete(!!year_var := years$year, nesting(!!!data_groups), fill = fill) %>%
    group_by(!!!data_groups) %>%
    select(colnames(data))
}

#' Add Mid-Year of Year Group
#'
#' Adds a new column containing the midpoint year of the range given in the
#' column indicated by the `year` argument, in a new column with "`_mid`"
#' appended to the `year` column name. The `year` column is assumed to contain
#' a _min_ year and a _max_ year, separated by `sep`. The midpoint is calculated
#' as `floor((year_max - year_min) / 2)` unless `offset` is explicitly provided.
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

  if (year_name %in% names(data)) return(data)
  stopifnot(quo_name(year) %in% names(data))

  mutate(data, !!year_name := mid_year(!!year, sep = sep, offset = offset))
}

#' Separate Year Groups
#'
#' Separates a column containing year groups, such as "`1983-1985`", into
#' two separate columns with "`_min`" and "`_max`" appended to the column names.
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
