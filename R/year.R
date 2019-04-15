#' Complete Year Groups
#'
#' Completes year groups with expected years.
#'
#' @family year processors
#' @export
complete_year_groups <- function(.data, start = NULL, end = NULL, year_var = "dx_year", fill = list(n = 0)) {
  stopifnot(year_var %in% names(.data))
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

  data_groups <- groups(.data) %>% set_names(group_vars(.data))
  data_groups <- data_groups[setdiff(names(data_groups), year_var)]

  .data %>%
    ungroup() %>%
    complete(!!year_var := years$year, nesting(!!!data_groups), fill = fill) %>%
    group_by(!!!data_groups) %>%
    select(colnames(.data))
}

#' Add Mid-Year Column
#'
#' Adds a new column containing the midpoint year of the range given in the
#' column indicated by `year_var`.
#'
#' @family year processors
#' @export
add_mid_year <- function(.data, year_var = dx_year, sep = "-") {
  year_var <- rlang::enquo(year_var)
  year_var_name <- paste0(rlang::quo_name(year_var), "_mid")

  if (year_var_name %in% names(.data)) return(.data)
  stopifnot(rlang::quo_name(year_var) %in% names(.data))

  mutate(.data, !!year_var_name := mid_year(!!year_var))
}

mid_year <- function(years, sep = "-", offset = 2) {
  low_year_regex <- paste0("(\\d{2,4}).*", sep, ".*")
  paste(as.integer(sub(low_year_regex, "\\1", years)) + offset)
}
