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

#' Add Mid-Year Column
#'
#' Adds a new column containing the midpoint year of the range given in the
#' column indicated by `year_var`.
#'
#' @family year processors
#' @export
add_mid_year <- function(data, year_var = dx_year, sep = "-") {
  year_var <- enquo(year_var)
  year_var_name <- glue("{quo_name(year_var)}_mid")

  if (year_var_name %in% names(data)) return(data)
  stopifnot(quo_name(year_var) %in% names(data))

  mutate(data, !!year_var_name := mid_year(!!year_var))
}


mid_year <- function(years, sep = "-", offset = NULL) {
  regex_years <- glue("(\\d{{2,4}})(\\s*{sep}\\s*(\\d{{2,4}}))?")

  years <- str_match_(years, regex_years) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("g")), as.integer)

  if (all(is.na(years$g3))) {
    abort("Unable to extract two year values for all provided years")
  }
  if (any(is.na(years$g1) | is.na(years$g3))) {
    n_not_found <- sum(is.na(years$g1) | is.na(years$g3))
    warn(glue("Two year values were not found for {n_not_found} year(s), ",
              "NA values were returned for the midpoint of these year(s)."))
  }

  offset <- offset %||% floor((years$g3 - years$g1) / 2)

  # return mid year
  ifelse(
    is.na(years$g3),
    NA_character_,
    paste(years$g1 + offset)
  )
}

str_match_ <- function(text, pattern, ..., perl = TRUE) {
  r_matches <- regexec(pattern, text, ..., perl = TRUE)
  ret <- regmatches(text, r_matches)
  if (length(ret)) {
    ret <- purrr::map_dfr(ret, ~ {
      .x <- .x %>% as.matrix() %>% t() %>% as.data.frame(stringsAsFactors = FALSE)
      names(.x) <- c("text", if (ncol(.x) > 1) paste0("g", 1:(ncol(.x) - 1L)))
      .x
    })
  }
  ret
}
