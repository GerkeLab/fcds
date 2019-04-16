# ---- Check for existing packages ----
check_package <- function(
  pkg,
  needed_by = NULL,
  how_to_install = paste0("install.packages('", pkg, "')"),
  warn = TRUE
) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (is.null(fun)) fun <- "{fcds}"
    msg <- glue(
      "Package `{pkg}` is {if (warn) 'suggested' else 'required'} by {needed_by}.",
      "\n{how_to_install}"
    )
    if (warn) warn(msg) else abort(msg)
    invisible(FALSE)
  } else invisible(TRUE)
}

requires_package <- function(pkg, needed_by = NULL, ...) {
  for (package in pkg) {
    check_package(package, needed_by, ..., warn = FALSE)
  }
}
suggests_package <- function(pkg, needed_by = NULL, ...) {
  for (package in pkg) {
    check_package(package, needed_by, ..., warn = TRUE)
  }
}

common_names <- function(x, y) intersect(names(x), names(y))

`%||%` <- function(x, y) if (is.null(x)) y else x

is_neg_infinite <- function(x) {
  vapply(x, function(y) {
    y < 0 & is.infinite(y)
  }, logical(1))
}

is_pos_infinite <- function(x) {
  vapply(x, function(y) {
    y > 0 & is.infinite(y)
  }, logical(1))
}

seq2 <- function(x, y) {
  purrr::map2(x, y, ~ {
    if (any(is.na(c(.x, .y)))) return(NA_integer_)
    seq.int(floor(.x), floor(.y))
  })
}

# Group Utilities ---------------------------------------------------------

#' Remove a Grouping Column from Groups
#'
#' Removes columns from the current list of groups, with the additional option
#' to remove the columns from the data completely. In essence, the opposite of
#' [dplyr::group_by()] with `add = TRUE`.
#'
#' @examples
#' Remove "type" from the groups
#' tidyr::table2 %>%
#'   dplyr::group_by(country, year, type) %>%
#'   group_drop(type)
#'
#' # Remove "type" from the groups and the output data frame
#' tidyr::table2 %>%
#'   dplyr::group_by(country, year, type) %>%
#'   group_drop(type, .remove_dropped = TRUE)
#'
#' # Only columns that were dropped from groups will be removed
#' tidyr::table2 %>%
#'   dplyr::group_by(country, type) %>%
#'   group_drop(year, type, .remove_dropped = TRUE)
#'
#' # Nothing happens if trying to drop a group that's not in the groups
#' tidyr::table2 %>%
#'   dplyr::group_by(country, year) %>%
#'   group_drop(type)
#'
#' @param .data A grouped tbl, tibble, or data.frame
#' @param ... Quoted or unquoted column names to be removed from the grouping
#' @param .remove_dropped Should columns that are dropped from the grouping also
#'   be removed from `.data`? Default is `FALSE`.
#' @family Group Utilities
#' @export
group_drop <- function(.data, ..., .remove_dropped = FALSE) {
  if (!inherits(.data, "grouped_df")) return(.data)

  group_var_names <- tidyselect::vars_select(names(.data), ...) %>%
    intersect(group_vars(.data))

  if (!length(group_var_names)) {
    # requested columns aren't in .data's groups
    return(.data)
  }

  data_groups <- group_vars(.data) %>%
    setdiff(group_var_names) %>%
    rlang::syms()

  .data <- .data %>% group_by(!!!data_groups)

  if (.remove_dropped) {
    select(.data, -group_var_names)
  } else {
    .data
  }
}

}
