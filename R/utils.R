fcds_file <- function(...) {
  system.file(..., package = "fcds")
}

cat_line <- function(..., sep = "", symbol = "", indent = 0) {
  symbol <- switch(
    symbol,
    "bullet" = "\u25cf ",
    "tick" = "\u2714 ",
    "dots" = "\u2026 ",
    ""
  )

  cat(strrep(" ", indent), symbol, sep = "")
  cat(..., sep = sep)
  cat("\n")
}

cat_bullet <- function(...) cat_line(..., symbol = "bullet")
cat_tick   <- function(...) cat_line(..., symbol = "tick")
cat_dots   <- function(...) cat_line(..., symbol = "dots") #nocov


quietly <- function(.f) {
  function(...) suppressWarnings(.f(...))
}

fct_remove_order <- function(x) {
  if (!is_fct_ordered(x)) return(x)
  class(x) <- "factor"
  x
}

is_fct_ordered <- function(x) inherits(x, "ordered")

fct_remove_order_all <- function(x) {
  dplyr::mutate_if(x, is_fct_ordered, fct_remove_order)
}

quiet_left_join  <- quietly(dplyr::left_join)
quiet_semi_join  <- quietly(dplyr::semi_join)
quiet_anti_join  <- quietly(dplyr::anti_join)
quiet_inner_join <- quietly(dplyr::inner_join)
quiet_complete   <- quietly(tidyr::complete)

# nocov start
fcds_version <- function(drop_dev = TRUE) {
  v <- utils::packageVersion("fcds")
  if (drop_dev) sub("\\.9\\d\\d\\d$", "", v) else v
}
# nocov end

# ---- Check for existing packages ----
check_package <- function(
  pkg,
  needed_by = NULL,
  how_to_install = glue("install.packages({double_quote(pkg)})"),
  warn = TRUE
) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (is.null(needed_by)) needed_by <- "{fcds}"
    msg <- glue(
      "Package `{pkg}` is {if (warn) 'suggested' else 'required'} by {needed_by}.",
      "\n{how_to_install}"
    )
    if (warn) warn(msg) else abort(msg)
    invisible(FALSE)
  } else invisible(TRUE)
}

requires_package <- function(pkg, needed_by = NULL, ...) {
  res <- set_names(rep(FALSE, length(pkg)), pkg)
  for (package in pkg) {
    res[package] <- check_package(package, needed_by, ..., warn = FALSE)
  }
  res
}
suggests_package <- function(pkg, needed_by = NULL, ...) {
  res <- set_names(rep(FALSE, length(pkg)), pkg)
  for (package in pkg) {
    res[package] <- check_package(package, needed_by, ..., warn = TRUE)
  }
  res
}

common_names <- function(x, y) intersect(names(x), names(y))

`%||%` <- function(x, y) if (is.null(x)) y else x  # nocov

is_neg_infinite <- function(x) {
  vapply(x, function(y) {
    y < 0 && is.infinite(y)
  }, logical(1))
}

is_pos_infinite <- function(x) {
  vapply(x, function(y) {
    y > 0 && is.infinite(y)
  }, logical(1))
}

seq2 <- function(x, y) {
  purrr::map2(x, y, ~ {
    if (any(is.na(c(.x, .y)))) return(NA_integer_)
    seq.int(floor(.x), floor(.y))
  })
}

validate_all_have_var <- function(var, ..., .abort = TRUE) {
  datasets <- purrr::map(
    list(...),
    ~ ifelse(var %in% names(.), "has", "missing")
  )

  msg <- if (purrr::some(datasets, ~ . == "missing")) {
    missing <- purrr::keep(datasets, ~ . == "missing") %>%
      names() %>% glue::glue_collapse("`, `", last = "` and `")

    msg <- glue("'{var}' is missing from `{missing}`")
    if (.abort) abort(msg)
    msg
  }
  x <- purrr::map_lgl(datasets, ~ . == "has")
  attributes(x)$msg_missing <- msg
  x
}

and_more <- function(x, wrap = "'", last = ", and ") {
  n <- length(x)
  if (n <= 3) {
    glue::glue_collapse(glue("{wrap}{x}{wrap}"), ", ", last = last)
  } else {
    x <- x[1:2]
    x_2 <- glue::glue_collapse(glue("{wrap}{x}{wrap}"), ", ")
    glue("{x_2}, and {n - 2L} more...")
  }
}

to_snake_case <- function(x) {
  x <- tolower(x)
  x <- gsub("[^[:alnum:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  gsub(" ", "_", x)
}

collapse <- function(x, sep = ", ", last = "") {
  glue::glue_collapse(x, sep = sep, last = last)
}

backtick <- function(x, sep = ", ", last = "") {
  x <- glue::backtick(x)
  if (sep != "") collapse(x, sep, last) else x
}

single_quote <- function(x, sep = ", ", last = "") {
  x <- glue::single_quote(x)
  if (sep != "") collapse(x, sep, last) else x
}

double_quote <- function(x, sep = ", ", last = "") {
  x <- glue::double_quote(x)
  if (sep != "") collapse(x, sep, last) else x
}

# Group Utilities ---------------------------------------------------------

#' Remove a Grouping Column from Groups
#'
#' Removes columns from the current list of groups, with the additional option
#' to remove the columns from the data completely. In essence, the opposite of
#' [dplyr::group_by()] with `.add = TRUE`.
#'
#' @examples
#' # Remove "type" from the groups
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

#' Apply a Function to a Temporarily Ungrouped Data Frame
#'
#' Occasionally it is useful to ungroup a data frame before applying a
#' calculation that would otherwise by slowed down by subdividing the
#' calculation by group. In these circumstances, the calculation should be
#' independent of the grouping. `with_ungroup()` temporarily removes groups,
#' applies the function `.f` to `.data` (as `.f(.data)`) and then restores
#' the original grouping. This function is fastest when the applied function
#' does not modify the row order or the value of grouping columns. In these
#' cases, the group index will need to be recalculated adding computational
#' overhead depending on the number of rows and groups in the data.
#'
#' @examples
#' # with_ungroup() ungroups the input data frame, applies the inner function,
#' # and restores grouping on output
#' tidyr::table1 %>%
#'   dplyr::group_by(country, year) %>%
#'   with_ungroup(~ dplyr::mutate(., r = cases / population))
#'
#' # Groups that "disappear" are implicitly dropped, with a warning
#' tidyr::table1 %>%
#'   dplyr::group_by(country, year) %>%
#'   with_ungroup(~ {
#'     dplyr::mutate(., r = cases/population) %>%
#'       dplyr::select(-year)
#'   })
#'
#' # Works like "normal" if no groupings are present
#' tidyr::table1 %>%
#'   with_ungroup(~ dplyr::mutate(., r = cases/population))
#'
#' @inheritParams purrr::as_mapper
#' @inheritParams group_drop
#' @family Group Utilities
#' @export
with_ungroup <- function(.data, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  if (!inherits(.data, "grouped_df")) return(.f(.data))
  data_group_vars <- group_vars(.data)
  data_group_data <- dplyr::group_data(.data)
  n_rows_init <- nrow(.data)

  .data <- ungroup(.data)
  .data$...sentinel <- sentinel <- seq_along(nrow(.data))
  .data <- .f(.data)

  fast_regroup <- FALSE
  if (nrow(.data) == n_rows_init) {
    fast_regroup <- !any(.data$...sentinel != sentinel)
  }
  .data$...sentinel <- NULL

  regroup(
    .data,
    data_group_vars,
    group_data = if (fast_regroup) data_group_data
  )
}

#' Restore Groups After Applying a Function to a Data Frame
#'
#' Groups are sometimes removed by [dplyr] functions, such as
#' [dplyr::summarize()]. However, it may be necessary to ensure that the data
#' output by a function retains as many of the original groups as are available
#' after applying the function. `with_retain_groups()` applies a function to a
#' grouped data frame and restores the original group as much as possible.
#'
#' @examples
#' # with_retain_groups() applies inner function to grouped data frame
#' # and restores grouping on output
#' tidyr::table1 %>%
#'   dplyr::group_by(country, year) %>%
#'   with_retain_groups(~ dplyr::summarize(., cases = sum(cases)))
#'
#' # Groups that "disappear" are implicitly dropped, with a warning
#' tidyr::table1 %>%
#'   dplyr::group_by(country, year) %>%
#'   with_retain_groups(~ {
#'     dplyr::summarize(., r = cases / population) %>%
#'       dplyr::summarize(r = mean(r))
#'   })
#'
#' # Works like "normal" if no groupings are present
#' tidyr::table1 %>%
#'   with_retain_groups(~ dplyr::mutate(., r = cases / population))
#'
#' @inheritParams with_ungroup
#' @family Group Utilities
#' @export
with_retain_groups <- function(.data, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  if (!inherits(.data, "grouped_df")) return(.f(.data))
  data_group_vars <- group_vars(.data)
  .data <- .f(.data)
  regroup(.data, data_group_vars)
}

regroup <- function(.data, groups_possible, group_data = NULL) {
  # Check that all group vars are still present
  g_after <- intersect(groups_possible, names(.data))
  g_missing <- setdiff(groups_possible, g_after)
  if (length(g_missing)) {
    g_missing <- glue::glue_collapse(glue("`{g_missing}`"), ", ")
    warn(glue("groups were implicitly dropped: {g_missing}"))
  }

  if (!is.null(group_data)) group_data <- align_grouping(.data, group_data)

  recalculate_grouping <- is.null(group_data) || length(g_missing) > 0

  if (recalculate_grouping) {
    return(group_by(.data, !!!rlang::syms(g_after)))
  } else {
    dplyr::new_grouped_df(.data, groups = group_data)
  }
}

align_grouping <- function(.data, group_data) {
  group_vars <- setdiff(names(group_data), ".rows")
  for (gv in group_vars) {
    if (is.factor(.data[[gv]])) {
      group_data[[gv]] <- factor(
        group_data[[gv]],
        levels = levels(.data[[gv]]),
        ordered = is.ordered(.data[[gv]])
      )
    } else {
      data_class <- class(.data[[gv]])
      coerce <- switch(
        data_class[length(data_class)],
        "integer" = as.integer,
        "numeric" = as.numeric,
        "character" = as.character,
        return(NULL)
      )
      group_data[[gv]] <- coerce(group_data[[gv]])
    }

    if (mismatched_values(group_data[[gv]], .data[[gv]])) {
      return(NULL)
    }
  }
  group_data
}

mismatched_values <- function(x, y) UseMethod("mismatched_values")

mismatched_values.default <- function(x, y) {
  length(setdiff(unique(x), unique(y))) > 0
}

mismatched_values.factor <- function(x, y) {
  length(setdiff(unique(as.character(x)), unique(as.character(y)))) > 0
}
