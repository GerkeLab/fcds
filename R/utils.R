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

group_drop <- function(.data, ..., .remove = FALSE) {
  group_var <- enquos(...)
  group_var_name <- map_chr(group_var, quo_name)
  are_in_groups <- map_lgl(group_var_name, ~ . %in% group_vars(.data))
  if (!any(are_in_groups)) return(.data)
  .groups <- groups(.data) %>% set_names(group_vars(.data))
  .groups <- .groups[setdiff(names(.groups), group_var_name)]
  .data <- .data %>% ungroup() %>% group_by(!!!.groups)
  if (!.remove) return(.data)
  select(.data, -group_var_name)
}
