# ---- Check for existing packages ----
check_package <- function(
  pkg,
  needed_by = NULL,
  how_to_install = paste0("install.packages('", pkg, "')"),
  warn = TRUE
) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (is.null(fun)) fun <- "{fcds}"
    msg <- glue::glue(
      "Package `{pkg}` is {if (warn) 'suggested' else 'required'} by {needed_by}.",
      "\n{how_to_install}"
    )
    if (warn) rlang::warn(msg) else rlang::stop(msg)
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
