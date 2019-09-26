
tidyr_unnest <- function(...) {
  if (utils::packageVersion("tidyr") < "1.0.0") {
    tidyr::unnest(...)
  } else {
    tidyr::unnest_legacy(...)
  }
}

tidyr_nest <- function(...) {
  if (utils::packageVersion("tidyr") < "1.0.0") {
    tidyr::nest(...)
  } else {
    tidyr::nest_legacy(...)
  }
}
