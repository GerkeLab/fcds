fcds:::requires_package(c("callr", "here"), "data importing scripts")

UPDATE_FCDS_EXAMPLE <- FALSE

fs::dir_create(here::here("data"))
callr::rscript(here::here("data-raw/01_county-fips-florida.R"))
callr::rscript(here::here("data-raw/02_seer-pop-florida-county.R"))
callr::rscript(here::here("data-raw/03_seer-pop-us-standard.R"))
callr::rscript(here::here("data-raw/04_seer-icd-o-3.R"))
callr::rscript(here::here("data-raw/06_iacr-icd-o-3.R"))

if (UPDATE_FCDS_EXAMPLE && length(fcds::fcds_cache_ls())) {
  callr::rscript(here::here("data-raw/05_fcds-example.R"))
}

# may need to run this next bit manually
devtools::load_all('.')
testthat::test_file(here::here("tests/testthat/test-pkg-data.R"))

if (!requireNamespace("USAboundaries", quietly = TRUE)) {
  usaboundaries_counties_fl <- USAboundaries::us_counties(states = "FL")
  usethis::use_data(usaboundaries_counties_fl, internal = TRUE, overwrite = TRUE)
}
