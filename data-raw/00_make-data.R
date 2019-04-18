fcds:::requires_package(c("callr", "here"), "data importing scripts")

fs::dir_create(here::here("data"))
callr::rscript(here::here("data-raw/01_county-fips-florida.R"))
callr::rscript(here::here("data-raw/02_seer-pop-florida-county.R"))
callr::rscript(here::here("data-raw/03_seer-pop-us-standard.R"))
callr::rscript(here::here("data-raw/04_seer-icd-o-3.R"))
