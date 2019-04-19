fcds:::requires_package(c("here", "rvest", "readxl"), "icd-o-3 import")

library(rvest)
library(readxl)
library(dplyr)

seer_icdo3_base_url <- "https://seer.cancer.gov/icd-o-3"
seer_icdo3 <- read_html(seer_icdo3_base_url)

seer_icdo3_xls <-
  seer_icdo3 %>%
  xml_nodes("a") %>%
  xml_attr("href") %>%
  grep("icdo3.+xlsx?$", ., value = TRUE) %>%
  file.path(seer_icdo3_base_url, .)

seer_icdo3_file <- here::here("data-raw", basename(seer_icdo3_xls))
download.file(seer_icdo3_xls, seer_icdo3_file)

seer_icd_o_3_raw <- read_excel(seer_icdo3_file)

seer_icd_o_3 <-
  seer_icd_o_3_raw %>%
  rename_all(fcds:::to_snake_case) %>%
  select(-contains("site_")) %>%
  mutate_at(vars(histology:histology_behavior_description), factor)

usethis::use_data(seer_icd_o_3, compress = "xz", overwrite = TRUE)
