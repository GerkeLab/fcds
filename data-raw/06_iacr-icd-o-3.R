library(tidyverse)
library(readxl)

# Download ICD-O-3 Terms from IACR
icdo3_url <- "http://www.iacr.com.fr/images/Newsflash/ICD-O-3.2_MFin_02082019_web.xls"
tmp <- here::here("data-raw", basename(icdo3_url))
download.file(icdo3_url, tmp)

# Read in the Excel file (first row is version)
icdo3 <- readxl::read_xls(tmp, skip = 1, guess_max = 1e5)

# Version is first row
icdo3_version <- readxl::read_xls(tmp, n_max = 1, col_names = FALSE)[[1]]

# Store source information as attributes
attributes(icdo3)$version <- icdo3_version
attributes(icdo3)$url <- icdo3_url

iacr_icd_o_3 <-
  icdo3 %>%
  slice(-1) %>%
  select(1:3) %>%
  mutate(
    ICDO3_morphology = ICDO3.2,
    ICDO3 = substr(ICDO3.2, 1, 3),
    ICDO3_histology = substr(ICDO3.2, 1, 4)
  ) %>%
  rename(level = Level, term = Term) %>%
  select(starts_with("ICDO3"), level, term, -ICDO3.2)

usethis::use_data(iacr_icd_o_3, overwrite = TRUE, compress = "xz")
