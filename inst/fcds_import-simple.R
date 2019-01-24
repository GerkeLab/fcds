library(tidyverse)
source(here::here("R", "county_fips_code.R"))

DATA_FILE <- here::here("data/STAT_dataset_2018.dat")
# DATA_FILE <- here::here("data/subsample.csv")

dat_raw <- read_csv(DATA_FILE) %>%
  janitor::clean_names()

fmt_dx_years <- function(x) {
  if (nchar(x) == 0 || is.na(x)) return(x)
  stopifnot(nchar(x) == 4)
  list(
    start = substr(x, 1, 2),
    end = substr(x, 3, 4)
  ) %>%
    map(~ ifelse(as.integer(.) < 50, paste0("20", .), paste0("19", .))) %>%
    reduce(paste, sep = " - ")
}

dat <-
  dat_raw %>%
  # Get county name from FIPS code
  mutate_at(vars(county_at_dx_n90), as.character) %>%
  left_join(county_fips_code, by = c("county_at_dx_n90" = "fips_code")) %>%
  mutate(county_name = tools::toTitleCase(county_name)) %>%
  # Recodes from STAT 2018 layout
  mutate(
    date_of_dx_year = fmt_dx_years(date_of_dx_year_recoded),
    addr_at_dx_state = recode(
      addr_at_dx_state_recoded,
      "00" = "Florida",
      "01" = "Other US States and Territories",
      "02" = "Not Applicable"
    ),
    birthplace_state_abrv = recode(
      birthplace_state_abrv_recoded,
      "00" = "Florida",
      "01" = "Other US States and Territories",
      "02" = "Not Applicable"
    ),
    addr_at_dx_country = recode(
      addr_at_dx_country_recoded,
      "01" = "US States and Territories",
      "02" = "Other Countries",
      "99" = "Unknown"
    ),
    birthplace_country = recode(
      birthplace_country_recoded,
      "01" = "US States and Territories",
      "02" = "Other Countries",
      "99" = "Unknown"
    ),
    marital_status_short = recode(
      marital_status_recoded,
      "1" = "Married",
      "2" = "Single",
      "9" = "Unknown"
    ),
    marital_status = recode(
      marital_status_recoded,
      "1" = "Married (including common law); Unmarried or Domestic Partner (same sex or opposite sex, registered or unregistered)",
      "2" = "Single, Separated, Divorced or Widowed",
      "9" = "Unknown"
    ),
    race = recode(
      race_recoded,
      "1" = "White",
      "2" = "Black",
      "3" = "Other",
      "9" = "Unknown"
    ),
    ethnicity = recode(
      ethnicity_recoded,
      "0" = "Not Hispanic",
      "8" = "Hispanic",
      "9" = "Unknown"
    ),
    sex = recode(
      sex_recoded,
      "1" = "Male",
      "2" = "Female",
      "9" = "Other/Unknown"
    ),
    dx_primary_payor = recode(
      dx_primary_payor_recoded,
      "01" = "Not insured; not insured self pay",
      "02" = "Insurance, NOS; Private insurance; Managed care, HMO, PPO, Private insurance Fee-for-service",
      "03" = "Medicaid; Medicaid administered through a managed care plan",
      "04" = "Medicare; Medicare NOS; Medicare with supplement NOS; Medicare administered through managed care plan; Medicare with private supplement; Medicare with Medicaid eligibility",
      "05" = "Tricare, Military, Veterans Affairs, Indian/Public health service",
      "99" = "Unknown"
    )
  )

saveRDS(dat, here::here("data", "stat_dataset_2018.rds"))
