#' Gets county-level population files from SEER
#' https://seer.cancer.gov/popdata/download.html
#'
#' Data dictionary: https://seer.cancer.gov/popdata/popdic.html
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

use_data <- partial(usethis::use_data, overwrite = TRUE, compress = "xz")

county_fips_file <- here::here("data", "county_fips_code.rda")
if (!file.exists(county_fips_file)) stop(
  "Please run `data-raw/county_fips_code.R` first"
)

county_fips_code <- fcds::county_fips_code %>%
  mutate(county_fips = sprintf("%03d", as.integer(fips_code)))

# Download Files ----------------------------------------------------------
seer_pop_fl_file <- here::here("data-raw", "seer_fl-1969-2016_19ages.txt.gz")
if (!file.exists(seer_pop_fl_file)) {
  download.file(
    "https://seer.cancer.gov/popdata/yr1969_2016.19ages/fl.1969_2016.19ages.txt.gz",
    seer_pop_fl_file
  )
}

seer_pop_fl_exp_race_file <- here::here(
  "data-raw", "seer_fl-1990-2016_19ages.txt.gz")
if (!file.exists(seer_pop_fl_exp_race_file)) download.file(
  "https://seer.cancer.gov/popdata/yr1990_2016.19ages/fl.1990_2016.19ages.txt.gz",
  seer_pop_fl_exp_race_file
)

seer_pop_us_file <- here::here("data-raw", "us.1990_2017.19ages.adjusted.txt.gz")
if (!file.exists(seer_pop_us_file)) {
  download.file(
    "https://seer.cancer.gov/popdata/yr1990_2017.19ages/us.1990_2017.19ages.adjusted.txt.gz",
    seer_pop_us_file
  )
}

# Recode Functions --------------------------------------------------------
# Data dictionary: https://seer.cancer.gov/popdata/popdic.html
recode_registry <- c("01" = "San Francisco-Oakland SMSA",
                     "02" = "Connecticut",
                     "20" = "Detroit (Metropolitan)",
                     "21" = "Hawaii",
                     "22" = "Iowa",
                     "23" = "New Mexico",
                     "25" = "Seattle (Puget Sound)",
                     "26" = "Utah",
                     "27" = "Atlanta (Metropolitan)",
                     "29" = "Alaska Natives",
                     "31" = "San Jose-Monterey",
                     "33" = "Arizona Indians",
                     "35" = "Los Angeles",
                     "37" = "Rural Georgia",
                     "41" = "California excluding SF/SJM/LA",
                     "42" = "Kentucky",
                     "43" = "Louisiana",
                     "44" = "New Jersey",
                     "47" = "Georgia excluding Atlanta/Rural Georgia",
                     "99" = "Registry for non-SEER area")
recode_race_1969 <- c("1" = "White",
                      "2" = "Black",
                      "3" = "Other")
recode_race_1990 <- c("1" = "White",
                      "2" = "Black",
                      "3" = "American Indian/Alaska Native",
                      "4" = "Asian or Pacific Islander")
recode_origin_1990 <- c("0" = "Non-Hispanic",
                        "1" = "Hispanic",
                        "9" = NA_character_)
                        # "9" = "Not applicable in 1969-2011 W,B,O files")
recode_sex <- c("1" = "Male", "2" = "Female")

age_18_groups_by_index <- function(i) {
  if (i == 0) return("0")
  if (i == 1) return("0 - 4")
  if ((i-1) * 5 == 85) return("85+")
  paste((i-1) * 5, i * 5 - 1, sep = " - ")
}
recode_age_groups <- map_chr(0:18, age_18_groups_by_index)
names(recode_age_groups) <- sprintf("%02d", 0:18)


# Read SEER Fixed Width File ----------------------------------------------
read_seer_fwf <- function(file, ...) {
  readr::read_fwf(
    file,
    col_positions = readr::fwf_widths(
      widths = c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
      col_names = c("year", "state", "state_fips", "county_fips",
                    "registry", "race", "origin", "sex", "age_group", "population")
    ),
    col_types = readr::cols(
      population = readr::col_integer(),
      .default = readr::col_character()
    ),
    ...
  )
}

# Load Data ---------------------------------------------------------------

# ---- SEER Florida Population ----
seer_pop_fl <-
  read_seer_fwf(seer_pop_fl_file) %>%
  mutate(
    age_group = recode(age_group, "00" = "01")
  ) %>%
  group_by(year, state, state_fips, county_fips, registry, race, origin, sex, age_group) %>%
  summarize(population = sum(as.integer(population))) %>%
  ungroup() %>%
  mutate(
    registry = recode_registry[registry],
    race = recode_race_1969[race],
    origin = recode_origin_1990[origin],
    sex = recode_sex[sex],
    age_group = recode_age_groups[age_group]
  ) %>%
  left_join(
    county_fips_code %>% select(starts_with("county")),
    by = "county_fips"
  )

use_data(seer_pop_fl)

# ---- SEER Florida Population with Expanded Race ----
seer_pop_fl_exp_race <-
  read_seer_fwf(seer_pop_fl_exp_race_file) %>%
  mutate(
    age_group = recode(age_group, "00" = "01")
  ) %>%
  group_by(year, state, state_fips, county_fips, registry, race, origin, sex, age_group) %>%
  summarize(population = sum(as.integer(population))) %>%
  ungroup() %>%
  mutate(
    registry = recode_registry[registry],
    race = recode_race_1990[race],
    origin = recode_origin_1990[origin],
    sex = recode_sex[sex],
    age_group = recode_age_groups[age_group]
  ) %>%
  left_join(
    county_fips_code %>% select(starts_with("county")),
    by = "county_fips"
  )

use_data(seer_pop_fl_exp_race)

# ---- SEER US Population ----
read_seer_pop_us <- function(txt, ...) {
  if (length(txt == 1) && !grepl("\n", txt)) {
    # treat txt as path and see if un-gzipped version exists
    path_ungzipped <- fs::path_ext_remove(txt)
    if (fs::file_exists(path_ungzipped)) {
      txt <- path_ungzipped
      message("Using: ", txt)
    }
  }
  read_seer_fwf(txt, ...)
}

seer_pop_us <-
  read_seer_pop_us(seer_pop_us_file) %>%
  mutate(
    age_group = recode(age_group, "00" = "01")
  ) %>%
  group_by(year, race, origin, sex, age_group) %>%
  summarize(population = sum(population)) %>%
  ungroup() %>%
  mutate(
    race = factor(race, names(recode_race_1990), recode_race_1990),
    origin = factor(origin, names(recode_origin_1990), recode_origin_1990),
    sex = factor(sex, names(recode_sex), recode_sex),
    age_group = recode_age_groups[age_group]
  )

use_data(seer_pop_us)
