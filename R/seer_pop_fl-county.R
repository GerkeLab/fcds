#' Gets county-level population files from SEER
#' https://seer.cancer.gov/popdata/download.html
#'
#' Data dictionary: https://seer.cancer.gov/popdata/popdic.html
library(tidyverse)
source(here::here("R/county_fips_code.R"))
county_fips_code <- county_fips_code %>%
  mutate(county_fips = sprintf("%03d", as.integer(fips_code)))

# Download Files ----------------------------------------------------------
# library(rvest)
# x <- read_html("https://seer.cancer.gov/popdata/download.html")

seer_fl_pop_file <- here::here("data-raw", "seer_fl-1969-2016_19ages.txt.gz")
if (!file.exists(seer_fl_pop_file)) {
  download.file(
    "https://seer.cancer.gov/popdata/yr1969_2016.19ages/fl.1969_2016.19ages.txt.gz",
    seer_fl_pop_file
  )
}

seer_fl_pop_exp_race_file <- here::here(
  "data-raw", "seer_fl-1990-2016_19ages.txt.gz")
if (!file.exists(seer_fl_pop_exp_race_file)) download.file(
  "https://seer.cancer.gov/popdata/yr1990_2016.19ages/fl.1990_2016.19ages.txt.gz",
  seer_fl_pop_exp_race_file
)


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


# Load Data ---------------------------------------------------------------
seer_fl_pop <-
  read_lines(seer_fl_pop_file) %>%
  data_frame(raw = .) %>%
  extract(
    raw,
    c("year", "state", "state_fips", "county_fips",
      "registry", "race", "origin", "sex", "age_group", "population"),
    regex = "(.{4})(.{2})(.{2})(.{3})(.{2})(.{1})(.{1})(.{1})(.{2})(.{8})"
  ) %>%
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

saveRDS(seer_fl_pop, here::here("data", "seer_fl_pop.rds"))

seer_fl_pop_exp_race <-
  read_lines(seer_fl_pop_exp_race_file) %>%
  data_frame(raw = .) %>%
  extract(
    raw,
    c("year", "state", "state_fips", "county_fips",
      "registry", "race", "origin", "sex", "age_group", "population"),
    regex = "(.{4})(.{2})(.{2})(.{3})(.{2})(.{1})(.{1})(.{1})(.{2})(.{8})"
  ) %>%
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

saveRDS(seer_fl_pop_exp_race,
        here::here("data", "seer_fl_pop_exp_race.rds"))
