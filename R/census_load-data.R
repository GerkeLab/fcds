library(tidyverse)

# Get Boundaries ----------------------------------------------------------
library(USAboundaries)
florida_counties <- us_counties(states = "Florida")


# Import US Census Data ---------------------------------------------------
# acs5_popvars <- readRDS(here::here("data", "acs5_popvars.rds"))
# fl_acs5_county <- readRDS(here::here("data", "fl_acs5_county.rds"))
# us_acs5 <- readRDS(here::here("data", "us_acs5.rds"))
#
# us_acs5_total <-
#   us_acs5 %>%
#   group_by(variable, sex, age_group, concept) %>%
#   summarize(total = sum(estimate))

# fs::dir_ls(here::here("data"), regexp = "dec") %>%
#   fs::path_rel(here::here()) %>%
#   map_chr(~ paste0(fs::path_file(fs::path_ext_remove(.x)),
#                    ' <- readRDS(here::here("',
#                    fs::path_dir(.x), '", "', fs::path_file(.x), '"))')) %>%
#   cat(sep = "\n")

dec_2000_popvars <- readRDS(here::here("data", "dec_2000_popvars.rds"))
fl_dec_2000 <- readRDS(here::here("data", "fl_dec_2000.rds"))
us_dec_2000 <- readRDS(here::here("data", "us_dec_2000.rds"))

us_total_2000 <-
  us_dec_2000 %>%
  group_by(variable, variable_number, category, sex, race, hispanic,
           age, age_low, age_high, age_group) %>%
  summarize(value = sum(value))
