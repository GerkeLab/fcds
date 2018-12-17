library(tidyverse)

# Get Boundaries ----------------------------------------------------------
library(USAboundaries)
florida_counties <- us_counties(states = "Florida")


# Import US Census Data ---------------------------------------------------
dec_2000_popvars <- readRDS(here::here("data", "dec_2000_popvars.rds"))
fl_dec_2000 <- readRDS(here::here("data", "fl_dec_2000.rds"))
us_dec_2000 <- readRDS(here::here("data", "us_dec_2000.rds"))

us_total_2000 <-
  us_dec_2000 %>%
  group_by(variable, variable_number, category, sex, race, hispanic,
           age, age_low, age_high, age_group) %>%
  summarize(value = sum(value))
