# Population data
library(tidyverse)
library(tidycensus)

# Get Decenniel Variables -------------------------------------------------
message("Getting Decennial Variables")
dec_years <- seq(1980, 2010, 10)
safe_load <- safely(load_variables)
dec_vars <- dec_years %>%
  set_names(paste(.)) %>%
  map(safe_load, dataset = "sf1", cache = TRUE)

# Remove Failures
dec_vars <- dec_vars %>% map("result") %>% compact()

# Focus on 2000
# DT::datatable(dec_vars$`2000`, filter = "top")

age_boundaries <- function(age) {
  age <- str_remove_all(age, "yrs")
  age <- str_replace(age, "over", "Inf")
  age <- str_split(age, pattern = "\\s*(&|to)\\s*")[[1]]
  age_df <- function(h, l = h) tibble(age_high = h, age_low = l)
  age <- if (length(age) == 1) {
    if (age == "All") {
      age_df(-Inf, Inf)
    } else if (age == "<5") {
      age_df(0, 4)
    } else {
      age <- as.numeric(age)
      age_df(age)
    }
  } else {
    age <- as.numeric(age)
    age_df(age[1], age[2])
  }
}

age_group_format <- function(age_high, age_low) {
  if (age_high == 5) age_low <- 0
  if (is.infinite(age_high)) {
    "All"
  } else if (age_high == age_low) {
    paste(age_high)
  } else {
    paste(age_low, age_high, sep = " - ")
  }
}


# Tidy 2000 Decennial Pop Vars --------------------------------------------
dec_2000_popvars <-
  dec_vars[["2000"]] %>%
  filter(str_detect(name, "^P(001|007|008|012)")) %>%
  mutate(label = str_remove_all(label, "'")) %>%
  separate(label, paste0("x", 1:4), sep = ":", fill = "right") %>%
  # Fix bad label format with extra ":"
  mutate(x3 = if_else(name == "P012G046", x4, x3)) %>%
  rename(category = x1) %>%
  select(-x4) %>%
  extract(name, "p", regex = "P(\\d{3})", remove = FALSE) %>%
  mutate(p = as.integer(p)) %>%
  extract(concept, "race", "\\((.+)\\)", remove = FALSE) %>%
  mutate(
    hispanic = case_when(
      !p %in% c(8, 12) ~ "All",
      str_detect(x2, "Hispanic&Not") ~ "All",
      str_detect(x2, "Not") ~ "No",
      str_detect(x2, "Hisp") ~ "Yes",
      str_detect(category, "NOT HISP") ~ "No",
      str_detect(category, "HISP") ~ "Yes",
      TRUE ~ "All"
    ),
    # Race is in 2nd and 3rd columns for lower variables
    race = grkmisc::if_na(race, p <= 7, then = x2),
    race = grkmisc::if_na(race, p == 8, then = x3),
    race = str_remove(race, "\\s[Aa]lone"),
    # Fix odd label formatting
    x2 = grkmisc::if_na(x2, name == "P012C002", then = category),
    category = ifelse(name == "P012C002", "SEX BY AGE AM IN/AK NTV", category),
    race = case_when(
      str_detect(race, "Tot") ~ "All",
      race == "AmInd/AK" ~ "American Indian And Alaska Native",
      race == "Bl/AfAm" ~ "Black Or African American",
      race == "Asian" ~ "Asian",
      race == "HI" ~ "Native Hawaiian And Other Pacific Islander",
      race == "Other" ~ "Some Other Race",
      str_detect(race, "Two.+races") ~ "Two Or More Races",
      str_detect(race, "White") ~ "White", # White non-hisp is race = white
      str_detect(race, "Hispanic") ~ "All", # Hispanic is all races
      TRUE ~ race
    ),
    race = grkmisc::if_na(race, then = "All"),
    sex = case_when(
      p < 12 ~ "All",
      str_detect(x2, "Male&Female") ~ "All",
      str_detect(x2, "Female") ~ "Female",
      str_detect(x2, "Male") ~ "Male",
      TRUE ~ "All"
    ),
    age = case_when(
      p < 12 ~ "All",
      TRUE ~ str_trim(x3)
    ),
    age = grkmisc::if_na(age, then = "All")
  ) %>%
  mutate(age_boundaries = map(age, age_boundaries)) %>%
  unnest(age_boundaries) %>%
  mutate(age_group = map2_chr(age_low, age_high, age_group_format)) %>%
  mutate(category = tools::toTitleCase(tolower(category))) %>%
  select(
    variable = name,
    variable_number = p,
    category, sex, race, hispanic,
    contains("age")
  ) %>%
  # filter(!str_detect(race, "Hispanic")) %>%
  group_by(sex, race, hispanic, age_group) %>%
  slice(1) %>%
  ungroup()

options(tigris_use_cache = TRUE)

data("fips_codes")
fips_codes <-
  fips_codes %>%
  mutate(geoid = paste0(state_code, county_code))


# Get US 2000 Decennial Census Data ---------------------------------------
message("Getting US and Florida decennial data")
us_dec_2000_src <-
  get_decennial(
    geography = "county",
    year = 2000,
    variables = dec_2000_popvars$variable,
    geometry = FALSE,
    keep_geo_vars = TRUE
  ) %>%
  rename(geoid = GEOID) %>%
  select(-NAME) %>%
  left_join(fips_codes, by = "geoid") %>%
  mutate(county = str_remove(county, "\\s*County"))


# Calculate Hispanic Subgroups not Present in Data ------------------------
calc_all_minus_subgroup <- function(.data, varlist, filter_subgroup, filter_group) {
  filter_subgroup <- rlang::enexpr(filter_subgroup)
  filter_group <- rlang::enexpr(filter_group)

  filter_join_ungroup <- function(.data, varlist, filter_exp) {
    varlist %>%
      filter(!!rlang::enexpr(filter_exp)) %>%
      left_join(.data, by = "variable") %>%
      ungroup()
  }

  subgroup <- filter_join_ungroup(.data, varlist, !!filter_subgroup)
  group <- filter_join_ungroup(.data, varlist, !!filter_group)

  x <- inner_join(
    subgroup,
    select(group, geoid, sex, age_group, variable, value),
    by = c("geoid", "sex", "age_group"),
    suffix = c("", ".all_group")
  ) %>%
    mutate(
      variable = paste0(variable, "_C"),
      value = value.all_group - value
    ) %>%
    select(colnames(.data))

  list(
    data = x,
    vars = varlist %>% ungroup() %>% filter(!!filter_subgroup) %>%
      mutate(variable = paste0(variable, "_C"))
  )
}

# Calculate White Hispanic by Age & Sex
white_hisp <- calc_all_minus_subgroup(
  us_dec_2000_src, dec_2000_popvars,
  filter_subgroup = race == "White" & hispanic == "No",
  filter_group = race == "White" & hispanic == "All"
)
white_hisp$vars <- mutate(white_hisp$vars, hispanic = "Yes")

# Calculate All races Non-Hispanic by Age & Sex
all_non_hisp <- calc_all_minus_subgroup(
  us_dec_2000_src, dec_2000_popvars,
  filter_subgroup = race == "All" & hispanic == "Yes",
  filter_group = race == "All" & hispanic == "All"
)
all_non_hisp$vars <- mutate(all_non_hisp$vars, hispanic = "No")

# Encorporate calculated data and variable list
us_dec_2000_src <- bind_rows(
  us_dec_2000_src,
  white_hisp$data,
  all_non_hisp$data
)

dec_2000_popvars <- bind_rows(
  dec_2000_popvars,
  white_hisp$vars,
  all_non_hisp$vars
)


# Add Male and Female Age Groups ------------------------------------------
age_groups <-
  dec_2000_popvars %>%
  ungroup() %>%
  filter(sex != "All", age_group != "All") %>%
  left_join(us_dec_2000_src, by = "variable") %>%
  split(.$sex) %>%
  reduce(left_join, by = c("geoid", "race", "hispanic", "age_group"),
         suffix = c("", ".Male")) %>%
  mutate(
    value = value + value.Male,
    variable = paste0(variable, "_A")
  ) %>%
  select(colnames(us_dec_2000_src))

age_groups_vars <-
  dec_2000_popvars %>%
  ungroup() %>%
  filter(sex == "Female", age_group != "All") %>%
  mutate(sex = "All", variable = paste0(variable, "_A"))

us_dec_2000_src <- bind_rows(us_dec_2000_src, age_groups)
dec_2000_popvars <- bind_rows(dec_2000_popvars, age_groups_vars)

# Subset to Florida -------------------------------------------------------
fl_dec_2000 <-
  us_dec_2000_src %>%
  filter(state_name == "Florida") %>%
  left_join(dec_2000_popvars, by = "variable")


# Summarize US ------------------------------------------------------------
us_dec_2000 <-
  us_dec_2000_src %>%
  group_by(state_name, variable) %>%
  summarize(value = sum(value)) %>%
  left_join(dec_2000_popvars, by = "variable")


# Save Data ---------------------------------------------------------------
saveRDS(dec_2000_popvars, here::here("data", "dec_2000_popvars.rds"))
saveRDS(fl_dec_2000, here::here("data", "fl_dec_2000.rds"))
saveRDS(us_dec_2000, here::here("data", "us_dec_2000.rds"))



# Old Code ----------------------------------------------------------------

# # Select Datasets (Population Variables) to Download ---
# acs5_2017_varlist <- load_variables(2017, "acs5", cache = TRUE)
# acs5_2017_popvars <-
#   acs5_2017_varlist %>%
#   filter(
#     str_detect(label, "^Estimate!!Total.+(Male|Female).+years$"),
#     concept == "SEX BY AGE" | str_detect(concept, "(WHITE|BLACK).+ALONE"),
#     str_detect(name, "B01001")
#   )
#
#
# # Get totals as well ---
# acs5_2017_popvars <- bind_rows(
#   acs5_2017_varlist %>%
#     filter(name %in% c("B00001_001", "B01001_001", "B01001_002", "B01001_026")),
#   acs5_2017_popvars
# ) %>%
#   separate(label, c("estimate", "total", "sex", "age_group"), sep = "!!", fill = "right") %>%
#   select(-estimate, -total) %>%
#   replace_na(list(sex = "Total", age_group = "All"))
#
# saveRDS(acs5_2017_popvars, here::here("data", "acs5_popvars.rds"))
#
#
# # Get Florida County-level Data ---
# fl_acs5_county <-
#   get_acs(
#     geography = "county",
#     state = "Florida",
#     year = 2017,
#     variables = acs5_2017_popvars$name,
#     keep_geo_vars = TRUE
#   ) %>%
#   rename(geoid = GEOID, name = NAME) %>%
#   left_join(
#     acs5_2017_popvars,
#     by = c("variable" = "name")
#   )
#
# saveRDS(fl_acs5_county, here::here("data", "fl_acs5_county.rds"))
#
#
# # Get US population ---
# us_acs5 <-
#   get_acs(
#     geography = "county",
#     year = 2017,
#     variables = acs5_2017_popvars$name,
#     keep_geo_vars = TRUE
#   ) %>%
#   rename(geoid = GEOID, name = NAME) %>%
#   left_join(
#     acs5_2017_popvars,
#     by = c("variable" = "name")
#   )
#
# saveRDS(us_acs5, here::here("data", "us_acs5.rds"))

