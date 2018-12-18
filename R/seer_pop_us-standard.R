library(readr)
library(dplyr)
library(stringr)

# SEER Standard Ages ----
# https://seer.cancer.gov/stdpopulations/

# Standard Populations - 18 Age Groups (0-4, 5-9, 10-14, ..., 85+)
# https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt
download.file("https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt",
              here::here("data-raw", "seer_stdpop-18ages.txt"))
seer_std_age_18 <- read_lines(here::here("data-raw", "seer_stdpop-18ages.txt"))

# Standard Populations - Single Ages to 84 and then 85+ (TXT, 4 KB) (2000 U.S.,
# World (WHO 2000-2025), and Canadian 2011 standards only)
# https://seer.cancer.gov/stdpopulations/stdpop.singleagesthru84.txt
download.file("https://seer.cancer.gov/stdpopulations/stdpop.singleagesthru84.txt",
              here::here("data-raw", "seer_stdpop-singleagesthru84.txt"))
seer_std_ages <- read_lines(here::here("data-raw", "seer_stdpop-singleagesthru84.txt"))

age_18_groups_by_index <- function(i) {
  if (i == 0) return("0")
  if (i == 1) return("0 - 4")
  if ((i-1) * 5 == 85) return("85+")
  paste((i-1) * 5, i * 5 - 1, sep = " - ")
}

seer_standard_dictionary <- c(
  "006" = "World (Segi 1960) Std Million (19 age groups)",
  "007" = "1991 Canadian Std Million (19 age groups)",
  "005" = "European (Scandinavian 1960) Std Million (19 age groups)",
  "008" = "1996 Canadian Std Million (19 age groups)",
  "010" = "World (WHO 2000-2025) Std Million (19 age groups)",
  "014" = "European (EU-27 plus EFTA 2011-2030) Std Million (19 age groups)",
  "016" = "2011 Canadian Standard Population (19 age groups)",
  "141" = "1940 U.S. Std Million (19 age groups)",
  "151" = "1950 U.S. Std Million (19 age groups)",
  "161" = "1960 U.S. Std Million (19 age groups)",
  "171" = "1970 U.S. Std Million (19 age groups)",
  "181" = "1980 U.S. Std Million (19 age groups)",
  "191" = "1990 U.S. Std Million (19 age groups)",
  "201" = "2000 U.S. Std Million (19 age groups)",
  "203" = "2000 U.S. Std Population (19 age groups - Census P25-1130)",
  "017" = "2011 Canadian Standard Population (single age to 84)",
  "202" = "2000 U.S. Std Population (single ages to 84 - Census P25-1130)",
  "205" = "2000 U.S. Std Population (single ages to 99 - Census P25-1130)",
  "011" = "World (WHO 2000-2025) Std Million (single ages to 84)",
  "012" = "World (WHO 2000-2025) Std Million (single ages to 99)",
  "001" = "World (Segi 1960) Std Million (18 age groups)",
  "002" = "1991 Canadian Std Million (18 age groups)",
  "003" = "European (Scandinavian 1960) Std Million (18 age groups)",
  "013" = "European (EU-27 plus EFTA 2011-2030) Std Million (18 age groups)",
  "004" = "1996 Canadian Std Million (18 age groups)",
  "015" = "2011 Canadian Standard Population (18 age groups)",
  "009" = "World (WHO 2000-2025) Std Million (18 age groups)",
  "140" = "1940 U.S. Std Million (18 age groups)",
  "150" = "1950 U.S. Std Million (18 age groups)",
  "160" = "1960 U.S. Std Million (18 age groups)",
  "170" = "1970 U.S. Std Million (18 age groups)",
  "180" = "1980 U.S. Std Million (18 age groups)",
  "190" = "1990 U.S. Std Million (18 age groups)",
  "200" = "2000 U.S. Std Million (18 age groups)",
  "204" = "2000 U.S. Std Population (18 age groups - Census P25-1130)"
)

seer_tibble <- function(x) {
  x %>%
    str_match("(\\d{3})(\\d{3})(\\d{8})") %>%
    purrr::array_tree(margin = 2) %>%
    set_names(c("raw", "standard", "age", "std_pop")) %>%
    as_tibble()
}

age_groups_18 <- map_chr(0:18, age_18_groups_by_index)
names(age_groups_18) <- sprintf("%03d", 0:18)

seer_std_age_18 <-
  seer_std_age_18 %>%
  seer_tibble() %>%
  select(-raw) %>%
  mutate(
    standard = seer_standard_dictionary[standard],
    age_group = age_groups_18[age],
    std_pop = as.numeric(std_pop)
  )


seer_std_ages <-
  seer_std_ages %>%
  seer_tibble() %>%
  select(-raw) %>%
  mutate(
    standard_name = seer_standard_dictionary[standard],
    age = as.integer(age),
    std_pop = as.numeric(std_pop),
    age_group = sprintf("%03d", age %/% 5 + 1),
    age_group = age_groups_18[age_group]
  ) %>%
  group_by(standard, standard_name, age_group) %>%
  summarize(std_pop = sum(std_pop))

seer_std_ages %>%
  filter(standard == "202") %>%
  ungroup() %>%
  saveRDS(here::here("data", "seer_std_ages.rds"))
