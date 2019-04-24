library(fcds)
library(dplyr)
library(purrr)
library(tidyr)

set.seed(20190424)
fcds <- fcds_load()

fcds_example <-
  fcds %>%
  filter(county_name %in% fcds_const("moffitt_catchment")) %>%
  group_by(year, year_mid, sex, race, hispanic, county_name) %>%
  mutate(i = sample(row_number())) %>%
  filter(i <= 5) %>%
  ungroup() %>%
  select(-i) %>%
  arrange(year_mid, age_group, county_name, cancer_ICDO3_morphology) %>%
  mutate(patient_id = row_number() + 10000)

shuffle_groups <- c("demographics", "cancer", "population", "seer", "tobacco")

for (group in shuffle_groups) {
  # scramble rows among the given columns
  fcds_example[, fcds_vars(group)] <-
    fcds_vars(group, .data = fcds_example) %>%
    sample_frac()
}

n_example <- nrow(fcds_example)

# make sure no comparable "real" cases remain
fcds_safe <- anti_join(fcds_example, fcds[, -1])

n_diff <- n_example - nrow(fcds_safe)
if (n_diff) {
  fcds_example <- fcds_example %>% filter(patient_id %in% fcds_safe$patient_id)
  message("Removed ", n_diff, " rows with similar data in full dataset")
}

usethis::use_data(fcds_example, overwrite = TRUE, compress = "gzip")
