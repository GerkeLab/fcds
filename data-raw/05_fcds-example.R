library(dplyr)
library(purrr)
library(tidyr)

set.seed(20190424)
fcds <- fcds::fcds_load()

fcds_example <-
  fcds %>%
  filter(county_name %in% fcds::fcds_const("moffitt_catchment")) %>%
  group_by(year_group, year, sex, race, origin, county_name) %>%
  mutate(i = sample(row_number())) %>%
  filter(i <= 5) %>%
  ungroup() %>%
  select(-patient_id) %>%
  mutate(i = row_number())

shuffle_groups <- c("demographics", "cancer", "population", "seer", "tobacco")

for (group in shuffle_groups) {
  # scramble rows among the given columns
  fcds_example[, fcds::fcds_vars(group)] <-
    fcds_example %>%
    select(fcds::fcds_vars(group)) %>%
    sample_frac()
}

n_example <- nrow(fcds_example)

# make sure no comparable "real" cases remain
fcds_safe <- suppressWarnings(anti_join(fcds_example, fcds[, -1]))

n_diff <- n_example - nrow(fcds_safe)
if (n_diff) {
  fcds_example <- fcds_example %>% filter(i %in% fcds_safe$i)
  message("Removed ", n_diff, " rows with similar data in full dataset")
}

fcds_example <-
  fcds_example %>%
  arrange(year, age_group, county_name, cancer_ICDO3_morphology) %>%
  mutate(patient_id = row_number() + 10000) %>%
  select(patient_id, everything(), -i)

usethis::use_data(fcds_example, overwrite = TRUE, compress = "gzip")
