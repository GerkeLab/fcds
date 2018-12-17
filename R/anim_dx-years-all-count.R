library(tidyverse)
library(here)
library(gganimate)

dat <- readRDS(here::here("data", "stat_dataset_2018_clean.rds"))
source(here::here("R", "census_load-data.R"))

ga <-
  dat %>%
  count(dx_date, dx_county) %>%
  complete(dx_date, dx_county, fill = list(n = 0)) %>%
  # filter(dx_date %in% sort(unique(dx_date))[1:2]) %>%
  left_join(florida_counties %>% select(geoid, name, geometry), by = c("dx_county" = "name")) %>%
  left_join(fl_acs5_county %>% filter(variable == "B01001_001") %>% select(geoid, pop_est = estimate),
            by = "geoid") %>%
  mutate(n = n/pop_est) %>%
  ggplot() +
  aes(fill = n) +
  geom_sf(color = "grey20", size = 0.25) +
  scale_fill_viridis_c() +
  coord_sf(datum = NA) +
  labs(fill = NULL) +
  theme_minimal() +
  transition_states(dx_date, 1, 1) +
  ggtitle("Total Cancer Diagnoses\n{closest_state}")

anim_save(here::here("out", "dx_year_total.mov"),
          animate(ga, renderer = av_renderer(),
                  width = 1200, height = 1200))
#
#
# moffitt_counties <- c(
#   "Charlotte", "Citrus", "De Soto", "Hardee", "Hernando", "Highlands",
#   "Hillsborough", "Lake", "Lee", "Manatee", "Pasco", "Pinellas",
#   "Polk", "Sarasota", "Sumter"
# )
# non_moffitt_counties <- setdiff(florida_counties$name, moffitt_counties)
# county_colors <- setNames(
#   scales::hue_pal()(length(moffitt_counties)),
#   moffitt_counties
# )
# county_colors <- c(county_colors,
#                    setNames(rep("#a0a0a0", length(non_moffitt_counties)),
#                             non_moffitt_counties))
#
# dat %>%
#   count(dx_date, dx_county) %>%
#   complete(dx_date, dx_county, fill = list(n = 0)) %>%
#   # filter(dx_date %in% sort(unique(dx_date))[1:2]) %>%
#   left_join(florida_counties %>% select(geoid, name, geometry), by = c("dx_county" = "name")) %>%
#   left_join(fl_acs5_county %>% filter(variable == "B01001_001") %>% select(geoid, pop_est = estimate),
#             by = "geoid") %>%
#   mutate(n = n/pop_est) %>%
#   ggplot() +
#   aes(dx_date, n, color = dx_county, group = dx_county, size = dx_county %in% moffitt_counties) +
#   geom_line() +
#   theme_minimal() +
#   scale_color_manual(values = county_colors) +
#   scale_size_discrete(range = c(0.75, 1.5))
