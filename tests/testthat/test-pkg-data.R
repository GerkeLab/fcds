context("data checks")

test_that("population data has years matching fcds mid years", {
  seer_pop_fl_years <- unique(seer_pop_fl$year)
  seer_pop_fl_1990_years <- unique(seer_pop_fl_1990$year)
  seer_pop_us_years <- unique(seer_pop_us$year)

  fcds_mid_years <- mid_year(fcds_const("year_group"))

  expect_setequal(seer_pop_fl_years, fcds_mid_years)
  expect_setequal(seer_pop_fl_1990_years, fcds_mid_years[fcds_mid_years >= "1990"])
  expect_setequal(seer_pop_us_years, fcds_mid_years[fcds_mid_years >= "1990"])
})

test_that("county names match", {
  expect_setequal(county_fips_fl$county_name, fcds_const("county_name"))
})

test_that("population data didn't change", {
  expect_known_hash(seer_pop_fl, "35b1ad25ae")
  expect_known_hash(seer_pop_fl_1990, "2dea1c21c4")
  expect_known_hash(seer_pop_us, "3caa057477")
})

test_that("icdo3 data didn't change", {
  expect_known_hash(seer_icd_o_3, "6003bfea82")
  expect_known_hash(iacr_icd_o_3, "274b06c5e3")
})
