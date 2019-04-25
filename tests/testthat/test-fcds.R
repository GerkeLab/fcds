
# fcds_vars() -------------------------------------------------------------

test_that("fcds_vars()", {
  expect_equal(
    fcds_vars("id"),
    c("patient_id", "year", "year_mid")
  )
  expect_equal(
    fcds_vars("pop"),
    fcds_var_group("population")
  )
  expect_equal(
    fcds_vars("demo"),
    fcds_var_group("demographics")
  )
  expect_equal(
    fcds_vars("cancer", "icd"),
    fcds_var_group("cancer")
  )
  expect_equal(
    fcds_vars(c("cancer", "icd")),
    fcds_var_group("cancer")
  )
  expect_true(
    all(grepl("^seer",
              fcds_vars("seer")))
  )
  expect_true(
    all(grepl("^tobacco", fcds_vars("tob")))
  )
  expect_error(
    fcds_vars("pop", "wrong"),
    "wrong"
  )
})

test_that("fcds_vars() with a data frame", {
  set.seed(421321)
  fcds_data <- tibble(
    patient_id = 1:5,
    year = 2000,
    county_name = "Pinellas",
    age_group = sample(fcds_const("age_group"), 5)
  )

  r_fcds_vars <- fcds_vars(.data = fcds_data, "id", "pop", "demo")

  expect_equal(names(r_fcds_vars), names(fcds_data))
})


# count_fcds() ------------------------------------------------------------

test_that("count_fcds()", {
  r_count_fcds <- fcds::fcds_example %>%
    group_by(county_name) %>%
    count_fcds(sex = "Male", race = "White", origin = "Non-Hispanic")

  expect_equal(
    unique(r_count_fcds$sex) %>% paste(),
    "Male"
  )
  expect_equal(
    unique(r_count_fcds$race) %>% paste(),
    "White"
  )
  expect_equal(
    unique(r_count_fcds$origin) %>% paste(),
    "Non-Hispanic"
  )
  expect_equal(
    dplyr::group_vars(r_count_fcds),
    c("county_name", "sex", "race", "origin", "year", "year_mid", "age_group")
  )

  expect_known_hash(r_count_fcds, "30fc9b78e1")

  r_count_fcds_default <- fcds::fcds_example %>%
    count_fcds()

  expect_equal(
    dplyr::group_vars(r_count_fcds_default),
    c("year", "year_mid", "age_group")
  )
  expect_known_hash(r_count_fcds_default, "5fe92053d0")

  r_count_fcds_moffitt <- fcds::fcds_example %>%
    count_fcds(moffitt_catchment = TRUE)
  expect_known_hash(r_count_fcds_moffitt, "d146051312")

  expect_error(
    fcds::fcds_example %>% count_fcds(race = "Banana")
  )
  expect_error(
    fcds::fcds_example %>% count_fcds(origin = "Not Hispanic")
  )

  # Check that `year_mid` is calculated if needed.
  # Coercion to data.frame is used to drop var_labels that are present in
  # the example processed FCDS data.
  expect_equivalent(
    fcds::fcds_example %>%
      select(-year_mid) %>%
      count_fcds() %>%
      as.data.frame(),
    fcds::fcds_example %>% count_fcds() %>% as.data.frame()
  )
})
