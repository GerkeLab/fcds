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
