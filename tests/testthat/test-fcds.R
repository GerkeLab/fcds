
# fcds_vars() -------------------------------------------------------------

test_that("fcds_vars()", {
  expect_equal(
    fcds_vars("id"),
    c("patient_id", "year_group", "year")
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

test_that("fcds_vars() without argument returns options", {
  expect_message(fcds_vars(), "groups include")
  expect_true("id" %in% fcds_vars())
})


# count_fcds() ------------------------------------------------------------

describe("count_fcds()", {
  r_count_fcds <- fcds::fcds_example %>%
    group_by(county_name) %>%
    count_fcds(sex = "Male", race = "White", origin = "Non-Hispanic")

  it("generally works", {
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
    expect_known_hash(r_count_fcds %>% dplyr::ungroup(), "c4a416ff9a")
  })

  it("adds filtered variables to grouping", {
    expect_equal(
      dplyr::group_vars(r_count_fcds),
      c("county_name", "sex", "race", "origin", "year_group", "year", "age_group")
    )
  })

  it("generally works with the defaults", {
    r_count_fcds_default <- fcds::fcds_example %>%
      count_fcds()

    expect_equal(
      dplyr::group_vars(r_count_fcds_default),
      c("year_group", "year", "age_group")
    )
    expect_known_hash(r_count_fcds_default %>% dplyr::ungroup(), "589260aeb9")
  })

  it("subsets to Moffitt counties", {
    r_count_fcds_moffitt <- fcds::fcds_example %>%
      count_fcds(county_name = "moffitt_catchment") %>%
      dplyr::ungroup()

    r_count_fcds_moffitt2 <- fcds::fcds_example %>%
      count_fcds(county_name = "Moffitt Cancer Center") %>%
      dplyr::ungroup()

    expect_known_hash(r_count_fcds_moffitt, "f60c484640")
    expect_known_hash(r_count_fcds_moffitt2, "f60c484640")
  })

  it("moffitt_catchment is deprecated", {
    expect_warning(
      fcds::fcds_example %>% count_fcds(moffitt_catchment = TRUE),
      "deprecated"
    )

    expect_identical(
      suppressWarnings(fcds::fcds_example %>% count_fcds(moffitt_catchment = TRUE)),
      fcds::fcds_example %>% count_fcds(county_name = "moffitt_catchment")
    )
  })

  it("county_name = TRUE includes county_name in group vars", {
    fcds_county <- fcds::fcds_example %>% count_fcds(county_name = TRUE)
    expect_identical(
      dplyr::group_vars(fcds_county),
      c("county_name", "year_group", "year", "age_group")
    )

    expect_identical(
      fcds_county %>% .$county_name %>% paste() %>% unique() %>% sort(),
      fcds::fcds_example %>% .$county_name %>% paste() %>% unique() %>% sort()
    )
  })

  it("errors when invalid FCDS constants are provided", {
    expect_error(
      fcds::fcds_example %>% count_fcds(race = "Banana")
    )
    expect_error(
      fcds::fcds_example %>% count_fcds(origin = "Not Hispanic")
    )
  })

  it("adds year (year_midpoint) if needed", {
    # Check that `year` (year midpoint) is calculated if needed.
    # Coercion to data.frame is used to drop var_labels that are present in
    # the example processed FCDS data.
    expect_equivalent(
      fcds::fcds_example %>%
        select(-year) %>%
        count_fcds() %>%
        as.data.frame(),
      fcds::fcds_example %>% count_fcds() %>% as.data.frame()
    )
  })

  it("lets additional columns be included in the count (and groups) via ...", {
    r_cf <- fcds::fcds_example %>%
      count_fcds(cancer_status) %>%
      dplyr::arrange(cancer_status, year, age_group)

    e_cf <- fcds::fcds_example %>%
      dplyr::group_by(year_group, year, age_group, cancer_status) %>%
      dplyr::count() %>%
      dplyr::arrange(cancer_status, year, age_group)

    expect_setequal(groups(r_cf), groups(e_cf))
    expect_equal(r_cf[colnames(e_cf)] %>% dplyr::ungroup(), e_cf %>% dplyr::ungroup())
  })

  it("removes un-observed factor levels in output groups", {
    r_cfl <- fcds::fcds_example %>%
      filter(year > 2000) %>%
      count_fcds(county_name = "Moffitt", sex = "Male")

    expect_true(
      length(setdiff(levels(r_cfl$county_name), fcds_const("moffitt_catchment"))) == 0
    )

    expect_equal(levels(r_cfl$sex), "Male")

    expect_equal(levels(r_cfl$year_group), c("2002-2006", "2007-2011", "2012-2016"))
  })

  it("has shortcuts for {sex,origin,race} = TRUE", {
    expect_equal(
      fcds::fcds_example %>% count_fcds(sex = TRUE),
      fcds::fcds_example %>% group_by(sex) %>% count_fcds()
    )

    expect_equal(
      fcds::fcds_example %>% count_fcds(origin = TRUE),
      fcds::fcds_example %>% group_by(origin) %>% count_fcds()
    )

    expect_equal(
      fcds::fcds_example %>% count_fcds(race = TRUE),
      fcds::fcds_example %>% group_by(race) %>% count_fcds()
    )

    expect_error(count_fcds(fcds::fcds_example, race = FALSE))
  })

  it("discard_unseen_levels appropriately", {
    expect_equal(
      fcds::fcds_example %>%
        count_fcds(sex = "Male") %>%
        dplyr::pull(sex) %>% levels(),
      "Male"
    )
    expect_equal(
      fcds::fcds_example %>%
        count_fcds(sex = "Male", discard_unseen_levels = "sex") %>%
        dplyr::pull(sex) %>% levels(),
      "Male"
    )
    expect_equal(
      fcds::fcds_example %>%
        count_fcds(sex = "Male", discard_unseen_levels = FALSE) %>%
        dplyr::pull(sex) %>% levels(),
      c("Male", "Female", "Unknown")
    )
    expect_equal(
      fcds::fcds_example %>%
        count_fcds(sex = "Male", discard_unseen_levels = "origin") %>%
        dplyr::pull(sex) %>% levels(),
      c("Male", "Female", "Unknown")
    )
  })
})

test_that("filter_fcds()", {
  r_filter_fcds <- fcds::fcds_example %>%
    filter_fcds("county_name", c("Pinellas", "Hillsborough"))

  expect_equal(
    r_filter_fcds$county_name %>% paste() %>% unique() %>% sort(),
    c("Hillsborough", "Pinellas")
  )

  expect_error(
    fcds::fcds_example %>% filter_fcds("taco", "soft")
  )
  expect_error(
    fcds::fcds_example %>% filter_fcds("county_name", "taco")
  )

  expect_equal(
    fcds::fcds_example %>% filter_fcds("county_name", NULL),
    fcds::fcds_example
  )
})


test_that("join_population_by_year() handles by_year edge cases", {
  expect_error(join_population_by_year(fcds::fcds_example, by_year = c("year1", "year2")))
  expect_error(join_population_by_year(fcds::fcds_example, by_year = 2015))

  overlapping_columns <- common_names(fcds::fcds_example, fcds::seer_pop_fl)
  overlapping_columns <- setdiff(overlapping_columns, "origin")

  sub_fcds_example <- fcds::fcds_example[1:10, overlapping_columns]

  r_joined_pop <-
    suppressWarnings(join_population_by_year(
      sub_fcds_example %>% dplyr::rename(year_mid = year),
      fcds::seer_pop_fl %>% dplyr::rename(year_mid = year),
      by_year = "year_mid"
    )) %>%
    dplyr::rename(year = year_mid)

  expect_known_hash(r_joined_pop, "06fc101af1")
  expect_known_hash(
    suppressWarnings(join_population_by_year(sub_fcds_example)),
    "06fc101af1"
  )
})
