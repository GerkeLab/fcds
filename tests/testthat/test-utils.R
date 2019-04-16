
# group_drop() ------------------------------------------------------------

test_that("group_drop(): Groups are dropped from grouping", {
  expect_equal(
    tidyr::table2 %>% group_by(country, year, type) %>% group_drop(type),
    tidyr::table2 %>% group_by(country, year)
  )
})

test_that("group_drop(): Group names can be unquoted or strings", {
  expect_equal(
    tidyr::table2 %>% group_by(country, year, type) %>% group_drop(type, "year"),
    tidyr::table2 %>% group_by(country, year, type) %>% group_drop("type", year)
  )
})

test_that("group_drop(): Group columns are removed from the output, if requested and dropped", {
  expect_equal(
    tidyr::table2 %>% group_by(country, year, type) %>%
      group_drop(type, .remove_dropped = TRUE),
    tidyr::table2 %>% group_by(country, year) %>% select(-type)
  )
})

test_that("group_drop(): Only actually dropped groups are removed (year not in groups)", {
  expect_equal(
    tidyr::table2 %>% group_by(country, type) %>%
      group_drop(year, type, .remove_dropped = TRUE),
    tidyr::table2 %>% group_by(country) %>% select(-type)
  )
})

test_that("group_drop(): Doesn't do anything if requested variable isn't in the groups", {
  expect_equal(
    tidyr::table2 %>% group_by(country, year) %>% group_drop(type),
    tidyr::table2 %>% group_by(country, year)
  )
})

test_that("group_drop(): Doesn't do anything if there are no groups defined", {
  expect_equal(tidyr::table2 %>% group_drop(country), tidyr::table2)

  expect_equal(data.frame(a = 1) %>% group_drop(a), data.frame(a = 1))
})


# with_ungroup() ----------------------------------------------------------

test_that("with_ungroup() computes and restores groups", {
  expect_equal(
    tidyr::table1 %>% group_by(country, year) %>%
      with_ungroup(~ mutate(., r = cases/population)),
    tidyr::table1 %>% mutate(r = cases/population) %>% group_by(country, year)
  )
})

test_that("with_ungroup() is a normal function if no groups", {
  expect_equal(
    tidyr::table1 %>% with_ungroup(~ mutate(., r = cases/population)),
    tidyr::table1 %>% mutate(r = cases/population)
  )
})

test_that("with_ungroup() implicitly drops groups with a warning", {
  expect_warning(
    tidyr::table1 %>% group_by(country, year) %>%
      with_ungroup(~ mutate(., r = cases/population) %>% select(-year)),
    "implicitly dropped.+year"
  )
})


# with_retain_groups() ----------------------------------------------------

test_that("with_retain_groups() restores groups after computation", {
  expect_equal(
    tidyr::table1 %>% group_by(country, year) %>%
      with_retain_groups(~ dplyr::summarize(., cases = sum(cases))),
    tidyr::table1 %>% group_by(country, year) %>%
      dplyr::summarize(cases = sum(cases)) %>% group_by(year, add = TRUE)
  )

  expect_equal(
    tidyr::table1 %>% group_by(country, year) %>%
      with_retain_groups(~ dplyr::ungroup(.) %>% dplyr::slice(1)),
    tidyr::table1 %>% dplyr::slice(1) %>% group_by(country, year)
  )
})

test_that("with_retain_groups() is a normal function if no groups", {
  expect_equal(
    tidyr::table1 %>% with_retain_groups(~ mutate(., r = cases/population)),
    tidyr::table1 %>% mutate(r = cases/population)
  )
})

test_that("with_retain_groups() implicitly drops groups with a warning", {
  expect_warning(
    tidyr::table1 %>% group_by(country, year) %>%
      with_retain_groups(~ {
          dplyr::summarize(., r = cases/population) %>%
          dplyr::summarize(r = mean(r))
      }),
    "implicitly dropped.+year"
  )
})


# Various Utilities -------------------------------------------------------

test_that("validate_all_have_var", {
  data1 <- tibble(a = 1, b = 2, c = 3)
  data2 <- tibble(b = 2, c = 3, d = 4)
  data3 <- tibble(c = 3, d = 4, e = 5)
  expect_error(
    validate_all_have_var("b", data1 = data1, data2 = data2, data3 = data3),
    "data3"
  )

  expect_error(
    validate_all_have_var("e", data1 = data1, data2 = data2, data3 = data3),
    "data1.+data2"
  )

  expect_null(
    validate_all_have_var("c", data1 = data1, data2 = data2, data3 = data3)
  )
})
