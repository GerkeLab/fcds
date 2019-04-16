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
