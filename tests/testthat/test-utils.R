
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

describe("with_ungroup()", {

  it("computes and restores groups", {
    expect_equal(
      tidyr::table1 %>% group_by(country, year) %>%
        with_ungroup(~ mutate(., r = cases/population)),
      tidyr::table1 %>% mutate(r = cases/population) %>% group_by(country, year)
    )
  })

  it("is a normal function if no groups", {
    expect_equal(
      tidyr::table1 %>% with_ungroup(~ mutate(., r = cases/population)),
      tidyr::table1 %>% mutate(r = cases/population)
    )
  })

  it("implicitly drops groups with a warning", {
    expect_warning(
      tidyr::table1 %>% group_by(country, year) %>%
        with_ungroup(~ mutate(., r = cases/population) %>% select(-year)),
      "implicitly dropped.+year"
    )
  })

  it("retains correct grouping of rows", {
    e_county_count <- fcds::fcds_example %>%
      filter(sex == "Male", race == "White", origin == "Non-Hispanic") %>%
      group_by(county_name) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(county_name = factor(county_name, levels = sort(fcds_const("moffitt"))))

    r_county_count <- fcds::fcds_example %>%
      group_by(county_name) %>%
      count_fcds(sex = "Male", race = "White", origin = "Non-Hispanic") %>%
      dplyr::summarize(n = sum(n)) %>% dplyr::summarize(n = sum(n)) %>%
      dplyr::summarize(n = sum(n)) %>% dplyr::summarize(n = sum(n)) %>%
      dplyr::summarize(n = sum(n)) %>% dplyr::summarize(n = sum(n)) %>%
      dplyr::summarize(n = sum(n))

    expect_equal(r_county_count, e_county_count)
  })
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

  expect_true(
    all(validate_all_have_var("c", data1 = data1, data2 = data2, data3 = data3))
  )

  expect_equal(
    attr(validate_all_have_var("b", data3 = data3, .abort = FALSE),
         "msg_missing"),
    "'b' is missing from `data3`"
  )
})

test_that("check for packages", {
  not_a_pkg <- "aqswdefrgthy"

  expect_error(requires_package(not_a_pkg), "required.+fcds")
  expect_error(requires_package(not_a_pkg, "test"), "required.+test")
  expect_warning(suggests_package(not_a_pkg), "suggested.+fcds")
  expect_warning(suggests_package(not_a_pkg, "test"), "suggested.+test")

  expect_false(suppressWarnings(suggests_package(not_a_pkg)))
  expect_true(requires_package("dplyr"))
  expect_true(suggests_package("dplyr"))

  expect_equal(
    suppressWarnings(suggests_package(c("dplyr", not_a_pkg))),
    set_names(c(TRUE, FALSE), c("dplyr", not_a_pkg))
  )
})

test_that("and_more()", {
  expect_equal(and_more(letters[1:5]), "'a', 'b', and 3 more...")
  expect_equal(and_more(letters[1:3]), "'a', 'b', and 'c'")
})

test_that("seq2()", {
  x <- c(1, 10, NA, 30.12)
  y <- c(10, 19, 29, 39.99)

  z <- seq2(x, y)
  expect_equal(z[[1]], x[1]:y[1])
  expect_equal(z[[2]], x[2]:y[2])
  expect_equal(z[[4]], 30:39)
  expect_equal(z[[3]], NA_integer_)
})

test_that("string helpers", {
  x <- letters[1:3]
  expect_equal(collapse(x), "a, b, c")
  expect_equal(collapse(x, last = ", and "), "a, b, and c")

  expect_equal(backtick(x), "`a`, `b`, `c`")
  expect_equal(backtick(x, sep = ""), c("`a`", "`b`", "`c`"))
  expect_equal(backtick(x, last = ", and "), "`a`, `b`, and `c`")

  expect_equal(single_quote(x), "'a', 'b', 'c'")
  expect_equal(double_quote(x), '"a", "b", "c"')
  expect_equal(single_quote(x, sep = ""), c("'a'", "'b'", "'c'"))
  expect_equal(double_quote(x, sep = ""), c('"a"', '"b"', '"c"'))
})

test_that("to_snake_case()", {
  expect_equal(to_snake_case("A---Really really Very.;'Bad string"),
               "a_really_really_very_bad_string")
})
