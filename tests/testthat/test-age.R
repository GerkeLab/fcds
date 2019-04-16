d_age_group <- data.frame(
  id = 1:4,
  age_group = c("0 - 4", "10-14", "65-69", "85+")
)


# Age Helpers -------------------------------------------------------------


test_that("expand_age_groups()", {
  r_age_group <- expand_age_groups(d_age_group)

  expect_equal(names(r_age_group), c("id", paste0("age_", c("group", "low", "high"))))
  expect_equal(r_age_group$age_low, c(0, 10, 65, 85))
  expect_equal(r_age_group$age_high, c(4, 14, 69, Inf))

  names(d_age_group)[2] <- "age_variable"
  expect_equal(
    d_age_group %>% expand_age_groups(age_var = age_variable) %>% names(),
    c("id", paste0("age_", c("variable", "low", "high")))
  )
})

test_that("expanding NA age group gives NA", {
  d_age_group$age_group[2] <- NA_character_

  r_age_group <- expand_age_groups(d_age_group)

  expect_equal(r_age_group$age_low[2], NA_real_)
  expect_equal(r_age_group$age_high[2], NA_real_)
})

test_that("expand_age_groups() allows renaming age_low and age_high", {
  r_age_group <- d_age_group %>%
    expand_age_groups(age_low_var = low, age_high_var = high)

  r_age_group_default <- d_age_group %>% expand_age_groups()

  expect_equal(names(r_age_group), c("id", "age_group", "low", "high"))
  expect_equal(r_age_group$low, r_age_group_default$age_low)
  names(r_age_group)[3:4] <- paste0("age_", names(r_age_group)[3:4])
  expect_equal(r_age_group, r_age_group_default)
})

test_that("filter_age()", {
  expect_equal(
    d_age_group %>% filter_age(age_low = 5) %>% .$id,
    2:4
  )

  expect_equal(
    # 65 - 69 not included if age_high = 66
    d_age_group %>% filter_age(age_high = 66) %>% .$id,
    1:2
  )

  expect_equal(d_age_group %>% filter_age(10, 14) %>% .$id, 2)
  expect_equal(
    d_age_group %>% filter_age(),
    d_age_group %>% expand_age_groups()
  )
})

test_that("complete_age_groups()", {
  d_age_group <- tibble(
    age_group = c("10 - 14", "15 - 19", "25 - 29"),
    n = 10:12
  )

  r_age_group <- d_age_group %>%
    complete_age_groups(10, 35)

  expect_equal(
    paste(r_age_group$age_group),
    paste(seq(10, 30, 5), "-", seq(14, 34, 5))
  )
  expect_equal(r_age_group$n, c(10, 11, 0, 12, 0))
})

test_that("standardize_age_groups()", {
  # normal
  expected_age_group <- d_age_group %>%
    expand_age_groups() %>%
    mutate(
      age_group_low = if_else(age_low < 0, "0", paste(age_low)),
      age_group_high = if_else(age_high > 0 & is.infinite(age_high), "+", paste(" -", age_high)),
      age_group = paste0(age_group_low, age_group_high),
      age_group = factor(age_group, fcds_const("age_group"), ordered = TRUE)
    ) %>%
    select(-dplyr::matches("(low|high)$"))

  stdized_age_group <- d_age_group %>% standardize_age_groups()

  expect_equal(stdized_age_group, expected_age_group)
  expect_equal(levels(stdized_age_group$age_group), fcds_const("age_group"))
})

test_that("standardize_age_groups() with non-standard age groups", {

  expect_error(
    data.frame(age_group = "3 - 6") %>% standardize_age_groups(),
    "not consistent with `std_age_groups`"
  )
  expect_equal(
    data.frame(age_group = "3 - 6") %>%
      standardize_age_groups(std_age_groups = "3 - 6"),
    data.frame(age_group = factor("3 - 6", ordered = TRUE))
  )
})

test_that("standardize_age_groups() with numeric-ish age inputs", {
  numeric_char_age <- tibble(age = c("1.245", "7.5", "11.3"))
  r_numeric_char_age <- numeric_char_age %>% standardize_age_groups(age)

  # standardize_age_groups doesn't overwrite columns unless age_group
  expect_equal(r_numeric_char_age$age, numeric_char_age$age)
  expect_equal(paste(r_numeric_char_age$age_group),
               paste(seq(0, 10, 5), "-", seq(4, 14, 5)))
})

test_that("standardize_age_groups() returns columns in same order", {
  t_age_group <- tibble(col1 = 1, age_group = "0 - 4", col2 = 2)
  expect_equal(
     t_age_group %>% standardize_age_groups() %>% names(),
     names(t_age_group)
  )
  expect_equal(
    t_age_group[, c(2, 1, 3)] %>% standardize_age_groups() %>% names(),
    names(t_age_group)[c(2, 1, 3)]
  )
})


# Age Adjustment ----------------------------------------------------------


# Age Adjustment Example Data ----
# Example modified from https://seer.cancer.gov/seerstat/tutorials/aarates/
d_incidence <- tibble::tribble(
  ~age_group,   ~n,
     "0 - 4",  116,
     "5 - 9",   67,
   "10 - 14",   71,
   "15 - 19",   87,
   "20 - 24",  177,
   "25 - 29",  290,
   "30 - 34",  657,
   "35 - 39", 1072,
   "40 - 44", 1691,
   "45 - 49", 2428,
   "50 - 54", 2931,
   "55 - 59", 2881,
   "60 - 64", 2817,
   "65 - 69", 2817,
   "70 - 74", 2744,
   "75 - 79", 2634,
   "80 - 84", 1884,
       "85+", 1705
) %>%
  fcds::standardize_age_groups()

d_population <- tibble::tribble(
  ~age_group, ~population, ~crude_rate,
     "0 - 4",      693068,        16.7,
     "5 - 9",      736212,         9.1,
   "10 - 14",      770999,         9.2,
   "15 - 19",      651390,        13.4,
   "20 - 24",      639159,        27.7,
   "25 - 29",      676354,        42.9,
   "30 - 34",      736557,        89.2,
   "35 - 39",      724826,       147.9,
   "40 - 44",      700200,       241.5,
   "45 - 49",      617437,       393.2,
   "50 - 54",      516541,       567.4,
   "55 - 59",      361170,       797.7,
   "60 - 64",      259440,      1085.8,
   "65 - 69",      206204,      1366.1,
   "70 - 74",      172087,      1594.5,
   "75 - 79",      142958,      1842.5,
   "80 - 84",       99654,      1890.5,
       "85+",       92692,      1839.4
  ) %>%
  fcds::standardize_age_groups()

d_std <- tibble::tribble(
  ~age_group,  ~std_pop, ~age_dist, ~rate,
     "0 - 4",  18986520,  0.069134,  1.16,
     "5 - 9",  19919840,  0.072532,  0.66,
   "10 - 14",  20056779,  0.073031,  0.67,
   "15 - 19",  19819518,  0.072167,  0.96,
   "20 - 24",  18257225,  0.066478,  1.84,
   "25 - 29",  17722067,   0.06453,  2.77,
   "30 - 34",  19511370,  0.071045,  6.34,
   "35 - 39",  22179956,  0.080762, 11.94,
   "40 - 44",  22479229,  0.081852, 19.77,
   "45 - 49",  19805793,  0.072117, 28.36,
   "50 - 54",  17224359,  0.062718, 35.59,
   "55 - 59",  13307234,  0.048454, 38.65,
   "60 - 64",  10654272,  0.038794, 42.12,
   "65 - 69",   9409940,  0.034264, 46.81,
   "70 - 74",   8725574,  0.031772, 50.66,
   "75 - 79",   7414559,  0.026998, 49.74,
   "80 - 84",   4900234,  0.017843, 33.73,
       "85+",   4259173,  0.015509, 28.53
  ) %>%
  fcds::standardize_age_groups()

d_answer <- tibble::tribble(
  ~age_group,  ~std_pop, ~age_dist, ~rate,
  "All Ages", 274633642,         1, 400.3
)

# Age Adjustment Tests ----

# Needs Tests
#
# * data should be at least grouped by {age}
# * what if data is grouped by year?

test_that("age_adjust()", {
  r_age_adjusted <- age_adjust(d_incidence, population = d_population, by_year = NULL)

  expect_equal(round(r_age_adjusted$rate, 1), d_answer$rate)
  expect_equal(r_age_adjusted$population, sum(d_population$population))
  expect_equal(r_age_adjusted$n, sum(d_incidence$n))

  expect_error(
    age_adjust(d_incidence,
               by_year = NULL,
               population = d_population %>% dplyr::rename(pop = population)),
    "population.+is missing"
  )
})
