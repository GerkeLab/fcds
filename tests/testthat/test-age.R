
# Age Helpers -------------------------------------------------------------

d_age_group <- dplyr::tibble(
  id = 1:4,
  age_group = c("0 - 4", "10-14", "65-69", "85+")
)

# ---- |> format_age_groups() ----
describe("format_age_groups()", {
  it("consistently formats age groups", {
    d_fmt_age_group <- dplyr::tibble(
      age_min = c(-Inf, 20, 85),
      age_max = c(19, 84, Inf)
    )
    expect_equal(
      format_age_groups(d_fmt_age_group)$age_group,
      c("0 - 19", "20 - 84", "85+")
    )

    expect_equal(
      format_age_groups(d_fmt_age_group, into = "ag")$ag,
      c("0 - 19", "20 - 84", "85+")
    )
  })

  it("round-trips with separate_age_groups()", {
    d_age_group_init <- dplyr::tibble(
      age_min = c(0, 20, 21),
      age_max = c(19, 20, Inf)
    ) %>%
      format_age_groups()
    d_age_group_round_trip <- d_age_group_init %>%
      separate_age_groups() %>% format_age_groups()

    cols_ordered <- colnames(d_age_group_init)
    expect_equivalent(d_age_group_round_trip[cols_ordered], d_age_group_init[cols_ordered])

    d_age_groups <- dplyr::tibble(
      age_group = fcds_const("age_group")
    ) %>%
      separate_age_groups()

    d_age_groups_round_trip <- d_age_groups %>%
      format_age_groups() %>% separate_age_groups()

    cols_ordered <- colnames(d_age_groups)
    expect_equivalent(d_age_groups_round_trip[cols_ordered], d_age_groups[cols_ordered])
  })

  it("replaces missing on either side with missing_age_group", {
    d_age_group_missing <- dplyr::tibble(
      age_min = c(NA, 20, 25),
      age_max = c(19, NA, 29)
    )
    expect_equal(
      format_age_groups(d_age_group_missing)$age_group,
      c("Unknown", "Unknown", "25 - 29")
    )
  })

  it("rounds silently", {
    d_fmt_age_group_numeric <- dplyr::tibble(
      age_min = 15.25,
      age_max = 20.01
    )

    expect_equal(
      format_age_groups(d_fmt_age_group_numeric)$age_group,
      "15 - 20"
    )
  })

  it("warns if boundaries are less than zero", {
    d_fmt_age_group_zero <- dplyr::tibble(
      age_min = -1,
      age_max = -2
    )

    expect_warning(r_fmt_age_group_zero <- format_age_groups(d_fmt_age_group_zero))
    expect_equal(r_fmt_age_group_zero$age_group, "0")
  })

  it("errors if boundaries are equivalent", {
    d_fmt_age_group_equal <- dplyr::tibble(
      age_min = c(0, 20),
      age_max = c(20, 40)
    )

    expect_error(format_age_groups(d_fmt_age_group_equal))
  })

  it("throws errors if boundaries are infinitely wrong", {
    expect_error(
      dplyr::tibble(age_min = Inf, age_max = 20) %>% format_age_groups()
    )

    expect_error(
      dplyr::tibble(age_min = 20, age_max = -Inf) %>% format_age_groups()
    )
  })
})

# ---- |> separate_age_groups() ----
describe("separate_age_groups()", {

  it("works as expected in most generic case", {
    r_age_group <- separate_age_groups(d_age_group)

    expect_equal(names(r_age_group), c("id", paste0("age_", c("group", "min", "max"))))
    expect_equal(r_age_group$age_min, c(0, 10, 65, 85))
    expect_equal(r_age_group$age_max, c(4, 14, 69, Inf))
  })

  it("handles non-standard age_group column name and still returns age_min, age_max", {
    names(d_age_group)[2] <- "age_variable"
    expect_equal(
      d_age_group %>% separate_age_groups(age_group = age_variable) %>% names(),
      c("id", paste0("age_", c("variable", "min", "max")))
    )
  })

  it("expands NA age group into two NA values", {
    d_age_group$age_group[2] <- NA_character_

    r_age_group <- separate_age_groups(d_age_group)

    expect_equal(r_age_group$age_min[2], NA_real_)
    expect_equal(r_age_group$age_max[2], NA_real_)
  })

  it("allows renaming age_min and age_max", {
    r_age_group <- d_age_group %>%
      separate_age_groups(into = c("low", "high"))

    r_age_group_default <- d_age_group %>% separate_age_groups()

    expect_equal(names(r_age_group), c("id", "age_group", "low", "high"))
    expect_equal(r_age_group$low, r_age_group_default$age_min)

    names(r_age_group)[3:4] <- names(r_age_group_default)[3:4]
    expect_equal(r_age_group, r_age_group_default)
  })

  it("errors if invalid `into` value", {
    expect_error(separate_age_groups(d_age_groups, into = "low"))
    expect_error(separate_age_groups(d_age_groups, into = c("low", "low")))
  })
})

# ---- |> filter_age_groups() ----
test_that("filter_age_groups()", {
  expect_equal(
    d_age_group %>% filter_age_groups(age_gt = 5) %>% .$id,
    2:4
  )

  expect_equal(
    # 65 - 69 not included if age_max = 66
    d_age_group %>% filter_age_groups(age_lt = 66) %>% .$id,
    1:2
  )

  expect_equal(d_age_group %>% filter_age_groups(10, 14) %>% .$id, 2)
  expect_equal(
    d_age_group %>% filter_age_groups(),
    d_age_group %>% separate_age_groups() %>% select(-age_min, -age_max)
  )
})

test_that("filter_age_groups() doesn't add columns", {
  r_age_group_has <- d_age_group %>% filter_age_groups()
  r_age_group_doesnt <- d_age_group %>%
    separate_age_groups() %>% filter_age_groups()

  expect_equal(names(r_age_group_has), names(d_age_group))
  expect_equal(
    names(r_age_group_doesnt),
    c(names(d_age_group), paste0("age_", c("min", "max")))
  )
})

# ---- |> complete_age_groups() ----
describe("complete_age_groups()", {

  d_age_group <- tibble(
    age_group = c("10 - 14", "15 - 19", "25 - 29"),
    n = 10:12
  )

  it("fills missing age groups", {
    r_age_group <- d_age_group %>%
      complete_age_groups(age_gt = 10, age_lt = 35)

    expect_equal(
      paste(r_age_group$age_group),
      paste(seq(10, 30, 5), "-", seq(14, 34, 5))
    )
    expect_equal(r_age_group$n, c(10, 11, 0, 12, 0))
  })

  it("handles age_group being a grouping variable", {
    expect_equal(
      d_age_group %>% group_by(age_group) %>% complete_age_groups(),
      d_age_group %>% complete_age_groups() %>% group_by(age_group)
    )
  })

  it("has tidyr::complete() syntax", {
    e_age_group <- tidyr::crossing(
      sub_group = letters[1:3],
      group = LETTERS[1:3],
      age_group = fcds_const("age_group")[1:5]
    ) %>%
      standardize_age_groups(std_age_groups = fcds_const("age_group")[1:5]) %>%
      mutate_at(quos(sub_group, group), as.factor) %>%
      dplyr::arrange(sub_group, group, age_group)

    e_age_group$sub_group_equal <- purrr::map_int(e_age_group$sub_group, ~ which(letters == .))

    set.seed(421216)
    drop_many_group_c <- c(
      which(e_age_group$group != "C"),
      sample(which(e_age_group$group == "C"), 12)
    )

    d_age_group <- e_age_group[drop_many_group_c, ]

    expect_equal(
      d_age_group %>%
        complete_age_groups(tidyr::nesting(sub_group, sub_group_equal), group, age_lt = 25),
      e_age_group
    )
  })
})

# ---- |> standardize_age_groups() ----
test_that("standardize_age_groups()", {
  # normal
  expected_age_group <- d_age_group %>%
    separate_age_groups() %>%
    mutate(
      age_group_min = if_else(age_min < 0, "0", paste(age_min)),
      age_group_max = if_else(age_max > 0 & is.infinite(age_max), "+", paste(" -", age_max)),
      age_group = paste0(age_group_min, age_group_max),
      age_group = factor(age_group, fcds_const("age_group"), ordered = TRUE)
    ) %>%
    select(-dplyr::matches("(min|max)$"))

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
    dplyr::tibble(age_group = "3 - 6") %>%
      standardize_age_groups(std_age_groups = "3 - 6"),
    dplyr::tibble(age_group = factor("3 - 6", ordered = TRUE))
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

test_that("standardize_age_groups() when input is grouped", {
  # normal
  expected_age_group <- d_age_group %>%
    separate_age_groups() %>%
    mutate(
      age_group_min = if_else(age_min < 0, "0", paste(age_min)),
      age_group_max = if_else(age_max > 0 & is.infinite(age_max), "+", paste(" -", age_max)),
      age_group = paste0(age_group_min, age_group_max),
      age_group = factor(age_group, fcds_const("age_group"), ordered = TRUE)
    ) %>%
    select(-dplyr::matches("(min|max)$")) %>%
    group_by(age_group)

  stdized_age_group <- d_age_group %>% group_by(age_group) %>%
    standardize_age_groups()

  expect_equal(stdized_age_group, expected_age_group)
  expect_equal(levels(stdized_age_group$age_group), fcds_const("age_group"))
})

# ---- |> recode_age_groups() ----

describe("recode_age_groups()", {
  d_age_groups <- dplyr::tibble(
    age_min = seq(0, 25, 5),
    age_max = seq(4, 29, 5)
  ) %>%
    format_age_groups()


  e_levels <- c("0 - 9", "10 - 19", "20 - 24", "25+")
  e_factor <- factor(e_levels[c(1, 1, 2, 2, 3, 4)], e_levels, ordered = TRUE)

  it("recodes age groups", {
    r_age_groups <- d_age_groups %>%
      recode_age_groups(breaks = c(10, 20, 25))

    expect_equal(r_age_groups$age_group, e_factor)

    expect_equal(
      r_age_groups$age_group %>% levels(),
      c("0 - 9", "10 - 19", "20 - 24", "25+")
    )

    r_age_groups2 <- d_age_groups %>%
      recode_age_groups(breaks = c(25, 20, 10))

    expect_equal(r_age_groups2, r_age_groups)

    r_age_groups3 <- d_age_groups %>%
      dplyr::select(age_group) %>%
      recode_age_groups(breaks = c(10, 20, 25))

    expect_equal(r_age_groups3, r_age_groups[, "age_group"])
  })

  it("upper and lower age group determined by existing groups", {
    r_age_groups_upper <- d_age_groups %>%
      recode_age_groups(breaks = c(10, 20, 25, NA))

    expect_equal(
      r_age_groups_upper$age_group %>% paste(),
      c(rep("0 - 9", 2), rep("10 - 19", 2), "20 - 24", "25 - 29")
    )

    r_age_groups_lower <- d_age_groups %>%
      dplyr::filter(age_min >= 5) %>%
      recode_age_groups(breaks = c(NA, 10, 20, 25))

    expect_equal(
      r_age_groups_lower$age_group %>% paste(),
      c("5 - 9", rep("10 - 19", 2), "20 - 24", "25+")
    )

    r_age_groups_both <- d_age_groups %>%
      dplyr::filter(age_min >= 5) %>%
      recode_age_groups(breaks = c(NA, 10, 20, NA))

    expect_equal(
      r_age_groups_both$age_group %>% paste(),
      c("5 - 9", rep("10 - 19", 2), rep("20 - 29", 2))
    )
  })

  it("errors when breaks fall within existing groups", {
    expect_error(
      recode_age_groups(d_age_groups, breaks = c(9))
    )
  })

  it("errors when breaks are all missing", {
    expect_error(
      recode_age_groups(d_age_groups, breaks = NA)
    )

    expect_error(
      recode_age_groups(d_age_groups, breaks = c(NA, NA))
    )
  })

  it("dichotomizes groups", {
    r_age_groups <- d_age_groups %>%
      recode_age_groups(breaks = 15)
    e_levels <- c("0 - 14", "15+")

    expect_equal(
      r_age_groups$age_group,
      factor(e_levels[c(1, 1, 1, 2, 2, 2)], e_levels, ordered = TRUE)
    )

    r_age_groups_lower <- d_age_groups %>%
      filter(age_min >= 5) %>%
      recode_age_groups(breaks = c(NA, 15))

    e_lower_levels <- c("5 - 14", "15+")

    expect_equal(
      r_age_groups_lower$age_group,
      factor(e_lower_levels[c(1, 1, 2, 2, 2)], e_lower_levels, ordered = TRUE)
    )

    r_age_groups_upper <- d_age_groups %>%
      filter(age_min >= 5) %>%
      recode_age_groups(breaks = c(15, NA))

    e_upper_levels <- c("0 - 14", "15 - 29")

    expect_equal(
      r_age_groups_upper$age_group,
      factor(e_upper_levels[c(1, 1, 2, 2, 2)], e_upper_levels, ordered = TRUE)
    )

    r_age_groups_both <- d_age_groups %>%
      filter(age_min >= 5) %>%
      recode_age_groups(breaks = c(NA, 15, NA))
    e_both_levels <- c("5 - 14", "15 - 29")

    expect_equal(
      r_age_groups_both$age_group,
      factor(e_both_levels[c(1, 1, 2, 2, 2)], e_both_levels, ordered = TRUE)
    )
  })

  it("handles Unknown or NA age_groups", {
    d_age_groups <- dplyr::tibble(
      age_group = fcds_const("age_group")
    )

    r_age_groups <- d_age_groups %>%
      recode_age_groups(50)
    expect_equal(
      unique(paste(r_age_groups$age_group)),
      c("0 - 49", "50+", "Unknown")
    )
  })

  it("handles age_group being in grouping variables", {
    d_age_grouped <- d_age_groups %>%
      dplyr::mutate(i = rep(1:3, each = 2)) %>%
      dplyr::group_by(age_group, i)

    r_age_grouped <- d_age_grouped %>%
      recode_age_groups(breaks = c(10, 20, 25))

    expect_equal(dplyr::group_vars(r_age_grouped), c("age_group", "i"))
    expect_equal(r_age_grouped$age_group, e_factor)
  })

  it("handles age_group having an unusual column name", {
    d_age_unusual <- d_age_groups %>%
      dplyr::rename(unusual_name = age_group)

    r_age_unusual <- d_age_unusual %>%
      recode_age_groups(breaks = c(10, 20, 25), unusual_name)

    expect_equal(r_age_unusual$unusual_name, e_factor)

    d_age_u_grouped <- d_age_unusual %>%
      dplyr::mutate(i = rep(1:3, each = 2)) %>%
      dplyr::group_by(i, unusual_name)

     r_age_u_grouped <- d_age_u_grouped %>%
      recode_age_groups(breaks = c(10, 20, 25), unusual_name)

    expect_equal(dplyr::group_vars(r_age_u_grouped), c("i", "unusual_name"))
    expect_equal(r_age_u_grouped$unusual_name, e_factor)
  })

  it("handles age_group being a factor", {
    d_age_groups$age_group <- factor(d_age_groups$age_group, ordered = TRUE)

    r_age_groups <- d_age_groups %>%
      recode_age_groups(breaks = c(10, 20, 25))

    expect_equal(r_age_groups$age_group, e_factor)
  })
})

# Age Adjustment ----------------------------------------------------------


# Age Adjustment Example Data ----
# Example modified from https://seer.cancer.gov/seerstat/tutorials/aarates/
d_incidence <- dplyr::tribble(
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

d_population <- dplyr::tribble(
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

d_std <- dplyr::tribble(
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

d_answer <- dplyr::tribble(
     ~n, ~population, ~rate,
  27069,     8796948, 400.3
)


# Age Adjustment Tests ----

test_that("age_adjust() SEER example using built-in standard population numbers", {
  # using built-in standard population numbers
  r_age_adjusted <- age_adjust(d_incidence, population = d_population, by_year = NULL)

  expect_equal(round(r_age_adjusted$rate, 1), d_answer$rate)
  expect_equal(r_age_adjusted$population, sum(d_population$population))
  expect_equal(r_age_adjusted$n, sum(d_incidence$n))
})

test_that("age_adjust() SEER example using example standard population numbers", {
  # using SEER example standard population numbers
  r_age_adjusted <- age_adjust(d_incidence,
                               population = d_population,
                               population_standard = d_std,
                               by_year = NULL)

  expect_equal(round(r_age_adjusted$rate, 1), d_answer$rate)
  expect_equal(r_age_adjusted$population, sum(d_population$population))
  expect_equal(r_age_adjusted$n, sum(d_incidence$n))
})

test_that("age_adjust() with additional grouping", {
  d_incidence_g <- tidyr::crossing(
    sex = fcds_const("sex"),
    race = fcds_const("race"),
    d_incidence
  ) %>%
    group_by(sex, race)

  d_population_g <- tidyr::crossing(
    sex = fcds_const("sex"),
    race = fcds_const("race"),
    d_population
  )

  r_age_adjusted <- age_adjust(d_incidence_g, population = d_population_g, by_year = NULL)

  expect_equal(group_vars(r_age_adjusted), group_vars(d_incidence_g))
  expect_equal(round(r_age_adjusted$rate, 1), rep(d_answer$rate, nrow(r_age_adjusted)))

  # age_group is removed from groups too
  expect_equal(
    age_adjust(d_incidence_g %>% group_by(age_group, .add = TRUE),
               population = d_population_g, by_year = NULL),
    r_age_adjusted
  )
})

test_that("age_adjust() with year", {
  d_incidence_y <- tidyr::crossing(
    year = c("1990", "2000"),
    d_incidence
  ) %>%
    group_by(year)

  d_population_y <- tidyr::crossing(
    year = c("1990", "2000"),
    d_population
  )

  e_age_adjusted <- tidyr::crossing(
    year = c("1990", "2000"),
    age_adjust(d_incidence, population = d_population, by_year = NULL)
  ) %>%
    group_by(year)

  r_age_adjusted <- age_adjust(d_incidence_y, population = d_population_y)

  expect_equal(group_vars(r_age_adjusted), group_vars(d_incidence_y))
  expect_equal(round(r_age_adjusted$rate, 1), rep(d_answer$rate, nrow(r_age_adjusted)))
  expect_equivalent(r_age_adjusted, e_age_adjusted)
  expect_equal(group_vars(r_age_adjusted), group_vars(e_age_adjusted))
})

test_that("age_adjust() warns if no grouping provided but non-rate/age rows don't collapse to single row", {
  # incidence counts have potentially two undeclared groups
  d_incidence_2 <- tidyr::crossing(
    group = c("group 1", "group 2"),
    d_incidence
  )

  d_population_2 <- tidyr::crossing(
    group = c("group 1", "group 2"),
    d_population
  )

  expect_warning(
    age_adjust(d_incidence_2, population = d_population_2, by_year = NULL)
  )
})

test_that("age_adjust() gives error when population doesn't have population column", {
  expect_error(
    age_adjust(d_incidence,
               by_year = NULL,
               population = d_population %>% dplyr::rename(pop = population)),
    "population.+is missing"
  )
})

test_that("age_adjust() accepts non-default count and age arguments", {
  d_incidence_nd <- d_incidence %>%
    dplyr::rename(my_age = age_group, my_count = n)

  d_answer_nd <- d_answer %>% dplyr::rename(my_count = n)

  r_age_adjusted <- age_adjust(
    d_incidence_nd,
    my_count,
    population = d_population,
    age = my_age,
    by_year = NULL
  ) %>% mutate(rate = round(rate, 1))

  expect_equal(r_age_adjusted, d_answer_nd)

  # errors when `my_age` not in all datasets
  expect_error(
    age_adjust(d_incidence_nd,
               count = my_count, age = my_age, by_year = NULL,
               population = d_population %>% dplyr::rename(my_age = age_group)),
    "my_age.+population_standard"
  )

  # errors when age = age_group but one of population is different
  expect_error(
    age_adjust(d_incidence,
               by_year = NULL,
               population = d_population %>% dplyr::rename(my_age = age_group)),
    "age_group.+population"
  )

  # errors if bad data somehow makes it to age_adjust_finalize()
  expect_error(
    age_adjust_finalize(d_incidence_nd, age = my_age),
    "requires `population_standard`.+my_age"
  )
})

test_that("age_adjust() errors or warns with mismatched age_groups", {
  d_std_sub <- d_std %>% dplyr::slice(-1:-5)

  expect_error(
    age_adjust(d_incidence, population = d_population, population_standard = d_std_sub, by_year = NULL),
    "age groups"
  )

  d_incidence_gg <- d_incidence %>%
    dplyr::slice(1:10) %>%
    separate_age_groups() %>%
    mutate(
      age_min = age_min + 2,
      age_max = age_max + 2,
      age_group = factor(glue("{age_min} - {age_max}"))
    ) %>%
    select(-age_min:-age_max)

  expect_error(
    suppressWarnings(age_adjust(d_incidence_gg, population = d_population, by_year = NULL)),
    "do not match any age groups"
  )
})

test_that("age_adjust() with keep_age = TRUE", {
  r_age_adjusted <- age_adjust(d_incidence, population = d_population, by_year = NULL, keep_age = TRUE)

  e_age_adjusted <- d_incidence %>%
    dplyr::left_join(d_population) %>%
    dplyr::left_join(d_std) %>%
    select(age_group, n, population, std_pop) %>%
    mutate(
      w = std_pop / sum(std_pop),
      rate = n / population * w * 100000
    ) %>%
    fct_remove_order_all()

  expect_equal(r_age_adjusted, e_age_adjusted)
})

test_that("age_adjust() with subgroups", {
  # idea: age_adjust() uses grouping to calculate final rate, so you should be
  # able to calculate a lower-resolution incidence (e.g. within-county
  # incidence) and sub-group incidence and population will be summarized prior
  # to final age adjustment calculation.

  d_incidence_split <- d_incidence %>%
    mutate(
      county1 = n - floor(n/2),
      county2 = n - county1
    ) %>%
    select(-n) %>%
    tidyr::gather(county, n, county1:county2) %>%
    group_by(age_group)

  d_pop_split <- d_population %>%
    mutate(
      county1 = population - floor(population/2),
      county2 = population - county1
    ) %>%
    select(-population) %>%
    tidyr::gather(county, population, county1:county2)

  e_age_adjusted <- age_adjust(d_incidence, population = d_population, by_year = NULL)
  r_age_adjusted <- age_adjust(d_incidence_split, population = d_pop_split, by_year = NULL)

  expect_equal(r_age_adjusted, e_age_adjusted)
})

test_that("age_adjust() throws warning with unequal numbers of age_groups", {
  d_mismatch_age_groups <- tidyr::crossing(
    county_name = "Pinellas",
    year = "2013",
    sex = c("Female", "Male"),
    age_group = fcds_const("age_group")[1:3],
    n = 5:10
  ) %>%
    group_by(county_name, sex)

  removed_age_group <- d_mismatch_age_groups[3, ]
  d_mismatch_age_groups <- d_mismatch_age_groups[-3, ]

  expect_warning(
    d_mismatch_age_groups %>% age_adjust()
  )
})

test_that("age_adjust() throws error if count column is missing from data", {
  d_no_count <- d_incidence[, -2]
  d_no_count$year <- "2013"

  expect_error(
    d_no_count %>% age_adjust(),
    "does not contain .count. column"
  )
})

test_that("choose_seer_data() throws error if origin in data but year < 1990", {
  d_bad_origin_year <- tidyr::crossing(
    origin = fcds_const("origin"),
    year_group = fcds_const("year_group")
  )

  expect_error(choose_seer_population(d_bad_origin_year), "prior to 1990")

  d_bad_origin_year <- tidyr::crossing(
    origin = fcds_const("origin"),
    year = paste(1989:1991)
  )

  expect_error(choose_seer_population(d_bad_origin_year), "prior to 1990")
})

test_that("choose_seer_data() chooses correct data set", {
  d_seer_fl <- tidyr::crossing(
    sex = fcds_const("sex"),
    year_group = fcds_const("year_group")
  )

  expect_equal(choose_seer_population(d_seer_fl), fcds::seer_pop_fl)

  d_seer_fl_1990 <- tidyr::crossing(
    sex = fcds_const('sex'),
    year = "1993"
  )

  expect_equal(choose_seer_population(d_seer_fl_1990), fcds::seer_pop_fl_1990)
})
