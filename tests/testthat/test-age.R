d_age_group <- data.frame(
  id = 1:4,
  age_group = c("0 - 4", "10-14", "65-69", "85+")
)


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

  expect_error(
    data.frame(age_group = "3 - 6") %>% standardize_age_groups(),
    "not consistent with `std_age_groups`"
  )
  expect_equal(
    data.frame(age_group = "3 - 6") %>%
      standardize_age_groups(std_age_groups = "3 - 6"),
    data.frame(age_group = factor("3 - 6", ordered = TRUE))
  )

  numeric_char_age <- tibble(age = c("1.245", "7.5", "11.3"))
  r_numeric_char_age <- numeric_char_age %>% standardize_age_groups(age)

  # standardize_age_groups doesn't overwrite columns unless age_group
  expect_equal(r_numeric_char_age$age, numeric_char_age$age)
  expect_equal(paste(r_numeric_char_age$age_group),
               paste(seq(0, 10, 5), "-", seq(4, 14, 5)))
})
