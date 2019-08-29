context("test-year")

d_year <- dplyr::tribble(
  ~year_group,
  "1981-1985",
  "1986-1990",
  "1991-1995",
  "1996-2000",
  "2001-2005",
  "2006-2010",
  "2011-2015"
)

e_mid_years <- c("1983", "1988", "1993", "1998", "2003", "2008", "2013")

test_that("mid_year()", {
  years <- c("1981-1985", "1986-1990", "1991-1995", "1996-2000",
             "2001-2005", "2006-2010", "2011-2015")


  expect_equal(mid_year(years), e_mid_years)

  expect_equal(
    mid_year(years, offset = 3),
    paste(as.integer(e_mid_years) + 1)
  )

  expect_warning(mid_year(e_mid_years))

  years[3] <- "1991"
  expect_warning(mid_year(years))
})

describe("separate_mid_year()", {
  it("separates `year_group` into `year_min` and `year_max`", {
    e_separated_years <- tibble(
      year_group = d_year$year_group,
      year_min = paste(as.integer(e_mid_years) - 2L),
      year_max = paste(as.integer(e_mid_years) + 2L)
    )

    expect_equal(
      separate_year_groups(d_year),
      e_separated_years
    )
  })

  it("gives informative errors when `year_group` missing from input", {
    d_year_bad <- d_year %>% dplyr::rename(year = year_group)

    expect_error(
      separate_year_groups(d_year_bad),
      "not a column in data"
    )
  })
})

test_that("add_mid_year_groups()", {
  e_year_mid <- tibble(
    year_group = d_year$year_group,
    year = e_mid_years
  )
  expect_equal(add_mid_year_groups(d_year), e_year_mid)

  expect_equal(suppressWarnings(add_mid_year_groups(e_year_mid)), e_year_mid)
  expect_warning(add_mid_year_groups(e_year_mid))
})

test_that("complete_year_groups()", {
  e_year <- tidyr::crossing(
    sex = fcds_const("sex"),
    race = fcds_const("race"),
    year_group = d_year$year_group
  )

  set.seed(421321)
  keep <- sample(nrow(e_year), 50)
  d_year <- e_year[keep, ]

  r_year_grouped <- d_year %>%
    group_by(sex, race) %>%
    complete_year_groups(year_group_levels = d_year$year_group) %>%
    dplyr::arrange(sex, race, year_group)

  r_year_ungrouped <- d_year %>%
    complete_year_groups(
      sex, race,
      year_group_levels = d_year$year_group
    ) %>%
    dplyr::arrange(sex, race, year_group)

  expect_equal(r_year_grouped, e_year)
  expect_equal(r_year_ungrouped, e_year)
  expect_equal(r_year_grouped, r_year_ungrouped)
})

test_that("complete_year_groups() when filtering years", {
  e_year <- tidyr::crossing(
    sex = fcds_const("sex"),
    race = fcds_const("race"),
    year_group = d_year$year_group
  ) %>%
    separate_year_groups() %>%
    filter(year_min >= 2001, year_max <= 2015) %>%
    select(-year_min:-year_max)

  set.seed(421321)
  d_year <- e_year %>% group_by(sex, race) %>% dplyr::sample_n(2) %>% ungroup()

  r_year_grouped <- d_year %>%
    group_by(sex, race) %>%
    complete_year_groups(
      year_gt = 2001, year_lt = 2015,
      year_group_levels = d_year$year_group
    ) %>%
    dplyr::arrange(sex, race, year_group)

  r_year_ungrouped <- d_year %>%
    complete_year_groups(
      sex, race, year_gt = 2001, year_lt = 2015,
      year_group_levels = d_year$year_group
    ) %>%
    dplyr::arrange(sex, race, year_group)

  expect_equal(r_year_grouped, e_year)
  expect_equal(r_year_ungrouped, e_year)
  expect_equal(r_year_grouped, r_year_ungrouped)
})
