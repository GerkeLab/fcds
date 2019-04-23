context("test-year")

d_year <- tibble::tribble(
        ~year,
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

test_that("separate_mid_year()", {
  e_separated_years <- tibble(
    year = d_year$year,
    year_min = paste(as.integer(e_mid_years) - 2L),
    year_max = paste(as.integer(e_mid_years) + 2L)
  )

  expect_equal(
    separate_year_groups(d_year),
    e_separated_years
  )
})

test_that("add_year_mid", {
  e_year_mid <- tibble(
    year = d_year$year,
    year_mid = e_mid_years
  )
  expect_equal(add_mid_year_groups(d_year), e_year_mid)
})
