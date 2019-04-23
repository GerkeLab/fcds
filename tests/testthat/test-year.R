context("test-year")

test_that("mid_year()", {
  years <- c("1981-1985", "1986-1990", "1991-1995", "1996-2000",
             "2001-2005", "2006-2010", "2011-2015")

  e_mid_years <- c("1983", "1988", "1993", "1998", "2003", "2008", "2013")

  expect_equal(mid_year(years), e_mid_years)

  expect_equal(
    mid_year(years, offset = 3),
    paste(as.integer(e_mid_years) + 1)
  )

  expect_error(mid_year(e_mid_years), "Unable to extract")

  years[3] <- "1991"
  expect_warning(mid_year(years), "not found for 1 year\\(s\\)")
})
