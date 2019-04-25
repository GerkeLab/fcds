
# Caching -----------------------------------------------------------------

set.seed(42122116)
fake_cache <- tempfile("fcds_")
dates <- as.POSIXct("2019-04-25 09:50:04", tz = "UTC") -
  sample(5:20, 5) * 3600 * 24 * #days
  sample(-12:12, 5) * 3600 *    #hours
  sample(-60:60, 5) * 60        #minutes
files <- file.path(fake_cache, strftime(dates, "fcds_%F-%H%M.rds"))

setup({
  dir.create(fake_cache)

  for (file in files) file.create(file)

  file.create(file.path(fake_cache, "ignore_me.rds"))
})

test_that("fcds_cache", {
  expect_equal(
    fcds_cache_ls(fake_cache),
    sort(files)
  )

  expect_equal(
    fcds_cache_ls(fake_cache, pattern = NULL),
    sort(c(files, file.path(fake_cache, "ignore_me.rds")))
  )

  expect_equal(nrow(fcds_cache_info(fake_cache)), 5)
  expect_equal(nrow(fcds_cache_info(".")), 0)

  # clean cache: dry_run
  before <- fcds_cache_ls(fake_cache)
  clean_out <- capture.output(fcds_cache_clean(fake_cache, dry_run = TRUE))
  after <- fcds_cache_ls(fake_cache)
  expect_equal(after, before)
  expect_true(sum(grepl("Keeping", clean_out)) == 1)
  expect_true(sum(grepl("Removing", clean_out)) == length(files) - 1L)

  # clean cache: all = FALSE
  clean_out <- capture.output(fcds_cache_clean(fake_cache))
  e_file <- files[which(dates == max(dates))]
  expect_equal(fcds_cache_ls(fake_cache), e_file)
  expect_true(sum(grepl("Keeping", clean_out)) == 1)
  expect_true(sum(grepl("Removing", clean_out)) == length(files) - 1L)

  # clean cache: all = FALSE again
  clean_out <- capture.output(fcds_cache_clean(fake_cache))
  expect_equal(fcds_cache_ls(fake_cache), e_file)
  expect_true(grepl("is clean", clean_out[1]))
  expect_true(sum(grepl("Kept", clean_out)) == 1)
  expect_true(sum(grepl("Removing", clean_out)) == 0)

  # clean cache: all = TRUE
  clean_out <- capture.output(fcds_cache_clean(fake_cache, all = TRUE))
  expect_equal(
    fcds_cache_ls(fake_cache, NULL),
    file.path(fake_cache, "ignore_me.rds")
  )
  expect_true(sum(grepl("Keeping", clean_out)) == 0)
  expect_true(sum(grepl("Removing", clean_out)) == 1)
})

teardown({
  unlink(fake_cache, recursive = TRUE)
})
