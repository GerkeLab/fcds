
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


# Constants ---------------------------------------------------------------

test_that("fcds_const(): partial matching", {
  expect_equal(
    fcds_const("moffitt"),
    fcds_const("moffitt_catchment")
  )
})

test_that("fcds_const(): full only applies to recoding values", {
  expect_equal(
    fcds_const("moffitt_catchment", full = TRUE),
    fcds_const("moffitt_catchment")
  )
})

test_that("fcds_const(): using path to fcds_recoding.yaml", {
  expect_equal(
    fcds_const("moffitt_catchment",
               fcds_recoding_file = fcds_file("fcds_recoding.yaml")),
    fcds_const("moffitt_catchment")
  )
})

test_that("fcds_const(): age groups consistent with SEER ages", {
  expect_equal(
    fcds_const("age_group"),
    levels(fcds::seer_std_ages$age_group)
  )
})

test_that("fcds_const(): Get a tibble when requesting full = TRUE", {
  r_const_age_group <- fcds_const("age_group", full = TRUE)
  expect_known_hash(r_const_age_group, "0df87b19da")
  expect_true(inherits(r_const_age_group, "data.frame"))
  expect_equal(
    names(r_const_age_group),
    c("name_clean", "name_original", "value", "label")
  )
  expect_equal(r_const_age_group$label, fcds_const("age_group"))
})

test_that("fcds_const(): Get a message when var = NULL", {
  expect_message(fcds_const(NULL))
})

test_that("fcds_const(): Get an error when duplicate matching", {
  expect_error(fcds_const("cancer"), "does not.+match")

})

test_that("fcds_const(): Get an error when requesting a non-recoding constant", {
  expect_error(fcds_const("patient_id"), "does not.+match")
})

test_that("valid_fcds_const(): Returns valid values when valid", {
  expect_equal(valid_fcds_const("age_group", "10 - 14"), "10 - 14")
  expect_equal(valid_fcds_const("county_name", fcds_const("moffitt_catchment")),
               fcds_const("moffitt_catchment"))
  expect_null(valid_fcds_const("race", NULL))
})

test_that("valid_fcds_const(): Gives an error when values are invalid", {
  expect_error(valid_fcds_const("age_group", "12"), "age_group")
  expect_error(valid_fcds_const("county_name", "Ohio"))
  expect_error(valid_fcds_const("patient_id", "Ohio"))
})

test_that("get_fcds_recoding()", {
  expect_error(get_fcds_recoding("taco"), "[Nn]o recoding found")

  bad_recoding <- list(
    list(
      name = list(clean = "taco")
    ),
    list(
      name = list(clean = "taco")
    )
  )

  expect_error(get_fcds_recoding("taco", bad_recoding), "[Mm]ore than one")
})


# Recoding ----

describe("fcds_recoding()", {
  it("returns list of available recoding settings", {
    recoding_files <- dir(fcds_file(), pattern = "fcds_recoding")
    expect_setequal(fcds_recoding(NULL), recoding_files)
  })

  it("returns latest release by default", {
    expect_equal(fcds_recoding(), "fcds_recoding.yaml")
  })

  it("returns version by release year", {
    expect_equal(fcds_recoding(2018), "fcds_recoding_release-2018.yaml")
    expect_equal(fcds_recoding("2018"), "fcds_recoding_release-2018.yaml")
    expect_equal(fcds_recoding(2019), "fcds_recoding.yaml")
  })

  it("returns errors for bad release years", {
    expect_error(fcds_recoding(18))
    expect_error(fcds_recoding("18"))
  })

  it("returns version if full file given", {
    expect_equal(fcds_recoding("fcds_recoding.yaml"), "fcds_recoding.yaml")
    expect_equal(fcds_recoding("fcds_recoding"), "fcds_recoding.yaml")
    expect_equal(fcds_recoding("fcds_recoding_release-2018.yaml"), "fcds_recoding_release-2018.yaml")
    expect_equal(fcds_recoding("fcds_recoding_release-2018"), "fcds_recoding_release-2018.yaml")
  })
})
