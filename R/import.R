
# Import FCDS Data --------------------------------------------------------


#' Import FCDS Data
#'
#' Imports the FCDS data file and applies basic data pre-processing. If the user
#' provides only the path to the raw FCDS data, by default this function will
#' save a cached version of the pre-processed data in [fcds] default directory
#' (see [fcds_default_data_path()] for more information). The user can then
#' subsequently call [fcds_load()] to load the cached, pre-processed data rather
#' than repeating the importing process or needing to locate the original
#' raw data. The data caching can be disabled by setting `output_file = NULL`.
#'
#' @references <https://fcds.med.miami.edu/>
#'
#' @section FCDS Data:
#'
#' Users must request the data from FCDS directly, via the
#' [FCDS Data Request](https://fcds.med.miami.edu/inc/datarequest.shtml)
#' webpage.
#'
#' @section FCDS Recoding:
#'
#' This section will discuss the formatting for the FCDS recoding yaml file.
#'
#' @return A tibble containing the pre-processed FCDS and optionally the
#'   original columns of the raw FCDS data (if `keep_original_columns = TRUE`).
#'   The pre-processing step provides the following columns.
#'
#' - `patient_id`: Patient ID Number.
#'   NAACCR Item #[20](http://datadictionary.naaccr.org/default.aspx?c=10#20).
#'   Derived from `Patient_ID_N20`.
#' - `year`: Year of Diagnosis (5 year group).
#'   NAACCR Item #[390](http://datadictionary.naaccr.org/default.aspx?c=10#390).
#'   Derived from `Date_of_Dx_Year_Recoded`.
#' - `year_mid`: Year of Diagnosis (midpoint of 5 year group).
#'   NAACCR Item #[390](http://datadictionary.naaccr.org/default.aspx?c=10#390).
#'   Derived from `Date_of_Dx_Year_Recoded`.
#' - `cancer_status`: Cancer Status at time abstract was completed.
#'   NAACCR Item #[1770](http://datadictionary.naaccr.org/default.aspx?c=10#1770).
#'   Derived from `Cancer_Status_N1770`.
#' - `cancer_site_group`: FCDS Site Group.
#'   NAACCR Item #[2220](http://datadictionary.naaccr.org/default.aspx?c=10#2220).
#'   Derived from `FCDS_Site_Group`.
#' - `cancer_site_specific`: FCDS Site Group with specific within-group areas.
#'   NAACCR Item #[2220](http://datadictionary.naaccr.org/default.aspx?c=10#2220).
#'   Derived from `FCDS_Site_Group`.
#' - `confirmation`: Diagnostic Confirmation at first diagnosis.
#'   NAACCR Item #[490](http://datadictionary.naaccr.org/default.aspx?c=10#490).
#'   Derived from `Diagnostic_Confirmation_N490`.
#' - `age_group`: FCDS Age Group.
#'   NAACCR Item #[2220](http://datadictionary.naaccr.org/default.aspx?c=10#2220).
#'   Derived from `FCDS_Age_Group`.
#' - `race`: Race (recoded).
#'   NAACCR Item #[160](http://datadictionary.naaccr.org/default.aspx?c=10#160).
#'   Derived from `Race_Recoded`.
#' - `sex`: Sex (recoded).
#'   NAACCR Item #[220](http://datadictionary.naaccr.org/default.aspx?c=10#220).
#'   Derived from `Sex_Recoded`.
#' - `origin`: Spanish/Hispanic Origin (recoded).
#'   NAACCR Item #[190](http://datadictionary.naaccr.org/default.aspx?c=10#190).
#'   Derived from `Ethnicity_Recoded`.
#' - `marital_status`: Marital Status at diagnosis (recoded).
#'   NAACCR Item #[150](http://datadictionary.naaccr.org/default.aspx?c=10#150).
#'   Derived from `Marital_Status_Recoded`.
#' - `county_name`: County Name of patient's primary residence at the time tumor was diagnosed.
#'   NAACCR Item #[90](http://datadictionary.naaccr.org/default.aspx?c=10#90).
#'   Derived from `County_at_DX_N90`.
#' - `county_fips`: County FIPS Code of patient's primary residence at the time tumor was diagnosed.
#'   NAACCR Item #[90](http://datadictionary.naaccr.org/default.aspx?c=10#90).
#'   Derived from `County_at_DX_N90`.
#' - `state`: State of patient's primary residence at the time of diagnosis (recoded).
#'   NAACCR Item #[80](http://datadictionary.naaccr.org/default.aspx?c=10#80).
#'   Derived from `Addr_at_DX_State_Recoded`.
#' - `florida_resident`: Patient's primary state of residence was Florida at time of diagnosis.
#'   NAACCR Item #[80](http://datadictionary.naaccr.org/default.aspx?c=10#80).
#'   Derived from `Addr_at_DX_State_Recoded`.
#' - `country`: Country of patient's primary residence at time of diagnosis (recoded).
#'   NAACCR Item #[102](http://datadictionary.naaccr.org/default.aspx?c=10#102).
#'   Derived from `Addr_at_Dx_Country_Recoded`.
#' - `birth_country`: Country of Birthplace (recoded).
#'   NAACCR Item #[254](http://datadictionary.naaccr.org/default.aspx?c=10#254).
#'   Derived from `Birthplace_Country_Recoded`.
#' - `birth_state`: State of Birthplace (recoded).
#'   NAACCR Item #[254](http://datadictionary.naaccr.org/default.aspx?c=10#254).
#'   Derived from `Birthplace_State_Abrv_Recoded`.
#' - `primary_payer`: Primary Payer at Diagnosis (recoded).
#'   NAACCR Item #[630](http://datadictionary.naaccr.org/default.aspx?c=10#630).
#'   Derived from `Dx_Primary_Payor_Recoded`.
#' - `cancer_reporting_source`: Type of Reporting Source.
#'   NAACCR Item #[500](http://datadictionary.naaccr.org/default.aspx?c=10#500).
#'   Derived from `Type_of_Reporting_Source_N500`.
#' - `cancer_ICDO3_conversion`: ICD-O-3 Conversion Flag.
#'   NAACCR Item #[2116](http://datadictionary.naaccr.org/default.aspx?c=10#2116).
#'   Derived from `ICDO3_Conversion_FL_N2116`.
#' - `cancer_laterality`: Laterality at Diagnosis.
#'   NAACCR Item #[410](http://datadictionary.naaccr.org/default.aspx?c=10#410).
#'   Derived from `Laterality_N410`.
#' - `cancer_grade`: Grade, Differentiation, or Cell Lineage Indicator (SEER/CCCR).
#'   NAACCR Item #[440](http://datadictionary.naaccr.org/default.aspx?c=10#440).
#'   Derived from `Grade_N440`.
#' - `cancer_ICDO3_histology`: Histologic Type ICD-O-3.
#'   NAACCR Item #[522](http://datadictionary.naaccr.org/default.aspx?c=10#522).
#'   Derived from `Histologic_Type_ICDO3_N522`.
#' - `cancer_ICDO3_behavior`: Behavior Code ICD-O-3.
#'   NAACCR Item #[523](http://datadictionary.naaccr.org/default.aspx?c=10#523).
#'   Derived from `Behavior_Code_ICDO3_N523`.
#' - `cancer_ICDO3_morphology`: Morphology Code ICD-O-3 (Type and Behavior).
#'   NAACCR Item #[521](http://datadictionary.naaccr.org/default.aspx?c=10#521).
#'   Derived from `Histologic_Type_ICDO3_N522`, `Behavior_Code_ICDO3_N523`.
#' - `seer_stage_1977`: SEER Summary Stage 1977.
#'   NAACCR Item #[760](http://datadictionary.naaccr.org/default.aspx?c=10#760).
#'   Derived from `SEER_Summ_Stage_1977_N760`.
#' - `seer_stage_2000`: SEER Summary Stage 2000.
#'   NAACCR Item #[759](http://datadictionary.naaccr.org/default.aspx?c=10#759).
#'   Derived from `SEER_Summ_Stage_2000_N759`.
#' - `seer_stage`: SEER Stage from 2000 falling back to 1977.
#'   NAACCR Item #[759](http://datadictionary.naaccr.org/default.aspx?c=10#759).
#'   Derived from `seer_stage_1977`, `seer_stage_2000`.
#' - `seer_stage_derived_1977`: Derivation of SEER Summary Stage 1977.
#'   NAACCR Item #[3040](http://datadictionary.naaccr.org/default.aspx?c=10#3040).
#'   Derived from `Derived_SS1977_FL_N3040`.
#' - `seer_stage_derived_2000`: Derivation of SEER Summary Stage 2000.
#'   NAACCR Item #[3050](http://datadictionary.naaccr.org/default.aspx?c=10#3050).
#'   Derived from `Derived_SS2000_FL_N3050`.
#' - `tobacco_cigarette`: Cigarette smoking.
#'   NAACCR Item #[9965](http://datadictionary.naaccr.org/default.aspx?c=10#9965).
#'   Derived from `FCDS_Tob_Use_Cigarette_N1300`.
#' - `tobacco_other`: Smoking tobacco products other than cigarettes (e.g., pipes, cigars, kreteks).
#'   NAACCR Item #[9966](http://datadictionary.naaccr.org/default.aspx?c=10#9966).
#'   Derived from `FCDS_Tob_Use_OthSmoke_N1300`.
#' - `tobacco_smokeless`: Smokeless tobacco products (e.g, chewing tobacco, snuff, etc.).
#'   NAACCR Item #[9967](http://datadictionary.naaccr.org/default.aspx?c=10#9967).
#'   Derived from `FCDS_Tob_Use_Smokeless_Tob_N1300`.
#' - `tobacco_nos`: Tobacco NOS, includes use of e-cigarettes and vaporizers.
#'   NAACCR Item #[9968](http://datadictionary.naaccr.org/default.aspx?c=10#9968).
#'   Derived from `FCDS_Tob_Use_NOS_N1300`.
#'
#' @param file The raw FCDS data file
#' @param output_dir The location where the cleaned FCDS data files should be
#'   located. Set to `NULL` save to current working directory. By default,
#'   stores the processed data in [fcds_default_data_path()], which in general
#'   points to the global FCDS data cache.
#' @param output_file Name of the file to store the cleaned FCDS data file. If
#'   you don't want to save the cleaned data, set `output_file = NULL`. The file
#'   name is formatted using [strftime()] and the current system time.
#' @param keep_original_columns Should the original FCDS columns be kept in the
#'   imported data? By default, only the cleaned columns are kept.
#' @param fcds_recoding The FCDS recoding definition. See the FCDS Recoding
#'   section for more information. Set to `NULL` to use the fcds package
#'   default settings.
#' @param verbose Prints additional information about the importing process
#' @param col_types Passed to [readr::read_csv()]. By default, all columns are
#'   imported initially as character and are recoded or updated during the
#'   pre-processing step.
#' @inheritDotParams readr::read_csv
#' @inheritParams readr::read_csv
#' @family FCDS Import Functions
#' @export
fcds_import <- function(
  file,
  output_dir = fcds_default_data_path(),
  output_file = "fcds_%F-%H%M.rds",
  ...,
  keep_original_columns = FALSE,
  fcds_recoding = NULL,
  verbose = TRUE,
  col_types = readr::cols(.default = readr::col_character())
) {

  if (verbose) cat_bullet(glue("Loading data from {file}"))
  data <- readr::read_csv(file, col_types = col_types, ...)

  if (verbose) cat_bullet(glue("Preprocessing FCDS data"))
  data <- fcds_recode_values(
    data,
    fcds_recoding = fcds_recoding,
    keep_original_columns = keep_original_columns,
    verbose = verbose
  )

  if (!is.null(output_file)) {
    output_file <- strftime(Sys.time(), output_file)
    if (!dir.exists(output_dir)) dir.create(output_dir)
    output_file <- file.path(output_dir, output_file)
    if (verbose) cat_bullet("Saving cleaned FCDS data")
    readr::write_rds(data, output_file, compress = "gz")
    if (verbose) cat_tick(glue("Saved cleaned data: {output_file}"))
  }

  data
}


# Load Pre-Processed FCDS Data --------------------------------------------

#' Load Pre-Processed FCD Data
#'
#' Loads preprocessed FCDS data. If called using defaults, `fcds_load()` will
#' look for cached, preprocessed FCDS data in the typical locations (see
#' [fcds_default_data_path()]). If `file` is a directory (as is the default),
#' then `fcds_load()` will look for files with names starting with  "`fcds_`"
#' within the directory and will choose the first file when sorted in descending
#' order by name. When used in conjunction with [fcds_import()], the filename
#' will contain the date of importing — with names like "`fcds_2019-04-23.rds`"
#' — so the typical behavior of this function is to load the most recently
#' imported FCDS data set. You can override this behavior by providing a path to
#' a specific RDS file.
#'
#' @param file The pre-processed FCDS data file, or a directory containing files
#'   with names starting with "`fcds_`". See [fcds_default_data_path()].
#' @param check_data Not yet implemented. When `TRUE`, data integrity is checked
#'   after loading.
#' @family FCDS Import Functions
#' @export
fcds_load <- function(file = fcds_default_data_path(), check_data = TRUE) {

  file <- fcds_resolve_data_file(file)

  data <- readRDS(file)

  if (check_data) {
    message("FCDS data checks are not yet implemented.")
  }

  data
}

fcds_resolve_data_file <- function(file) {
  if (!file.exists(file)) {
    abort(glue("{file} does not exist, please run `fcds_import()` first."))
  }

  if (is_dir(file)) {
    # file is a directory, find the most recent fcds_ data files
    files <- dir(file, "^fcds_", full.names = TRUE)
    if (!length(files)) {
      abort(glue("No FCDS data files (named 'fcds_...') were found in {file}"))
    }
    file <- rev(sort(files))[1]
    message(glue("Loading {file}"))
  }

  file
}

is_dir <- function(path) {
  purrr::map_lgl(purrr::set_names(path), ~ isTRUE(file.info(.x)$isdir))
}

# Default Paths -----------------------------------------------------------

#' Default Path for FCDS Data
#'
#' Returns the default data path for FCDS data. You can globally set the path
#' using the `fcds.default_data_path` option, or by providing a path to
#' `fcds_default_data_path()`. Restore the FCDS package default cache by setting
#' the default to `NULL`.
#'
#' @examples
#' fcds_default_data_path()
#'
#' # Set to ~/tmp
#' fcds_default_data_path("~/tmp")
#' fcds_default_data_path()
#'
#' # Restore global default
#' fcds_default_data_path(NULL)
#' fcds_default_data_path()
#'
#' @return Returns the current global setting for the path to the FCDS data
#'   cache.
#'
#' @param path Set the default path for storing or looking for cached FCDS data.
#' @family FCDS Import Functions
#' @export
fcds_default_data_path <- function(path = NULL) {
  if (!missing(path)) {
    return(fcds_default_data_path_set(path))
  }

  default <- getOption("fcds.default_data_path")
  if (!is.null(default)) {
    return(default)
  }

  fcds_default <- file.path("~", ".fcds")
  suppressWarnings(
    enc2native(normalizePath(fcds_default))
  )
}

fcds_default_data_path_set <- function(path) {
  if (!is.null(path)) stopifnot(is.character(path) && length(path) == 1)
  options("fcds.default_data_path" = path)
  fcds_default_data_path()
}


# Cache -------------------------------------------------------------------

#' List or Clean Cached FCDS Data Files
#'
#' Helper functions to list cached processed **fcds** data files or to clean
#' outdated cached files.
#'
#' @param path The path to the fcds cached data, defaults to
#'   [fcds_default_data_path()]
#' @name fcds_cache
#' @family FCDS Import Functions
NULL

#' @describeIn fcds_cache List cached fcds data files
#' @param pattern The file pattern used to identify fcds cached data files,
#'   by default these are expected to be `.rds` files whose names start with
#'   `fcds_`.
#' @export
fcds_cache_ls <- function(path = NULL, pattern = "^fcds_.+\\.[Rr][Dd][Ss]$") {
  path <- path %||% fcds_default_data_path()
  dir(path, full.names = TRUE, pattern = pattern)
}

#' @describeIn fcds_cache Report information about cached fcds data files
#' @inheritDotParams fcds_cache_ls pattern
#' @export
fcds_cache_info <- function(path = NULL, ...) {
  fi <- dplyr::tibble(path = fcds_cache_ls(path))
  fi <- dplyr::bind_cols(fi, file.info(fcds_cache_ls(path, ...)))
  fi$size_mb <- fi$size * 10^-6
  fi %>%
    dplyr::select(path, size_mb, dplyr::ends_with("time"), mode) %>%
    dplyr::arrange(dplyr::desc(path))
}

#' @describeIn fcds_cache Clean outdated cached fcds data files
#' @inheritParams fcds_cache_info
#' @param all Should all cached data files be removed? If `FALSE`, the default
#'   value, the most recent file is kept. Recency is determined by sorting file
#'   names in reverse order rather than by file system modification times. If in
#'   doubt, use `dry_run = TRUE` to check behavior.
#' @param dry_run If `TRUE`, `fcds_cache_clean()` reports planned actions
#'   without removing any files.
#' @export
fcds_cache_clean <- function(path = NULL, ..., all = FALSE, dry_run = FALSE) {
  path <- path %||% fcds_default_data_path()
  files <- fcds_cache_ls(path, ...)
  files <- rev(sort(files))
  file_keep <- if (!all) files[1]
  files <- setdiff(files, file_keep)

  if (!length(files)) {
    cat_tick("fcds cache is clean (", path, ")")
    if (!is.null(file_keep)) cat_tick("Kept  ", file_keep)
    return(invisible())
  }
  cat_line("Cleaning fcds cache at ", path)
  if (!is.null(file_keep)) {
    cat_tick("Keeping  ", file_keep)
  }
  for (file in files) {
    cat_bullet("Removing ", file)
    if (!dry_run) unlink(file)
  }
  cat_tick("fcds cache is clean")
  return(invisible())
}

# Recode FCDS Values ------------------------------------------------------

fcds_recode_values <- function(
  data,
  fcds_recoding = NULL,
  keep_original_columns = FALSE,
  verbose = TRUE
) {
  fcds_recoding <- fcds_recoding %||% load_fcds_recoding()
  validate_fcds_recoding(fcds_recoding)

  get_recode <- function(recoding, which = c("value", "label")) {
    switch(
      match.arg(which),
      "value" = purrr::map_chr(recoding$recode, "value"),
      "label" = purrr::map_chr(recoding$recode, "label")
    )
  }

  capture_init <- function(column) {
    tibble(
      clean = column$clean,
      original = paste(column$original, collapse = ", "),
      n_unique = if (length(column$original) == 1) {
        length(unique(data[[column$original]]))
      } else NA_integer_,
      n_missing_original = if (length(column$original) == 1) {
        sum(is.na(data[[column$original]]))
      } else NA_integer_
    )
  }

  recoding_misses <- list()

  if (verbose) p <- dplyr::progress_estimated(length(fcds_recoding))

  for (action in fcds_recoding) {
    column <- action$name

    recoding_misses[[column$clean]] <- capture_init(column)

    # Check that original columns exist in input data
    if (!all(column$original %in% names(data))) {
      missing_cols <- setdiff(column$original, names(data))
      abort(glue("Input data is missing columns {and_more(missing_cols)}"))
    }


    if (!"mutate" %in% names(action)) {
      data[[column$clean]] <- data[[column$original]]

      if ("recode" %in% names(action)) {
        data[[column$clean]] <- factor(
          data[[column$clean]],
          levels = get_recode(action, "value"),
          labels = get_recode(action, "label")
        )
      }
    } else {
      # Run code in action$mutate within the context of the current data
      data[[column$clean]] <- rlang::eval_tidy(
        rlang::parse_expr(action$mutate),
        data = data
      )
    }

    recoding_misses[[column$clean]]$n_missing_after <- sum(is.na(data[[column$clean]]))

    if ("description" %in% names(action)) {
      labelled::var_label(data[[column$clean]]) <- action$description
    }

    if (verbose) p$tick()$print()
  }

  if (verbose) {
    p$stop()
    cat("\n")
  }


  attributes(data)$recode_report <- dplyr::bind_rows(recoding_misses)

  keep_columns <- purrr::map(fcds_recoding, "name") %>% purrr::map_chr("clean")
  if (!keep_original_columns) {
    data <- data[, keep_columns]
  } else {
    # move original columns to the end of the data frame
    data <- data[, intersect(keep_columns, names(data))]
  }

  data
}


load_fcds_recoding <- function(file = fcds_file("fcds_recoding.yaml"), ...) {
  yaml::read_yaml(file, ...)
}


validate_fcds_recoding <- function(recoding) {

  has_name <- purrr::map_int(recoding, ~ "name" %in% names(.))
  if (!all(has_name)) {
    no_name <- which(!has_name)
    abort(glue("All recoding items need a listed named 'name'. ",
               "Missing from elements {and_more(no_name)}"))
  }

  okay_names <- c("name", "recode", "description", "naaccr", "mutate")
  invalid_names <- purrr::map(recoding, ~ setdiff(names(.), okay_names))
  has_invalid <- purrr::map_int(invalid_names, length) > 0
  if (any(has_invalid)) {
    first_invalid <- which(has_invalid)[1]
    warn(glue(
      "Ignoring unknown recoding item(s) in element {first_invalid}",
      ifelse(sum(has_invalid) > 1, " (and {sum(has_invalid) - 1} more)", ""),
      ": {and_more(invalid_names[[first_invalid]])}"
    ))
  }

  # cannot have both "recode" AND "mutate" actions
  exclusive_actions <- c("recode", "mutate")
  has_multiple_exclusive_actions <- purrr::map_int(
    recoding,
    ~ sum(names(.) %in% exclusive_actions)
  )
  if (any(has_multiple_exclusive_actions > 1)) {
    first_with_multiple <- which(has_multiple_exclusive_actions > 1)
    exclusive_actions <- paste(exclusive_actions, collapse = ", ")
    abort(glue(
      "Recoding items actions may contain only one of {exclusive_actions}. ",
      "Action {first_with_multiple} has ",
      "{has_multiple_exclusive_actions[first_with_multiple]}."
    ))
  }

  invisible(TRUE)
}

document_fcds_recoding <- function(recoding = load_fcds_recoding()) {
  document_item <- function(item) {
    glue(
      "
- `{item$name$clean}`: {sub('\n', '', item$description)}.
  NAACCR Item #[{item$naaccr}](http://datadictionary.naaccr.org/default.aspx?c=10#{item$naaccr}).
  Derived from {paste0('`', item$name$original, '`', collapse = ', ')}.
"
    )
  }
  purrr::map_chr(recoding, document_item)
}
