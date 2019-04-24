# Data Sets ---------------------------------------------------------------

#' Florida County FIPS Codes
#'
#' Federal Information Processing Standards (FIPS) County Codes for Florida, as
#' defined in `20 Appendix B FIPS County Codes for Florida.pdf` provided by
#' FCDS.
#'
#' @format A data frame with 68 rows and 2 variables: \describe{
#'   \item{`county_name`}{Name of the county in Florida.}
#'   \item{`fips_code`}{FIPS code.} }
"county_fips_fl"


#' SEER Florida population data
#'
#' Florida county-level population estimates from 1969--2016 in 18 age groups,
#' with races "white", "black", and "other". Downloaded from SEER on 2019-01-24
#' (see references for direct links). Recoding of column values was performed
#' according to the guidelines available at
#' <https://seer.cancer.gov/popdata/popdic.html>. The original SEER data
#' includes 19 age groups, but the data set provided by this package includes
#' age "`0`" with ages "`1 - 4`" in the "`0 - 4`" group to match the coding in
#' the FCDS data.
#'
#' @references <https://seer.cancer.gov/popdata/>
#' @references <https://seer.cancer.gov/popdata/yr1969_2016.19ages/fl.1969_2016.19ages.txt.gz>
#' @references <https://seer.cancer.gov/popdata/popdic.html>
#'
#' @format A data frame with 334273 rows and 11 variables:
#' \describe{
#'   \item{`year`}{Year}
#'   \item{`state`}{State postal abbreviation}
#'   \item{`state_fips`}{State FIPS code}
#'   \item{`county_fips`}{County FIPS code}
#'   \item{`registry`}{Registry}
#'   \item{`race`}{Race (White, Black, Other)}
#'   \item{`origin`}{Origin: Non-Hispanic, Hispanic. Not applicable for data
#'                   prior to 1990.}
#'   \item{`sex`}{Sex}
#'   \item{`age_group`}{Age: 18 groups from 0,
#'                      0- 4, 5 - 9, \dots 85+}
#'   \item{`population`}{Population}
#'   \item{`county_name`}{County Name, see [county_fips_fl]}
#' }
#' @family SEER Population Data
"seer_pop_fl"


#' SEER Florida population data, expanded Race
#'
#' Florida county-level population estimates from 1990--2016 in 19 age groups,
#' with 4 expanded races by origin. Downloaded from SEER on 2019-01-24 (see
#' references for direct links). Recoding of column values was performed
#' according to the guidelines available at
#' <https://seer.cancer.gov/popdata/popdic.html>. The original SEER data
#' includes 19 age groups, but the data set provided by this package includes
#' age "`0`" with ages "`1 - 4`" in the "`0 - 4`" group to match the coding in
#' the FCDS data.
#'
#' @references <https://seer.cancer.gov/popdata/>
#' @references <https://seer.cancer.gov/popdata/yr1990_2016.19ages/fl.1990_2016.19ages.txt.gz>
#' @references <https://seer.cancer.gov/popdata/popdic.html>
#'
#'
#' @format A data frame with 334273 rows and 11 variables:
#' \describe{
#'   \item{`year`}{Year}
#'   \item{`state`}{State postal abbreviation}
#'   \item{`state_fips`}{State FIPS code}
#'   \item{`county_fips`}{County FIPS code}
#'   \item{`registry`}{Registry}
#'   \item{`race`}{Race: White, Black, American Indian/Alaska Native,
#'                 Asian or Pacific Islander}
#'   \item{`origin`}{Origin: Non-Hispanic, Hispanic.
#'                   Not applicable for data prior to 1990.}
#'   \item{`sex`}{Sex}
#'   \item{`age_group`}{Age: 19 groups from 0, 1--4, 5--9, \dots 85+}
#'   \item{`population`}{Population}
#'   \item{`county_name`}{County Name, see [county_fips_fl]}
#' }
#' @family SEER Population Data
"seer_pop_fl_exp_race"


#' SEER Standard Population for Age-Adjustment
#'
#' US. Standard populations, year 2000, for age-adjusted statistics. Downloaded
#' from SEER on 2019-01-24 (see References for direct links). This data set is
#' the *2000 U.S. Std Population (single ages to 84 - Census P25-1130)*, with
#' ages agregated into 18 age groups: "`0 - 4`", ..., "`80 - 84`", "`85+`".
#'
#' @references <https://seer.cancer.gov/stdpopulations/>
#' @references <https://seer.cancer.gov/stdpopulations/stdpop.singleagesthru84.txt>
#'
#' @format A data frame with 18 rows and 4 variables:
#' \describe{
#'   \item{\code{standard}}{Standard}
#'   \item{\code{standard_name}}{Standard Name}
#'   \item{\code{age_group}}{Age group}
#'   \item{\code{std_pop}}{Standard Population}
#' }
"seer_std_ages"

#' SEER ICD-O-3 Histology Code and Behavior
#'
#' Contains the histology codes and behavior description provided by the
#' [ICD-O-3 SEER Site/Histology Validation List](https://seer.cancer.gov/icd-o-3/).
#'
#' @references <https://seer.cancer.gov/icd-o-3/>
#' @format A data frame with 12,084 rows and 5 variables:
#' \describe{
#'   \item{\code{site_group}}{integer. Site group, e.g. "`Esophagus`"}
#'   \item{\code{histology}}{character. Four-digit ICD-O-3 histology code, e.g. "`8262`"}
#'   \item{\code{histology_description}}{integer. Description of histology, e.g. "`Papillary Adenocarcinoma, NOS`"}
#'   \item{\code{morphology}}{character. Five-digit ICD-O-3 morphology code, e.g. "`8262/3`"}
#'   \item{\code{morphology_description}}{integer. Description of morphology, e.g. "`Villous Adenocarcinoma`"}
#' }
"seer_icd_o_3"


# Constants ---------------------------------------------------------------

#' @export
fcds_const <- function(
  const = c("year", "county_name", "sex", "race", "hispanic", "age_group",
            "moffitt_catchment")
) {
  switch(
    match.arg(const),
    "year" = c(
      "1981-1985",
      "1986-1990",
      "1991-1995",
      "1996-2000",
      "2001-2005",
      "2006-2010",
      "2011-2015"),
    "county_name" = c(
      "Alachua",
      "Baker",
      "Bay",
      "Bradford",
      "Brevard",
      "Broward",
      "Calhoun",
      "Charlotte",
      "Citrus",
      "Clay",
      "Collier",
      "Columbia",
      "DeSoto",
      "Dixie",
      "Duval",
      "Escambia",
      "Flagler",
      "Franklin",
      "Gadsden",
      "Gilchrist",
      "Glades",
      "Gulf",
      "Hamilton",
      "Hardee",
      "Hendry",
      "Hernando",
      "Highlands",
      "Hillsborough",
      "Holmes",
      "Indian River",
      "Jackson",
      "Jefferson",
      "Lafayette",
      "Lake",
      "Lee",
      "Leon",
      "Levy",
      "Liberty",
      "Madison",
      "Manatee",
      "Marion",
      "Martin",
      "Miami-Dade",
      "Monroe",
      "Nassau",
      "Okaloosa",
      "Okeechobee",
      "Orange",
      "Osceola",
      "Palm Beach",
      "Pasco",
      "Pinellas",
      "Polk",
      "Putnam",
      "Santa Rosa",
      "Sarasota",
      "Seminole",
      "St. Johns",
      "St. Lucie",
      "Sumter",
      "Suwannee",
      "Taylor",
      "Union",
      "Volusia",
      "Wakulla",
      "Walton",
      "Washington"),
    "sex" = c("Female", "Male", "Unknown"),
    "race" = c("Black", "Other", "Unknown", "White"),
    "hispanic" = c("Hispanic", "Not Hispanic", "Unknown"),
    "age_group" = c(
      "0 - 4",
      "5 - 9",
      "10 - 14",
      "15 - 19",
      "20 - 24",
      "25 - 29",
      "30 - 34",
      "35 - 39",
      "40 - 44",
      "45 - 49",
      "50 - 54",
      "55 - 59",
      "60 - 64",
      "65 - 69",
      "70 - 74",
      "75 - 79",
      "80 - 84",
      "85+",
      "Unknown"),
    "moffitt_catchment" = c(
      "Hillsborough",
      "Pasco",
      "Pinellas",
      "Polk",
      "Charlotte",
      "Citrus",
      "DeSoto",
      "Hardee",
      "Hernando",
      "Highlands",
      "Lake",
      "Lee",
      "Manatee",
      "Sarasota",
      "Sumter"
    )
  )
}

valid_fcds_const <- function(value, value_name, var_name = value_name, stop = TRUE) {
  if (is.null(value)) return(NULL)
  value <- paste(value)
  not_in_const <- vapply(value, function(x) !x %in% fcds_const(value_name), logical(1))
  if (any(not_in_const)) {
    if (stop) rlang::abort(
      glue::glue("Invalid '{var_name}': \"{value[not_in_const][1]}\" is not one of the options ",
                 "in `fcds_const(\"{value_name}\")`")
    ) else {
      rlang::warn(paste("Ignoring invalid values in", var_name))
    }
    value <- value[!not_in_const]
  }
  value
}

