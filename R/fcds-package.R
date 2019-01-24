#' @keywords internal
#' @importFrom dplyr select filter mutate recode
#' @importFrom dplyr tibble as_tibble
"_PACKAGE"

#' Florida County FIPS Codes
#'
#' Federal Information Processing Standards (FIPS) County Codes for Florida, as
#' defined in `20 Appendix B FIPS County Codes for Florida.pdf` provided by
#' FCDS.
#'
#' @format A data frame with 68 rows and 2 variables: \describe{
#'   \item{`county_name`}{Name of the county in Florida.}
#'   \item{`fips_code`}{FIPS code.} }
"county_fips_code"


#' SEER Florida population data
#'
#' Florida county-level population estimates from 1969--2016 in 18 age groups,
#' with races "white", "black", and "other". Downloaded from SEER on 2019-01-24
#' (see references for direct links). Recoding of column values was performed
#' according to the guidelines available at
#' <https://seer.cancer.gov/popdata/popdic.html>. For general consistency, all
#' variables are stored as `character` vectors. The original data includes 19
#' age groups, but age `0` is included with ages `1 - 4` in the `0 - 4` group
#' in this data.
#'
#' @references <https://seer.cancer.gov/popdata/>
#' @references <https://seer.cancer.gov/popdata/yr1969_2016.19ages/fl.1969_2016.19ages.txt.gz>
#' @references <https://seer.cancer.gov/popdata/popdic.html>
#'
#' @format A data frame with 334273 rows and 11 variables: \describe{
#'   \item{`year`}{Year} \item{`state`}{State postal abbreviation}
#'   \item{`state_fips`}{State FIPS code} \item{`county_fips`}{County FIPS code}
#'   \item{`registry`}{Registry} \item{`race`}{Race (White, Black, Other)}
#'   \item{`origin`}{Origin: Non-Hispanic, Hispanic. Not applicable for data
#'   prior to 1990.} \item{`sex`}{Sex} \item{`age_group`}{Age: 18 groups from 0,
#'   0- 4, 5 - 9, \dots 85+} \item{`population`}{Population}
#'   \item{`county_name`}{County Name, see [county_fips_code()]} }
#' @family SEER Population Data
"seer_fl_pop"

#' SEER Florida population data, expanded Race
#'
#' Florida county-level population estimates from 1990--2016 in 19 age groups,
#' with 4 expanded races by origin. Downloaded from SEER on 2019-01-24 (see
#' references for direct links). Recoding of column values was performed
#' according to the guidelines available at
#' <https://seer.cancer.gov/popdata/popdic.html>. For general consistency, all
#' variables are stored as `character` vectors. The original data includes 19
#' age groups, but age `0` is included with ages `1 - 4` in the `0 - 4` group
#' in this data.
#'
#' @references <https://seer.cancer.gov/popdata/>
#' @references <https://seer.cancer.gov/popdata/yr1990_2016.19ages/fl.1990_2016.19ages.txt.gz>
#' @references <https://seer.cancer.gov/popdata/popdic.html>
#'
#'
#' @format A data frame with 334273 rows and 11 variables: \describe{
#'   \item{`year`}{Year} \item{`state`}{State postal abbreviation}
#'   \item{`state_fips`}{State FIPS code} \item{`county_fips`}{County FIPS code}
#'   \item{`registry`}{Registry} \item{`race`}{Race: White, Black, American
#'   Indian/Alaska Native, Asian or Pacific Islander} \item{`origin`}{Origin:
#'   Non-Hispanic, Hispanic. Not applicable for data prior to 1990.}
#'   \item{`sex`}{Sex} \item{`age_group`}{Age: 19 groups from 0, 1--4, 5--9,
#'   \dots 85+} \item{`population`}{Population} \item{`county_name`}{County
#'   Name, see [county_fips_code()]} }
#' @family SEER Population Data
"seer_fl_pop_exp_race"


#' SEER Standard Population for Age-Adjustment
#'
#' US. Standard populations, year 2000, for age-adjusted statistics. Downloaded
#' from SEER on 2019-01-24 (see References for direct links). This data set is
#' the *2000 U.S. Std Population (single ages to 84 - Census P25-1130)*, with
#' ages agregated into 18 age groups: `0 - 4`, ..., `80 - 84`, `85+`.
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
