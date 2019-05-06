# Data Sets ---------------------------------------------------------------

#' Florida County FIPS Codes
#'
#' Federal Information Processing Standards (FIPS) County Codes for Florida, as
#' defined in `20 Appendix B FIPS County Codes for Florida.pdf` provided by
#' FCDS.
#'
#' @format A data frame with 68 rows and 2 variables: \describe{
#'   \item{`county_name`}{Name of the county in Florida.}
#'   \item{`county_fips`}{FIPS code.} }
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
#' the FCDS data. Only years matching the FCDS 5-year mid year are included.
#'
#' @references <https://seer.cancer.gov/popdata/>
#' @references <https://seer.cancer.gov/popdata/yr1969_2016.19ages/fl.1969_2016.19ages.txt.gz>
#' @references <https://seer.cancer.gov/popdata/popdic.html>
#'
#' @format A data frame with 53,466 rows and 11 variables:
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


#' SEER Florida population data, with origin, post-1990
#'
#' Florida county-level population estimates from 1990--2016 in 19 age groups,
#' with Spanish/Hispanic origin. Downloaded from SEER on 2019-01-24 (see
#' references for direct links). Recoding of column values was performed
#' according to the guidelines available at
#' <https://seer.cancer.gov/popdata/popdic.html>. The original SEER data
#' includes 19 age groups, but the data set provided by this package includes
#' age "`0`" with ages "`1 - 4`" in the "`0 - 4`" group to match the coding in
#' the FCDS data. The SEER data was also modified to match the FCDS race
#' recoding by collapsing the expanded race variable into "White", "Black" and
#' "Other". Only years matching the FCDS 5-year mid year are included.
#'
#' @references <https://seer.cancer.gov/popdata/>
#' @references <https://seer.cancer.gov/popdata/yr1990_2016.19ages/fl.1990_2016.19ages.txt.gz>
#' @references <https://seer.cancer.gov/popdata/popdic.html>
#'
#'
#' @format A data frame with 75,335 rows and 11 variables:
#' \describe{
#'   \item{`year`}{Year}
#'   \item{`state`}{State postal abbreviation}
#'   \item{`state_fips`}{State FIPS code}
#'   \item{`county_fips`}{County FIPS code}
#'   \item{`registry`}{Registry}
#'   \item{`race`}{Race: White, Black, Other}
#'   \item{`origin`}{Origin: Non-Hispanic, Hispanic}
#'   \item{`sex`}{Sex}
#'   \item{`age_group`}{Age: 19 groups from 0, 1--4, 5--9, \dots 85+}
#'   \item{`population`}{Population}
#'   \item{`county_name`}{County Name, see [county_fips_fl]}
#' }
#' @family SEER Population Data
"seer_pop_fl_1990"


#' SEER U.S. Standard Population for Age-Adjustment
#'
#' U.S. Standard populations, year 2000, for age-adjusted statistics. Downloaded
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


#' FCDS Example Data
#'
#' Example data similar to the processed FCDS data that is returned by
#' [fcds_import()]. The values contained within are only representative of the
#' types of values that appear in the final data set, but have **no overlap**
#' whatsoever with the published data.
#'
#' @format A data frame with 7,951 rows and 36 variables. Note that the official
#'   data contains more than 3 million rows. This example, while representative
#'   of the offical data, contains simulated data for only 15 counties in
#'   Florida, may not include all cancer types or all possible values seen in
#'   the data and has absolutely no overlap (i.e. no row-wise matches, excluding
#'   `patient_id`) with the published data.
"fcds_example"


#' SEER U.S. Population
#'
#' Contains the SEER _All States Combined, Adjusted_ data, with Spanish/Hispanic
#' origin. The SEER data included expanded race groups, but these were collapsed
#' to match the FCDS race grouping of "White", "Black", or "Other". Downloaded
#' from [SEER Population Data](https://seer.cancer.gov/popdata/download.html).
#' This data set is minimally processed to join ages "`0`" with "`1 - 4`" to
#' match the FCDS data format.
#'
#' @references <https://seer.cancer.gov/popdata/>
#' @references <https://seer.cancer.gov/popdata/download.html>
#'
#' @format A data frame with 1,140 rows and 6 variables:
#' \describe{
#'   \item{\code{year}}{character. Year}
#'   \item{\code{race}}{factor. Race: "White", "Black", "Other"}
#'   \item{\code{origin}}{factor. Hispanic Origin: "Hispanic", "Non-Hispanic"}
#'   \item{\code{sex}}{factor. Sex: "Male", "Female"}
#'   \item{\code{age_group}}{factor. Five-year age group from "`0 - 4`" to "`85+`"}
#'   \item{\code{population}}{integer. Number of persons in U.S. population matching demographics}
#' }
"seer_pop_us"
