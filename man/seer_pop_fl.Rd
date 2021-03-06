% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcds-data.R
\docType{data}
\name{seer_pop_fl}
\alias{seer_pop_fl}
\title{SEER Florida population data}
\format{A data frame with 53,466 rows and 11 variables:
\describe{
\item{\code{year}}{Year}
\item{\code{state}}{State postal abbreviation}
\item{\code{state_fips}}{State FIPS code}
\item{\code{county_fips}}{County FIPS code}
\item{\code{registry}}{Registry}
\item{\code{race}}{Race (White, Black, Other)}
\item{\code{origin}}{Origin: Non-Hispanic, Hispanic. Not applicable for data
prior to 1990.}
\item{\code{sex}}{Sex}
\item{\code{age_group}}{Age: 18 groups from 0,
0- 4, 5 - 9, \dots 85+}
\item{\code{population}}{Population}
\item{\code{county_name}}{County Name, see \link{county_fips_fl}}
}}
\usage{
seer_pop_fl
}
\description{
Florida county-level population estimates from 1969--2016 in 18 age groups,
with races "white", "black", and "other". Downloaded from SEER on 2019-01-24
(see references for direct links). Recoding of column values was performed
according to the guidelines available at
\url{https://seer.cancer.gov/popdata/popdic.html}. The original SEER data
includes 19 age groups, but the data set provided by this package includes
age "\code{0}" with ages "\code{1 - 4}" in the "\code{0 - 4}" group to match the coding in
the FCDS data. Only years matching the FCDS 5-year mid year are included.
}
\references{
\url{https://seer.cancer.gov/popdata/}

\code{https://seer.cancer.gov/popdata/yr1969_2017.19ages/fl.1969_2017.19ages.txt.gz}

\url{https://seer.cancer.gov/popdata/popdic.html}
}
\seealso{
Other SEER Population Data: \code{\link{seer_pop_fl_1990}}
}
\concept{SEER Population Data}
\keyword{datasets}
