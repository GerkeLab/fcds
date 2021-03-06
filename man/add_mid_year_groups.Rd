% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/year.R
\name{add_mid_year_groups}
\alias{add_mid_year_groups}
\title{Add Mid-Year of Year Group}
\usage{
add_mid_year_groups(data, year_group = year_group, into = "year",
  sep = "-", offset = NULL)
}
\arguments{
\item{data}{A data frame}

\item{year_group}{The unquoted column containing the \code{year_group}.}

\item{into}{Character name of the column that will contain the mid year
exctracted from \code{year_group}.}

\item{sep}{Characters separating years in \code{year}. Whitespace on either side
of \code{sep} will be automatically removed. Passed to \code{\link[tidyr:separate]{tidyr::separate()}}.}

\item{offset}{If supplied, the number of years to be added to the lower bound
of the year group to calculate the mid-year value. By default uses
\code{floor((year_max - year_min) / 2)}.}
}
\description{
Adds a new column containing the midpoint year of the range given in the
column indicated by the \code{year_group} argument, in a new column named
according to the \code{into} argument. The \code{year} column is assumed to contain
a \emph{min} year and a \emph{max} year, separated by \code{sep}. The midpoint is calculated
as \code{floor((year_max - year_min) / 2)} unless \code{offset} is explicitly provided.
}
\examples{
year_groups <- c("1981-1985", "1986-1990", "1991-1995", "1996-2000",
                 "2001-2005", "2006-2010", "2011-2015")

dplyr::tibble(year = year_groups) \%>\%
  add_mid_year_groups()

}
\seealso{
Other year processors: \code{\link{complete_year_groups}},
  \code{\link{separate_year_groups}}
}
\concept{year processors}
