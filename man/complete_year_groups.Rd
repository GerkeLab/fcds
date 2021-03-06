% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/year.R
\name{complete_year_groups}
\alias{complete_year_groups}
\title{Complete Year Groups}
\usage{
complete_year_groups(data, ..., year_gt = NULL, year_lt = NULL,
  year_group = year_group, year_group_levels = NULL, fill = list(n =
  0))
}
\arguments{
\item{data}{A data frame}

\item{...}{Ignored if \code{data} is a grouped data frame. If not grouped,
additional arguments are passed to \code{\link[tidyr:complete]{tidyr::complete()}}. Use these arguments
specify which columns are included in the expansion and how. See
\code{\link[tidyr:complete]{tidyr::complete()}} for more information.}

\item{year_gt}{Optional earliest year to include (inclusive)}

\item{year_lt}{Optional latest year to include (inclusive)}

\item{year_group}{The unquoted column containing the \code{year_group}.}

\item{year_group_levels}{The expected year groups that should appear in the
data set. If not supplied, the expected year groups from the latest FCDS
release will be used instead.}

\item{fill}{Default values for rows in columns added to the data}
}
\description{
Completes year groups in the data frame with the expected year values,
see \code{\link[=fcds_const]{fcds_const()}}.
}
\examples{
d_year <- tidyr::crossing(
  sex = "Female",
  race = fcds_const("race"),
  year_group = c("1981-1985", "1986-1990", "1991-1995", "1996-2000",
                 "2001-2005", "2006-2010", "2011-2015")
) \%>\%
  dplyr::mutate(year_group = factor(year_group))

# These two versions are equivalent. The first version completes all variables
# included in the grouping and the second explicitly declares the variables
# that should be completed.

d_year \%>\%
  dplyr::group_by(sex, race) \%>\%
  complete_year_groups() \%>\%
  dplyr::arrange(sex, race, year_group)

d_year \%>\%
  complete_year_groups(sex, race) \%>\%
  dplyr::arrange(sex, race, year_group)

# If you have previously filtered the data to include a subset of years, you
# will likely want to exclude those years from the group completion.
# You can use `year_gt` and `year_lt` to exclude years beyond the boundaries.

d_year \%>\%
  dplyr::filter(!year_group \%in\% c("1981-1985", "1986-1990")) \%>\%
  complete_year_groups(year_gt = 1990) \%>\%
  dplyr::arrange(sex, race, year_group)

}
\seealso{
Other year processors: \code{\link{add_mid_year_groups}},
  \code{\link{separate_year_groups}}
}
\concept{year processors}
