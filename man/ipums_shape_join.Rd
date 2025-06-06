% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shape_join.R
\name{ipums_shape_join}
\alias{ipums_shape_join}
\alias{ipums_shape_left_join}
\alias{ipums_shape_right_join}
\alias{ipums_shape_inner_join}
\alias{ipums_shape_full_join}
\title{Join tabular data to geographic boundaries}
\usage{
ipums_shape_left_join(
  data,
  shape_data,
  by,
  suffix = c("", "SHAPE"),
  verbose = TRUE
)

ipums_shape_right_join(
  data,
  shape_data,
  by,
  suffix = c("", "SHAPE"),
  verbose = TRUE
)

ipums_shape_inner_join(
  data,
  shape_data,
  by,
  suffix = c("", "SHAPE"),
  verbose = TRUE
)

ipums_shape_full_join(
  data,
  shape_data,
  by,
  suffix = c("", "SHAPE"),
  verbose = TRUE
)
}
\arguments{
\item{data}{A tibble or data frame. Typically, this will contain data that
has been aggregated to a specific geographic level.}

\item{shape_data}{An \code{\link[sf:sf]{sf}} object loaded with \code{\link[=read_ipums_sf]{read_ipums_sf()}}.}

\item{by}{Character vector of variables to join by. See \code{\link[dplyr:mutate-joins]{dplyr::left_join()}}
for syntax.}

\item{suffix}{If there are non-joined duplicate variables in the two
data sources, these suffixes will be added to the output to disambiguate
them. Should be a character vector of length 2.

Defaults to adding the \code{"SHAPE"} suffix to duplicated variables in
\code{shape_file}.}

\item{verbose}{If \code{TRUE}, display information about any geometries that were
unmatched during the join.}
}
\value{
An \code{sf} object containing the joined data
}
\description{
These functions are analogous to dplyr's \link[dplyr:mutate-joins]{joins}, except
that:
\itemize{
\item They operate on a data frame and an \code{\link[sf:sf]{sf}} object
\item They retain the variable attributes provided in IPUMS files and loaded
by ipumsr data-reading functions
\item They handle minor incompatibilities between attributes in spatial and
tabular data that emerge in some IPUMS files
}
}
\examples{
\dontshow{if (requireNamespace("sf")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
data <- read_ipums_agg(
  ipums_example("nhgis0972_csv.zip"),
  verbose = FALSE
)

sf_data <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))
joined_data <- ipums_shape_inner_join(data, sf_data, by = "GISJOIN")

colnames(joined_data)
\dontshow{\}) # examplesIf}
}
