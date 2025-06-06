% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_helpers.R
\name{ipums_data_collections}
\alias{ipums_data_collections}
\title{List IPUMS data collections}
\usage{
ipums_data_collections()
}
\value{
A \code{\link[tibble:tbl_df-class]{tibble}} with four columns containing the
full collection name, the type of data the collection provides,
the collection code used by the IPUMS API, and the
status of API support for the collection.
}
\description{
List IPUMS data collections with their corresponding codes used by the
IPUMS API. Note that some data collections do not yet have API support.

Currently, ipumsr supports extract definitions for the following collections:
\itemize{
\item IPUMS USA (\code{"usa"})
\item IPUMS CPS (\code{"cps"})
\item IPUMS International (\code{"ipumsi"})
\item IPUMS Time Use (\code{"atus"}, \code{"ahtus"}, \code{"mtus"})
\item IPUMS Health Surveys (\code{"nhis"}, \code{"meps"})
\item IPUMS NHGIS (\code{"nhgis"})
\item IPUMS IHGIS (\code{"ihgis"})
}

Learn more about the IPUMS API in \code{vignette("ipums-api")}.
}
\examples{
ipums_data_collections()
}
