
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ipumsr <img src="man/figures/logo.png" align="right" height="149" width="128.5"/>

<!-- badges: start -->

[![Project
Status:Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ipumsr)](https://CRAN.R-project.org/package=ipumsr)
[![R build
status](https://github.com/ipums/ipumsr/workflows/R-CMD-check/badge.svg)](https://github.com/ipums/ipumsr/actions)
[![Codecov test
coverage](https://codecov.io/gh/ipums/ipumsr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ipums/ipumsr?branch=main)

<!-- badges: end -->

ipumsr provides an R interface for handling
[IPUMS](https://www.ipums.org) data, allowing users to:

- Easily read files downloaded from the IPUMS extract system

- Request data, download files, and get metadata from certain IPUMS
  collections

- Interpret and process data using the contextual information that is
  included with many IPUMS files

## Installation

To install the package from CRAN, use

``` r
install.packages("ipumsr")
```

To install the development version of the package, use

``` r
remotes::install_github("ipums/ipumsr")
```

## What is IPUMS?

[IPUMS](https://www.ipums.org/mission-purpose) is the world’s largest
publicly available population database, providing census and survey data
from around the world integrated across time and space. IPUMS
integration and documentation make it easy to study change, conduct
comparative research, merge information across data types, and analyze
individuals within family and community context. Data and services are
available free of charge.

IPUMS consists of multiple projects, or collections, that provide
different data products.

- **Microdata** projects distribute data for individual survey units,
  like people or households.
- **Aggregate data** projects distribute summary tables of aggregate
  statistics for particular geographic units along with corresponding
  GIS mapping files.

ipumsr supports different levels of functionality for each IPUMS
project:

| Project / Collection                                                  | Data Type      | Description                                                                                                                                                                                                                                             | Read Data Extracts | Request & Download Data | Get Metadata |
|:----------------------------------------------------------------------|:---------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------------|:------------------------|:-------------|
| [IPUMS USA](https://usa.ipums.org/usa/)                               | Microdata      | U.S. Census and American Community Survey microdata (1850-present)                                                                                                                                                                                      | X                  | X                       |              |
| [IPUMS CPS](https://cps.ipums.org/cps/)                               | Microdata      | Current Population Survey microdata including basic monthly surveys and supplements (1962-present)                                                                                                                                                      | X                  | X                       |              |
| [IPUMS International](https://international.ipums.org/international/) | Microdata      | Census microdata covering over 100 countries, contemporary and historical                                                                                                                                                                               | X                  | X                       |              |
| [IPUMS NHGIS](https://www.nhgis.org/)                                 | Aggregate Data | Tabular U.S. Census data and GIS mapping files (1790-present)                                                                                                                                                                                           | X                  | X                       | X            |
| [IPUMS IHGIS](https://ihgis.ipums.org/)                               | Aggregate Data | Tabular and GIS data from population, housing, and agricultural censuses around the world                                                                                                                                                               |                    |                         |              |
| [IPUMS Time Use](https://timeuse.ipums.org/)                          | Microdata      | Time use microdata from the U.S. (1930-present) and thirteen other countries (1965-present)                                                                                                                                                             | X                  |                         |              |
| [IPUMS Health Surveys](https://healthsurveys.ipums.org/)              | Microdata      | Microdata from the U.S. [National Health Interview Survey (NHIS)](https://nhis.ipums.org/nhis/) (1963-present) and [Medical Expenditure Panel Survey (MEPS)](https://meps.ipums.org/meps/) (1996-present)                                               | X                  |                         |              |
| [IPUMS Global Health](https://globalhealth.ipums.org/)                | Microdata      | Health survey microdata for low- and middle-income countries, including harmonized data collections for [Demographic and Health Surveys (DHS)](https://www.idhsdata.org/) and [Performance Monitoring for Action (PMA)](https://pma.ipums.org/) surveys | X                  |                         |              |
| [IPUMS Higher Ed](https://highered.ipums.org/highered/)               | Microdata      | Survey microdata on the science and engineering workforce in the U.S. from 1993 to 2013                                                                                                                                                                 | X                  |                         |              |

ipumsr uses the [IPUMS API](https://developer.ipums.org/) to submit data
requests, download data extracts, and get metadata, so the scope of
ipumsr functionality generally corresponds to the [available API
functionality](https://developer.ipums.org/docs/v2/apiprogram/apis/). As
the IPUMS team extends the API to support more functionality for more
projects, we aim to extend ipumsr capabilities accordingly.

## Getting started

If you’re new to IPUMS data, learn more about what’s available through
the [IPUMS Projects Overview](https://www.ipums.org/overview).

For an overview of how to find, request, download, and read IPUMS data
into R, see [IPUMS Data and
R](https://tech.popdata.org/ipumsr/articles/ipums.html).

To learn how ipumsr interfaces with the IPUMS extract system via the
IPUMS API, see the [Introduction to the IPUMS API for R
Users](https://tech.popdata.org/ipumsr/articles/ipums-api.html).

Additional demonstration vignettes are also available as
[Articles](https://tech.popdata.org/ipumsr/articles/) on the package
website or by using the `vignette()` function in R (e.g.
`vignette("ipums-read", package = "ipumsr")`). These include vignettes
for accessing
[microdata](https://tech.popdata.org/ipumsr/articles/ipums-api-micro.html)
and [NHGIS data and
metadata](https://tech.popdata.org/ipumsr/articles/ipums-api-nhgis.html)
via the API.

The [IPUMS support website](https://www.ipums.org/support) also houses
many project-specific R-based [training
exercises](https://www.ipums.org/support/exercises). (As of September
2023, these exercises have not yet been updated to incorporate the major
API data and metadata access capabilities added to ipumsr in version
0.6.0.)

## Related work

- The [survey](http://r-survey.r-forge.r-project.org/survey/) and
  [srvyr](https://github.com/gergness/srvyr/) packages can help you
  incorporate IPUMS survey weights into your analysis for various survey
  designs.

- See [haven](https://haven.tidyverse.org/index.html) for more
  information about value labels and labelled vectors

- [hipread](https://github.com/ipums/hipread) underlies the hierarchical
  file reading functions in ipumsr

## Getting help + contributing

We greatly appreciate feedback and development contributions. Please
submit any bug reports, pull requests, or other suggestions on
[GitHub](https://github.com/ipums/ipumsr/issues). Before contributing,
please be sure to read the [Contributing
Guidelines](https://github.com/ipums/ipumsr/blob/master/CONTRIBUTING.md)
and the [Code of
Conduct](https://github.com/ipums/ipumsr/blob/master/CONDUCT.md).

If you have general questions or concerns about IPUMS data, check out
our [user forum](https://forum.ipums.org) or send an email to
<ipums@umn.edu>.
