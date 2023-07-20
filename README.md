
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

ipumsr provides an R interface for handling IPUMS data, allowing users
to:

- Easily read files downloaded from the IPUMS extract system

- Submit requests for data and download files through the IPUMS API

- Clean and prepare data using the contextual information contained in
  the variable-level metadata that is included with many IPUMS files

## What is IPUMS?

[IPUMS](https://www.ipums.org/mission-purpose) is the world’s largest
publicly available individual-level population database, providing
census and survey data from around the world integrated across time and
space. IPUMS integration and documentation make it easy to study change,
conduct comparative research, merge information across data types, and
analyze individuals within family and community context. Data and
services are available free of charge.

IPUMS consists of multiple projects, or collections, that provide
different data products.

- **Microdata** projects distribute data for individual survey units,
  like people or households.
- **Aggregate data** projects distribute aggregate statistics calculated
  from microdata for particular geographic units.

| Project                                                               | Data Type      | Description                                                                                                                                                                                                                                             |
|-----------------------------------------------------------------------|----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [IPUMS USA](https://usa.ipums.org/usa/)                               | Microdata      | U.S. Census and American Community Survey microdata (1850-present)                                                                                                                                                                                      |
| [IPUMS CPS](https://cps.ipums.org/cps/)                               | Microdata      | Current Population Survey microdata including basic monthly surveys and supplements (1962-present)                                                                                                                                                      |
| [IPUMS International](https://international.ipums.org/international/) | Microdata      | Census microdata covering over 100 countries, contemporary and historical                                                                                                                                                                               |
| [IPUMS NHGIS](https://www.nhgis.org/)                                 | Aggregate Data | Tabular U.S. Census data and GIS boundary files (1790-present)                                                                                                                                                                                          |
| [IPUMS IHGIS](https://ihgis.ipums.org/)                               | Aggregate Data | Tabular and GIS data from population, housing, and agricultural censuses around the world                                                                                                                                                               |
| [IPUMS Time Use](https://timeuse.ipums.org/)                          | Microdata      | Time use microdata from the U.S. (1930-present) and thirteen other countries (1965-present)                                                                                                                                                             |
| [IPUMS Health Surveys](https://healthsurveys.ipums.org/)              | Microdata      | Microdata from the U.S. [National Health Interview Survey (NHIS)](https://nhis.ipums.org/nhis/) (1963-present) and [Medical Expenditure Panel Survey (MEPS)](https://meps.ipums.org/meps/) (1996-present)                                               |
| [IPUMS Global Health](https://globalhealth.ipums.org/)                | Microdata      | Health survey microdata for low- and middle-income countries, including harmonized data collections for [Demographic and Health Surveys (DHS)](https://www.idhsdata.org/) and [Performance Monitoring for Action (PMA)](https://pma.ipums.org/) surveys |
| [IPUMS Higher Ed](https://highered.ipums.org/highered/)               | Microdata      | Survey microdata on the science and engineering workforce in the U.S. from 1993 to 2013                                                                                                                                                                 |

## Installation

To install the package from CRAN, use

``` r
install.packages("ipumsr")
```

To install the development version of the package, use

``` r
remotes::install_github("ipums/ipumsr")
```

## Getting started

If you’re new to IPUMS data, see the [general
introduction](https://tech.popdata.org/ipumsr/articles/ipums.html).

To learn about obtaining IPUMS data via the IPUMS API, see the [IPUMS
API
introduction](https://tech.popdata.org/ipumsr/articles/ipums-api.html)

Additional demonstrations are also available on the [package
website](https://tech.popdata.org/ipumsr/articles/). You can access them
through the **articles** tab or by using the `vignette()` function in R
(e.g. `vignette("ipums-read", package = "ipumsr")`).

The [IPUMS support website](https://www.ipums.org/support/exercises)
also houses many project-specific exercises.

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
