
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

[IPUMS](https://www.ipums.org) is the world’s largest publicly available
population database, providing census and survey data from around the
world integrated across time and space. IPUMS integration and
documentation make it easy to study change, conduct comparative
research, merge information across data types, and analyze individuals
within family and community context. Data and services are available
free of charge.

IPUMS consists of multiple projects, or *collections*, that provide
different data products.

- **Microdata** projects distribute data for individual survey units,
  like people or households.
- **Aggregate data** projects distribute summary tables of aggregate
  statistics for particular geographic units along with corresponding
  GIS mapping files.

ipumsr supports different levels of functionality for each IPUMS
project, as summarized in the table below.

<table class="table-hover table-proj-summary">
<thead>
<tr>
<th style="text-align:center;">
</th>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
Data Type
</th>
<th style="text-align:left;">
Description
</th>
<th style="text-align:center;">
Read Data Extracts
</th>
<th style="text-align:center;">
Request & Download Data
</th>
<th style="text-align:center;">
Browse Metadata
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
<a href='https://usa.ipums.org/usa/'><img src='man/figures/logo-square_usa_50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://usa.ipums.org/usa/'>IPUMS USA</a>
</td>
<td style="text-align:left;">
Microdata
</td>
<td style="text-align:left;">
U.S. Census and American Community Survey microdata (1850-present)
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
<a href='https://cps.ipums.org/cps/'><img src='man/figures/logo-square_cps_50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://cps.ipums.org/cps/'>IPUMS CPS</a>
</td>
<td style="text-align:left;">
Microdata
</td>
<td style="text-align:left;">
Current Population Survey microdata including basic monthly surveys and
supplements (1962-present)
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
<a href='https://international.ipums.org/international/'><img src='man/figures/logo-square_international_50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://international.ipums.org/international/'>IPUMS
International</a>
</td>
<td style="text-align:left;">
Microdata
</td>
<td style="text-align:left;">
Census microdata covering over 100 countries, contemporary and
historical
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
<a href='https://www.nhgis.org/'><img src='man/figures/logo-square_nhgis50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://www.nhgis.org/'>IPUMS NHGIS</a>
</td>
<td style="text-align:left;">
Aggregate Data
</td>
<td style="text-align:left;">
Tabular U.S. Census data and GIS mapping files (1790-present)
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
</tr>
<tr>
<td style="text-align:center;">
<a href='https://ihgis.ipums.org/'><img src='man/figures/logo-square_ihgis_50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://ihgis.ipums.org/'>IPUMS IHGIS</a>
</td>
<td style="text-align:left;">
Aggregate Data
</td>
<td style="text-align:left;">
Tabular and GIS data from population, housing, and agricultural censuses
around the world
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
<a href='https://timeuse.ipums.org/'><img src='man/figures/logo-square_time-use_50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://timeuse.ipums.org/'>IPUMS Time Use</a>
</td>
<td style="text-align:left;">
Microdata
</td>
<td style="text-align:left;">
Time use microdata from the U.S. (1930-present) and thirteen other
countries (1965-present)
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
<a href='https://healthsurveys.ipums.org/'><img src='man/figures/logo-square_health-surveys_50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://healthsurveys.ipums.org/'>IPUMS Health Surveys</a>
</td>
<td style="text-align:left;">
Microdata
</td>
<td style="text-align:left;">
Microdata from the U.S. <a href='https://nhis.ipums.org/nhis/'>National
Health Interview Survey (NHIS)</a> (1963-present) and
<a href='https://meps.ipums.org/meps/'>Medical Expenditure Panel Survey
(MEPS)</a> (1996-present)
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
<a href='https://globalhealth.ipums.org/'><img src='man/figures/logo-square_global-health_50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://globalhealth.ipums.org/'>IPUMS Global Health</a>
</td>
<td style="text-align:left;">
Microdata
</td>
<td style="text-align:left;">
Health survey microdata for low- and middle-income countries, including
harmonized data collections for
<a href='https://www.idhsdata.org/'>Demographic and Health Surveys
(DHS)</a> and <a href='https://pma.ipums.org/'>Performance Monitoring
for Action (PMA)</a> surveys
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
<a href='https://highered.ipums.org/highered/'><img src='man/figures/logo-square_higher-ed_50x50.png'></a>
</td>
<td style="text-align:left;">
<a href='https://highered.ipums.org/highered/'>IPUMS Higher Ed</a>
</td>
<td style="text-align:left;">
Microdata
</td>
<td style="text-align:left;">
Survey microdata on the science and engineering workforce in the U.S.
from 1993 to 2013
</td>
<td style="text-align:center;">
<img src='man/figures/check-solid.svg' class='icon-check'>
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
</tbody>
</table>

ipumsr uses the [IPUMS API](https://developer.ipums.org/) to submit data
requests, download data extracts, and get metadata, so the scope of
functionality generally corresponds to that [available via the
API](https://developer.ipums.org/docs/v2/apiprogram/apis/). As the IPUMS
team extends the API to support more functionality for more projects, we
aim to extend ipumsr capabilities accordingly.

## Getting started

If you’re new to IPUMS data, learn more about what’s available through
the [IPUMS Projects Overview](https://www.ipums.org/overview). Then, see
`vignette("ipums")` for an overview of how to obtain IPUMS data.

The package vignettes are the best place to explore what ipumsr has to
offer:

- To read IPUMS data extracts into R, see `vignette("ipums-read")`.

- To interact with the IPUMS extract and metadata system via the IPUMS
  API, see `vignette("ipums-api")`.

- For additional details about microdata and NHGIS extract requests, see
  `vignette("ipums-api-micro")` and `vignette("ipums-api-nhgis")`.

- To work with labelled values in IPUMS data, see
  `vignette("value-labels")`.

- For techniques for working with large data extracts, see
  `vignette("ipums-bigdata")`.

The [IPUMS support website](https://www.ipums.org/support) also houses
many project-specific R-based [training
exercises](https://www.ipums.org/support/exercises). However, note that
some of these exercises may not be be up to date with ipumsr’s current
functionality.

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
Guidelines](https://tech.popdata.org/ipumsr/CONTRIBUTING.html) and the
[Code of Conduct](https://tech.popdata.org/ipumsr/CODE_OF_CONDUCT.html).

If you have general questions or concerns about IPUMS data, check out
our [user forum](https://forum.ipums.org) or send an email to
<ipums@umn.edu>.
