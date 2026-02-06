# ipumsr

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

|                                                                                                                                      |                                                                       | Data Type      | Description                                                                                                                                                                                                                                             |                Read Data Extracts                |             Request & Download Data              |                 Browse Metadata                  |
|:------------------------------------------------------------------------------------------------------------------------------------:|:----------------------------------------------------------------------|:---------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------------------------------------------:|:------------------------------------------------:|:------------------------------------------------:|
|                     [![IPUMS USA logo](reference/figures/logo-square_usa_50x50.png)](https://usa.ipums.org/usa/)                     | [IPUMS USA](https://usa.ipums.org/usa/)                               | Microdata      | U.S. Census and American Community Survey microdata (1850-present)                                                                                                                                                                                      | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) |                                                  |
|                     [![IPUMS CPS logo](reference/figures/logo-square_cps_50x50.png)](https://cps.ipums.org/cps/)                     | [IPUMS CPS](https://cps.ipums.org/cps/)                               | Microdata      | Current Population Survey microdata including basic monthly surveys and supplements (1962-present)                                                                                                                                                      | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) |                                                  |
| [![IPUMS International logo](reference/figures/logo-square_international_50x50.png)](https://international.ipums.org/international/) | [IPUMS International](https://international.ipums.org/international/) | Microdata      | Census microdata covering over 100 countries, contemporary and historical                                                                                                                                                                               | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) |                                                  |
|                     [![IPUMS NHGIS logo](reference/figures/logo-square_nhgis50x50.png)](https://www.nhgis.org/)                      | [IPUMS NHGIS](https://www.nhgis.org/)                                 | Aggregate Data | Tabular U.S. Census data and GIS mapping files (1790-present)                                                                                                                                                                                           | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) |
|                    [![IPUMS IHGIS logo](reference/figures/logo-square_ihgis_50x50.png)](https://ihgis.ipums.org/)                    | [IPUMS IHGIS](https://ihgis.ipums.org/)                               | Aggregate Data | Tabular and GIS data from population, housing, and agricultural censuses around the world                                                                                                                                                               | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) |
|                [![IPUMS Time Use logo](reference/figures/logo-square_time-use_50x50.png)](https://timeuse.ipums.org/)                | [IPUMS Time Use](https://timeuse.ipums.org/)                          | Microdata      | Time use microdata from the U.S. (1930-present) and thirteen other countries (1965-present)                                                                                                                                                             | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) |                                                  |
|       [![IPUMS Health Surveys logo](reference/figures/logo-square_health-surveys_50x50.png)](https://healthsurveys.ipums.org/)       | [IPUMS Health Surveys](https://healthsurveys.ipums.org/)              | Microdata      | Microdata from the U.S. [National Health Interview Survey (NHIS)](https://nhis.ipums.org/nhis/) (1963-present) and [Medical Expenditure Panel Survey (MEPS)](https://meps.ipums.org/meps/) (1996-present)                                               | ![Check mark](reference/figures/check-solid.svg) | ![Check mark](reference/figures/check-solid.svg) |                                                  |
|        [![IPUMS Global Health logo](reference/figures/logo-square_global-health_50x50.png)](https://globalhealth.ipums.org/)         | [IPUMS Global Health](https://globalhealth.ipums.org/)                | Microdata      | Health survey microdata for low- and middle-income countries, including harmonized data collections for [Demographic and Health Surveys (DHS)](https://www.idhsdata.org/) and [Performance Monitoring for Action (PMA)](https://pma.ipums.org/) surveys | ![Check mark](reference/figures/check-solid.svg) |                                                  |                                                  |
|          [![IPUMS Higher Ed logo](reference/figures/logo-square_higher-ed_50x50.png)](https://highered.ipums.org/highered/)          | [IPUMS Higher Ed](https://highered.ipums.org/highered/)               | Microdata      | Survey microdata on the science and engineering workforce in the U.S. from 1993 to 2013                                                                                                                                                                 | ![Check mark](reference/figures/check-solid.svg) |                                                  |                                                  |

ipumsr uses the [IPUMS API](https://developer.ipums.org/) to submit data
requests, download data extracts, and get metadata, so the scope of
functionality generally corresponds to that [available via the
API](https://developer.ipums.org/docs/v2/apiprogram/apis/). As the IPUMS
team extends the API to support more functionality for more projects, we
aim to extend ipumsr capabilities accordingly.

## Getting started

If you’re new to IPUMS data, learn more about what’s available through
the [IPUMS Projects Overview](https://www.ipums.org/overview). Then, see
[`vignette("ipums")`](https://tech.popdata.org/ipumsr/dev/articles/ipums.md)
for an overview of how to obtain IPUMS data.

The package vignettes are the best place to explore what ipumsr has to
offer:

- To read IPUMS data extracts into R, see
  [`vignette("ipums-read")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-read.md).

- To interact with the IPUMS extract and metadata system via the IPUMS
  API, see
  [`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md).

- For additional details about microdata and aggregate data extract
  requests, see
  [`vignette("ipums-api-micro")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-micro.md)
  and
  [`vignette("ipums-api-agg")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-agg.md).

- To work with labelled values in IPUMS data, see
  [`vignette("value-labels")`](https://tech.popdata.org/ipumsr/dev/articles/value-labels.md).

- For techniques for working with large data extracts, see
  [`vignette("ipums-bigdata")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-bigdata.md).

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
