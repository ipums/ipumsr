
<!-- README.md is generated from README.Rmd. Please edit that file -->
ipumsimport
===========

The ipumsimport package helps import IPUMS extracts from the [website](https://www.ipums.org) into R. Currently it only works with microdata projects and has been tested with both rectangular and hierarchical data for CPS and IPUMS-I.

It can be installed by running the following commands:

``` r
if (!require(devtools)) install.packages("devtools")

if (Sys.info()$sysname == "Windows") {
  devtools::install_local("Z:/personal/gfellis/ipumsimport")
} else {
  devtools::install_local("/pkg/ipums/personal/gfellis/ipumsimport")
}
```

To use, create an extract on the website and put the unzipped .dat file and the DDI .xml file into the same directory. Then you can use the following commands to load a dataset.

``` r
library(ipumsimport)
ddi_file <- "C:/users/gfellis/desktop/cps_00001.xml" # path to a extract DDI.
cps_data <- ip_read_data(ddi_file)
```
