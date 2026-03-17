# Get file information for an IPUMS extract

Get information about the IPUMS project, date, notes, conditions, and
citation requirements for an extract based on an
[ipums_ddi](https://tech.popdata.org/ipumsr/reference/ipums_ddi-class.md)
object.

`ipums_conditions()` is a convenience function that provides conditions
and citation information for a recently loaded dataset.

## Usage

``` r
ipums_file_info(object, type = NULL)

ipums_conditions(object = NULL)
```

## Arguments

- object:

  An `ipums_ddi` object.

  For `ipums_conditions()`, leave `NULL` to display conditions for most
  recently loaded dataset.

- type:

  Type of file information to display. If `NULL`, loads all types.
  Otherwise, one of `"ipums_project"`, `"extract_date"`,
  `"extract_notes"`, `"conditions"` or `"citation"`.

## Value

For `ipums_file_info()`, if `type = NULL`, a named list of metadata
information. Otherwise, a string containing the requested information.

## Examples

``` r
ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))

ipums_file_info(ddi)
#> $ipums_project
#> [1] "IPUMS CPS"
#> 
#> $extract_date
#> [1] "2023-07-10"
#> 
#> $extract_notes
#> [1] "User-provided description:  Reproducing cps00006"
#> 
#> $conditions
#> [1] "Users of IPUMS-CPS data must agree to abide by the conditions of use. A user's license is valid for one year and may be renewed.  Users must agree to the following conditions:\n\n(1) No fees may be charged for use or distribution of the data.  All persons are granted a limited license to use these data, but you may not charge a fee for the data if you distribute it to others.\n\n(2) Cite IPUMS appropriately.  For information on proper citation,  refer to the citation requirement section of this DDI document.\n\n(3) Tell us about any work you do using the IPUMS.  Publications, research  reports, or presentations making use of IPUMS-CPS should be added to our  Bibliography. Continued funding for the IPUMS depends on our ability to  show our sponsor agencies that researchers are using the data for productive  purposes.\n\n(4) Use it for GOOD -- never for EVIL."
#> 
#> $citation
#> [1] "Publications and research reports based on the IPUMS-CPS database must cite it appropriately. The citation should include the following:\n\nSarah Flood, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren and Michael Westberry. Integrated Public Use Microdata Series, Current Population Survey: Version 10.0 [dataset]. Minneapolis, MN: IPUMS, 2022. https://doi.org/10.18128/D030.V10.0\n\nThe licensing agreement for use of IPUMS-CPS data requires that users supply us with the title and full citation for any publications, research reports, or educational materials making use of the data or documentation. Please add your citation to the IPUMS bibliography: http://bibliography.ipums.org/"
#> 
```
