# Get path to IPUMS example datasets

Construct file path to example extracts included with ipumsr. These data
are used in package examples and can be used to experiment with ipumsr
functionality.

## Usage

``` r
ipums_example(path = NULL)
```

## Arguments

- path:

  Name of file. If `NULL`, all available example files will be listed.

## Value

The path to a specific example file or a vector of all available files.

## Examples

``` r
# List all available example files
ipums_example()
#>  [1] "cps_00097.dat.gz"          "cps_00097.xml"            
#>  [3] "cps_00157.dat.gz"          "cps_00157.xml"            
#>  [5] "cps_00158.csv.gz"          "cps_00158.xml"            
#>  [7] "cps_00159.dat.gz"          "cps_00159.xml"            
#>  [9] "cps_00160.dat.gz"          "cps_00160.xml"            
#> [11] "ihgis0014.zip"             "nhgis0712_csv.zip"        
#> [13] "nhgis0712_shape_small.zip" "nhgis0730_fixed.zip"      
#> [15] "nhgis0731_csv.zip"         "nhgis0972_csv.zip"        
#> [17] "nhgis0972_shape_small.zip"

# Get path to a specific example file
file <- ipums_example("cps_00157.xml")

read_ipums_micro(file)
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
#> # A tibble: 7,668 × 8
#>     YEAR SERIAL MONTH     ASECWTH STATEFIP       PERNUM ASECWT INCTOT           
#>    <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>       <dbl>  <dbl> <dbl+lbl>        
#>  1  1962     80 3 [March]   1476. 55 [Wisconsin]      1  1476.      4883        
#>  2  1962     80 3 [March]   1476. 55 [Wisconsin]      2  1471.      5800        
#>  3  1962     80 3 [March]   1476. 55 [Wisconsin]      3  1579. 999999998 [Missi…
#>  4  1962     82 3 [March]   1598. 27 [Minnesota]      1  1598.     14015        
#>  5  1962     83 3 [March]   1707. 27 [Minnesota]      1  1707.     16552        
#>  6  1962     84 3 [March]   1790. 27 [Minnesota]      1  1790.      6375        
#>  7  1962    107 3 [March]   4355. 19 [Iowa]           1  4355. 999999999 [N.I.U…
#>  8  1962    107 3 [March]   4355. 19 [Iowa]           2  1386.         0        
#>  9  1962    107 3 [March]   4355. 19 [Iowa]           3  1629.       600        
#> 10  1962    107 3 [March]   4355. 19 [Iowa]           4  1432. 999999999 [N.I.U…
#> # ℹ 7,658 more rows
```
