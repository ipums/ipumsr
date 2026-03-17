# tidyselect selection language in ipumsr

Slightly modified implementation of tidyselect [selection
language](https://tidyselect.r-lib.org/reference/language.html) in
ipumsr.

### Syntax

In general, the selection language in ipumsr operates the same as in
tidyselect.

Where applicable, variables can be selected with:

- A character vector of variable names (`c("var1", "var2")`)

- A bare vector of variable names (`c(var1, var2)`)

- A selection helper from tidyselect (`starts_with("var")`). See below
  for a list of helpers.

### Primary differences

- tidyselect selection is generally intended for use with column
  variables in data.frame-like objects. In contrast, ipumsr allows
  selection language syntax in other cases as well (for instance, when
  selecting files from within a .zip archive). ipumsr functions will
  indicate whether they support the selection language.

- Selection with
  [`where()`](https://tidyselect.r-lib.org/reference/where.html) is not
  consistently supported.

### Selection helpers (from tidyselect)

- `var1`:`var10`: variables lying between `var1` on the left and `var10`
  on the right.

- `starts_with("a")`: names that start with `"a"`

- `ends_with("z")`: names that end with `"z"`

- `contains("b")`: names that contain `"b"`

- `matches("x.y")`: names that match regular expression `x.y`

- `num_range(x, 1:4)`: names following the pattern `x1, x2, ..., x4`

- `all_of(vars)`/`any_of(vars)`: matches names stored in the character
  vector `vars`. `all_of(vars)` will error if the variables aren't
  present; `any_of(vars)` will match just the variables that exist.

- [`everything()`](https://tidyselect.r-lib.org/reference/everything.html):
  all variables

- [`last_col()`](https://tidyselect.r-lib.org/reference/everything.html):
  furthest column to the right

Operators for combining those selections:

- `!selection`: only variables that don't match `selection`

- `selection1 & selection2`: only variables included in both
  `selection1` and `selection2`

- `selection1 | selection2`: all variables that match either
  `selection1` or `selection2`

## Examples

``` r
cps_file <- ipums_example("cps_00157.xml")

# Load 3 variables by name
read_ipums_micro(
  cps_file,
  vars = c("YEAR", "MONTH", "PERNUM"),
  verbose = FALSE
)
#> # A tibble: 7,668 × 3
#>     YEAR MONTH     PERNUM
#>    <dbl> <int+lbl>  <dbl>
#>  1  1962 3 [March]      1
#>  2  1962 3 [March]      2
#>  3  1962 3 [March]      3
#>  4  1962 3 [March]      1
#>  5  1962 3 [March]      1
#>  6  1962 3 [March]      1
#>  7  1962 3 [March]      1
#>  8  1962 3 [March]      2
#>  9  1962 3 [March]      3
#> 10  1962 3 [March]      4
#> # ℹ 7,658 more rows

# "Bare" variables are supported
read_ipums_micro(
  cps_file,
  vars = c(YEAR, MONTH, PERNUM),
  verbose = FALSE
)
#> # A tibble: 7,668 × 3
#>     YEAR MONTH     PERNUM
#>    <dbl> <int+lbl>  <dbl>
#>  1  1962 3 [March]      1
#>  2  1962 3 [March]      2
#>  3  1962 3 [March]      3
#>  4  1962 3 [March]      1
#>  5  1962 3 [March]      1
#>  6  1962 3 [March]      1
#>  7  1962 3 [March]      1
#>  8  1962 3 [March]      2
#>  9  1962 3 [March]      3
#> 10  1962 3 [March]      4
#> # ℹ 7,658 more rows

# Standard tidyselect selectors are also supported
read_ipums_micro(cps_file, vars = starts_with("ASEC"), verbose = FALSE)
#> # A tibble: 7,668 × 2
#>    ASECWTH ASECWT
#>      <dbl>  <dbl>
#>  1   1476.  1476.
#>  2   1476.  1471.
#>  3   1476.  1579.
#>  4   1598.  1598.
#>  5   1707.  1707.
#>  6   1790.  1790.
#>  7   4355.  4355.
#>  8   4355.  1386.
#>  9   4355.  1629.
#> 10   4355.  1432.
#> # ℹ 7,658 more rows

# Selection methods can be combined
read_ipums_micro(
  cps_file,
  vars = c(YEAR, MONTH, contains("INC")),
  verbose = FALSE
)
#> # A tibble: 7,668 × 3
#>     YEAR MONTH     INCTOT                               
#>    <dbl> <int+lbl> <dbl+lbl>                            
#>  1  1962 3 [March]      4883                            
#>  2  1962 3 [March]      5800                            
#>  3  1962 3 [March] 999999998 [Missing. (1962-1964 only)]
#>  4  1962 3 [March]     14015                            
#>  5  1962 3 [March]     16552                            
#>  6  1962 3 [March]      6375                            
#>  7  1962 3 [March] 999999999 [N.I.U.]                   
#>  8  1962 3 [March]         0                            
#>  9  1962 3 [March]       600                            
#> 10  1962 3 [March] 999999999 [N.I.U.]                   
#> # ℹ 7,658 more rows

read_ipums_micro(
  cps_file,
  vars = starts_with("S") & ends_with("P"),
  verbose = FALSE
)
#> # A tibble: 7,668 × 1
#>    STATEFIP      
#>    <int+lbl>     
#>  1 55 [Wisconsin]
#>  2 55 [Wisconsin]
#>  3 55 [Wisconsin]
#>  4 27 [Minnesota]
#>  5 27 [Minnesota]
#>  6 27 [Minnesota]
#>  7 19 [Iowa]     
#>  8 19 [Iowa]     
#>  9 19 [Iowa]     
#> 10 19 [Iowa]     
#> # ℹ 7,658 more rows

# Other selection arguments also support this syntax.
# For instance, load a particular file based on a tidyselect match:
read_ipums_agg(
  ipums_example("nhgis0731_csv.zip"),
  file_select = contains("nominal_state"),
  verbose = FALSE
)
#> # A tibble: 84 × 28
#>    GISJOIN STATE         STATEFP STATENH A00AA1790 A00AA1800 A00AA1810 A00AA1820
#>    <chr>   <chr>         <chr>   <chr>       <dbl>     <dbl>     <dbl>     <dbl>
#>  1 G010    Alabama       01      010            NA        NA        NA    127901
#>  2 G020    Alaska        02      020            NA        NA        NA        NA
#>  3 G025    Alaska Terri… NA      025            NA        NA        NA        NA
#>  4 G040    Arizona       04      040            NA        NA        NA        NA
#>  5 G045    Arizona Terr… NA      045            NA        NA        NA        NA
#>  6 G050    Arkansas      05      050            NA        NA        NA        NA
#>  7 G055    Arkansas Ter… NA      055            NA        NA        NA     14273
#>  8 G060    California    06      060            NA        NA        NA        NA
#>  9 G080    Colorado      08      080            NA        NA        NA        NA
#> 10 G085    Colorado Ter… NA      085            NA        NA        NA        NA
#> # ℹ 74 more rows
#> # ℹ 20 more variables: A00AA1830 <dbl>, A00AA1840 <dbl>, A00AA1850 <dbl>,
#> #   A00AA1860 <dbl>, A00AA1870 <dbl>, A00AA1880 <dbl>, A00AA1890 <dbl>,
#> #   A00AA1900 <dbl>, A00AA1910 <dbl>, A00AA1920 <dbl>, A00AA1930 <dbl>,
#> #   A00AA1940 <dbl>, A00AA1950 <dbl>, A00AA1960 <dbl>, A00AA1970 <dbl>,
#> #   A00AA1980 <dbl>, A00AA1990 <dbl>, A00AA2000 <dbl>, A00AA2010 <dbl>,
#> #   A00AA2020 <dbl>
```
