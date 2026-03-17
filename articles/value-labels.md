# Value Labels in IPUMS data

## IPUMS variable metadata

IPUMS data come with three primary types of variable-level metadata:

- ***Variable labels*** are succinct labels that serve as human-readable
  variable names (in contrast to more esoteric column names).

- ***Variable descriptions*** are extended text descriptions of the
  contents of a variable. These provide more information about what a
  given variable measures.

- ***Value labels*** link particular data values to more meaningful text
  labels. For instance, the `HEALTH` variable may have data values
  including `1` and `2`, but these are actually stand-ins for
  “Excellent” and “Very good” health. This mapping would be contained in
  a value-label pair that includes a value and its associated label.

The rest of this article will focus on value labels; for more about
variable labels and descriptions, see
[`?ipums_var_info`](https://tech.popdata.org/ipumsr/reference/ipums_var_info.md).

## Value labels

ipumsr uses the
[`labelled`](https://haven.tidyverse.org/reference/labelled.html) class
from the [haven](https://haven.tidyverse.org) package to handle value
labels.

You can see this in the column data types when loading IPUMS data. Note
that `<int+lbl>` appears below `MONTH` and `ASECFLAG`:

``` r
library(ipumsr)

ddi <- read_ipums_ddi(ipums_example("cps_00160.xml"))
cps <- read_ipums_micro(ddi, verbose = FALSE)

cps[, 1:5]
#> # A tibble: 10,883 × 5
#>     YEAR SERIAL MONTH       CPSID ASECFLAG 
#>    <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>
#>  1  2016  24138 3 [March] 2.02e13 1 [ASEC] 
#>  2  2016  24139 3 [March] 2.02e13 1 [ASEC] 
#>  3  2016  24139 3 [March] 2.02e13 1 [ASEC] 
#>  4  2016  24140 3 [March] 2.02e13 1 [ASEC] 
#>  5  2016  24140 3 [March] 2.02e13 1 [ASEC] 
#>  6  2016  24140 3 [March] 2.02e13 1 [ASEC] 
#>  7  2016  24141 3 [March] 2.02e13 1 [ASEC] 
#>  8  2016  24142 3 [March] 2.02e13 1 [ASEC] 
#>  9  2016  24142 3 [March] 2.02e13 1 [ASEC] 
#> 10  2016  24142 3 [March] 2.02e13 1 [ASEC] 
#> # ℹ 10,873 more rows
```

This indicates that the data contained in these columns are integers but
include value labels. You can use the function
[`is.labelled()`](https://haven.tidyverse.org/reference/labelled.html)
to determine if a variable is indeed labelled:

``` r
is.labelled(cps$STATEFIP)
#> [1] TRUE
```

Some of the labels are actually printed inline alongside their data
values, but it can be easier to see them by isolating them:

``` r
# Labels print when accessing the column
head(cps$MONTH)
#> <labelled<integer>[6]>: Month
#> [1] 3 3 3 3 3 3
#> 
#> Labels:
#>  value     label
#>      1   January
#>      2  February
#>      3     March
#>      4     April
#>      5       May
#>      6      June
#>      7      July
#>      8    August
#>      9 September
#>     10   October
#>     11  November
#>     12  December
```

``` r
# Get labels alone
ipums_val_labels(cps$MONTH)
#> # A tibble: 12 × 2
#>      val lbl      
#>    <int> <chr>    
#>  1     1 January  
#>  2     2 February 
#>  3     3 March    
#>  4     4 April    
#>  5     5 May      
#>  6     6 June     
#>  7     7 July     
#>  8     8 August   
#>  9     9 September
#> 10    10 October  
#> 11    11 November 
#> 12    12 December
```

## `labelled` vs. `factor`

Base R already supports the linking of numeric data to categories using
its `factor` data type. While factors may be more familiar, they were
designed to support efficient calculations in linear models, not as a
human-readable labeling system for interpreting and processing data.

Compared to factors, `labelled` vectors have two main properties that
make them more suitable for working with IPUMS data:

- They don’t require that all values be labelled
- They don’t require values to be assigned to increasing integers
  starting at 1

Consider the case of the `AGE` variable. For many IPUMS products, `AGE`
provides a person’s age in years, but certain special values have other
interpretations:

``` r
head(cps$AGE)
#> <labelled<integer>[6]>: Age
#> [1] 54 54 52 38 15 38
#> 
#> Labels:
#>  value               label
#>      0        Under 1 year
#>     90 90 (90+, 1988-2002)
#>     99                 99+
```

As you can see, the 0 value represents all ages less than 1, and the 90
and 99 values actually represent ranges of ages. Coercing `AGE` to a
factor would convert all values of 0 to 1, because factors always assign
values starting at 1:

``` r
cps$AGE_FACTOR <- as_factor(cps$AGE)

age0_factor <- cps[cps$AGE == 0, ]$AGE_FACTOR

# The levels look the same
unique(age0_factor)
#> [1] Under 1 year
#> 84 Levels: Under 1 year 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ... 99+
```

``` r
# But the values have changed
unique(as.numeric(age0_factor))
#> [1] 1
```

Additionally, because not all values exist in the data, high values,
like 85, 90, and 99 have been mapped to lower values:

``` r
age85_factor <- cps[cps$AGE == 85, ]$AGE_FACTOR

unique(as.numeric(age85_factor))
#> [1] 82
```

These different representations lead to inconsistencies in calculated
values:

``` r
mean(cps$AGE)
#> [1] 35.0226
```

``` r
mean(as.numeric(cps$AGE_FACTOR))
#> [1] 35.94836
```

### Cautions regarding `labelled` variables

While `labelled` variables provide the benefits described above, they
also present challenges.

For example, you may have noticed that *both* of the means calculated
above are suspect:

- In the case of `AGE_FACTOR`, the values have been remapped during
  conversion and several are inconsistent with the original data.
- In the case of `AGE`, we have considered all people over 90 to be
  exactly 90, and all people over 99 to be exactly 99—`labelled`
  variables don’t ensure that calculations are correct any more than
  factors do!

Furthermore, many R functions ignore value labels or even actively
remove them from the data:

``` r
ipums_val_labels(cps$HEALTH)
#> # A tibble: 5 × 2
#>     val lbl      
#>   <int> <chr>    
#> 1     1 Excellent
#> 2     2 Very good
#> 3     3 Good     
#> 4     4 Fair     
#> 5     5 Poor
```

``` r
HEALTH2 <- ifelse(cps$HEALTH > 3, 3, cps$HEALTH)

ipums_val_labels(HEALTH2)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: val <dbl>, lbl <chr>
```

So, `labelled` vectors are not intended for use throughout the entire
analysis process. Instead, they should be used during the initial data
preparation process to convert raw data into values that are more
meaningful. These can then be converted to other variable types (often
factors) for analysis.

Unfortunately, this isn’t a process that can typically be automated, as
it depends primarily on the research questions the data will be used to
address. However, ipumsr provides several functions to manipulate value
labels to make this process easier.

## Prepping data with value labels

### Convert labelled values to other data types

Use
[`as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)[¹](#fn1)
once labels have the correct categories and need no further
manipulation. For instance, `MONTH` already has sensible categories, so
we can convert it to a factor right away:

``` r
ipums_val_labels(cps$MONTH)
#> # A tibble: 12 × 2
#>      val lbl      
#>    <int> <chr>    
#>  1     1 January  
#>  2     2 February 
#>  3     3 March    
#>  4     4 April    
#>  5     5 May      
#>  6     6 June     
#>  7     7 July     
#>  8     8 August   
#>  9     9 September
#> 10    10 October  
#> 11    11 November 
#> 12    12 December
cps$MONTH <- as_factor(cps$MONTH)
```

[`as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)
can also convert all `labelled` variables in a data frame to factors at
once. If you prefer to work with factors, you can do this conversion
immediately after loading data, and then prepare these variables using
techniques you would use for factors.

``` r
cps <- as_factor(cps)

# ... further preparation of variables as factors
```

If you prefer to handle these variables in `labelled` format, you can
use the `lbl_*` helpers first, then call
[`as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)
on the entire data frame.

Some variables may be more appropriate to use as numeric values rather
than factors. In these cases, you can simply remove the labels with
[`zap_labels()`](https://haven.tidyverse.org/reference/zap_labels.html).

`INCTOT`, which measures personal income, fits this description:

``` r
inctot_num <- zap_labels(cps$INCTOT)

typeof(inctot_num)
#> [1] "double"
```

``` r
ipums_val_labels(inctot_num)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: val <dbl>, lbl <chr>
```

Note that labelled values are not generally intended to be interpreted
as numeric values, so
[`zap_labels()`](https://haven.tidyverse.org/reference/zap_labels.html)
should only be used after labels have been properly handled. For
example, in `INCTOT`, labelled values used to identify missing values
are encoded with large numbers:

``` r
ipums_val_labels(cps$INCTOT)
#> # A tibble: 2 × 2
#>         val lbl                      
#>       <dbl> <chr>                    
#> 1 999999998 Missing. (1962-1964 only)
#> 2 999999999 N.I.U.
```

Treating these as legitimate observations will significantly skew any
calculations with this variable if not first converted to `NA`.

### Create missing values based on value labels

Many IPUMS variables use labelled values to identify missing data. This
allows for more detail about why certain observations were missing than
would be available were values loaded as `NA`.

As we saw with `INCTOT`, value labels were used to identify two types of
missing data: those that are legitimately missing and those that are not
in the universe of observations.

``` r
ipums_val_labels(cps$INCTOT)
#> # A tibble: 2 × 2
#>         val lbl                      
#>       <dbl> <chr>                    
#> 1 999999998 Missing. (1962-1964 only)
#> 2 999999999 N.I.U.
```

To convert one or both of these labelled values to `NA`, use
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/reference/lbl_na_if.md).
To use
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/reference/lbl_na_if.md),
you must supply a function to handle the conversion. The function should
take a value-label pair as its input and output `TRUE` for those pairs
whose values should be converted to `NA`.

#### Syntax for value label functions

Several `lbl_*` helper functions, including
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/reference/lbl_na_if.md),
require a user-defined function to handle recoding of value-label pairs.
ipumsr provides a syntax to easily reference the values and labels in
this user-defined function:

- The `.val` argument references the *values*
- The `.lbl` argument references the *labels*

For instance, to convert all values equal to `999999999` to `NA`, we can
provide a function that uses the `.val` argument:

``` r
# Convert to NA using function that returns TRUE for all labelled values equal to 99999999
inctot_na <- lbl_na_if(
  cps$INCTOT,
  function(.val, .lbl) .val == 999999999
)

# All 99999999 values have been converted to NA
any(inctot_na == 999999999, na.rm = TRUE)
#> [1] FALSE
```

``` r
# And the label has been removed:
ipums_val_labels(inctot_na)
#> # A tibble: 1 × 2
#>         val lbl                      
#>       <dbl> <chr>                    
#> 1 999999998 Missing. (1962-1964 only)
```

We could achieve the same result by referencing the labels themselves:

``` r
# Convert to NA for labels that contain "N.I.U."
inctot_na2 <- lbl_na_if(
  cps$INCTOT,
  function(.val, .lbl) grepl("N.I.U.", .lbl)
)

# Same result
all(inctot_na2 == inctot_na, na.rm = TRUE)
#> [1] TRUE
```

You can also specify the function using a one-sided formula:

``` r
lbl_na_if(cps$INCTOT, ~ .val == 999999999)
```

Note that `.val` only refers to *labelled* values—unlabelled values are
not affected:

``` r
x <- lbl_na_if(cps$INCTOT, ~ .val >= 0)

# Unlabelled values greater than the cutoff are still present:
length(which(x > 0))
#> [1] 7501
```

To convert unlabelled values to `NA`, use
[`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html)
instead.

### Relabel values

[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
can be used to create new value-label pairs, often to recombine existing
labels into more general categories. It takes a two-sided formula to
handle the relabeling:

- On the left-hand side, use the
  [`lbl()`](https://tech.popdata.org/ipumsr/reference/lbl.md) helper to
  define a new value-label pair.
- On the right-hand side, provide a function that returns `TRUE` for
  those value-label pairs that should be relabelled with the new
  value-label pair from the left-hand side.

The function again uses the `.val` and `.lbl` [syntax](#syntax)
mentioned above to refer to values and labels, respectively.

For instance, we could reclassify the categories in `MIGRATE1` such that
all migration within a state is captured in a single category:

``` r
ipums_val_labels(cps$MIGRATE1)
#> # A tibble: 8 × 2
#>     val lbl                                 
#>   <int> <chr>                               
#> 1     0 NIU                                 
#> 2     1 Same house                          
#> 3     2 Different house, place not reported 
#> 4     3 Moved within county                 
#> 5     4 Moved within state, different county
#> 6     5 Moved between states                
#> 7     6 Abroad                              
#> 8     9 Unknown
```

``` r
cps$MIGRATE1 <- lbl_relabel(
  cps$MIGRATE1,
  lbl(0, "NIU / Missing / Unknown") ~ .val %in% c(0, 2, 9),
  lbl(1, "Stayed in state") ~ .val %in% c(1, 3, 4)
)

ipums_val_labels(cps$MIGRATE1)
#> # A tibble: 4 × 2
#>     val lbl                    
#>   <dbl> <chr>                  
#> 1     0 NIU / Missing / Unknown
#> 2     1 Stayed in state        
#> 3     5 Moved between states   
#> 4     6 Abroad
```

Many IPUMS variables include detailed labels that are grouped together
into more general categories. These are often encoded with multi-digit
values, where the starting digit refers to the larger category.

For instance, the `EDUC` variable contains categories for individual
grades as well as categories for multiple grade groups:

``` r
head(ipums_val_labels(cps$EDUC), 15)
#> # A tibble: 15 × 2
#>      val lbl                 
#>    <int> <chr>               
#>  1     0 NIU or no schooling 
#>  2     1 NIU or blank        
#>  3     2 None or preschool   
#>  4    10 Grades 1, 2, 3, or 4
#>  5    11 Grade 1             
#>  6    12 Grade 2             
#>  7    13 Grade 3             
#>  8    14 Grade 4             
#>  9    20 Grades 5 or 6       
#> 10    21 Grade 5             
#> 11    22 Grade 6             
#> 12    30 Grades 7 or 8       
#> 13    31 Grade 7             
#> 14    32 Grade 8             
#> 15    40 Grade 9
```

You could use
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
to collapse the detailed categories into the more general ones, but you
would have to define new value labels for all the categories. Instead,
you could use
[`lbl_collapse()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md).

[`lbl_collapse()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
uses a function that takes `.val` and `.lbl` [arguments](#syntax) and
returns the new value each input value should be assigned to. The label
of the lowest original value is used for each collapsed group. To group
by the tens digit, use the integer division operator `%/%`:

``` r
# %/% refers to integer division, which divides but discards the remainder
10 %/% 10
#> [1] 1
11 %/% 10
#> [1] 1
```

``` r
# Convert to groups by tens digit
cps$EDUC2 <- lbl_collapse(cps$EDUC, ~ .val %/% 10)

ipums_val_labels(cps$EDUC2)
#> # A tibble: 14 × 2
#>      val lbl                 
#>    <dbl> <chr>               
#>  1     0 NIU or no schooling 
#>  2     1 Grades 1, 2, 3, or 4
#>  3     2 Grades 5 or 6       
#>  4     3 Grades 7 or 8       
#>  5     4 Grade 9             
#>  6     5 Grade 10            
#>  7     6 Grade 11            
#>  8     7 Grade 12            
#>  9     8 1 year of college   
#> 10     9 2 years of college  
#> 11    10 3 years of college  
#> 12    11 4 years of college  
#> 13    12 5+ years of college 
#> 14    99 Missing/Unknown
```

#### Relabeling caveats

It is always worth checking that the new labels make sense based on your
research question. For instance, in the above example, both
`"12th grade, no diploma"` and `"High school diploma or equivalent"` are
collapsed to a single group as they both have values in the 70s. This
may be suitable for your purposes, but for more control, it is best to
use
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md).

Note that
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
and
[`lbl_collapse()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
only operate on *labelled* values, and are therefore designed for use
with fully `labelled` vectors. That is, if you attempt to relabel a
vector that has some unlabelled values, they will be converted to `NA`.

To avoid this, you can add labels for all values using
[`lbl_add_vals()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md)
before relabeling (see [below](#lbl_add)). In general, this shouldn’t be
necessary, as most partially-labelled vectors only include labels with
ancillary information, like missing value indicators. These can
typically be handled by other helpers, like
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/reference/lbl_na_if.md),
without requiring relabeling.

### Remove unused value labels

Some variables may contain labels for values that don’t appear in the
data. Unused levels still appear in factor representations of these
variables, so it is often beneficial to remove them with
[`lbl_clean()`](https://tech.popdata.org/ipumsr/reference/lbl_clean.md):

``` r
ipums_val_labels(cps$STATEFIP)
#> # A tibble: 75 × 2
#>      val lbl                 
#>    <int> <chr>               
#>  1     1 Alabama             
#>  2     2 Alaska              
#>  3     4 Arizona             
#>  4     5 Arkansas            
#>  5     6 California          
#>  6     8 Colorado            
#>  7     9 Connecticut         
#>  8    10 Delaware            
#>  9    11 District of Columbia
#> 10    12 Florida             
#> # ℹ 65 more rows
ipums_val_labels(lbl_clean(cps$STATEFIP))
#> # A tibble: 5 × 2
#>     val lbl         
#>   <int> <chr>       
#> 1    19 Iowa        
#> 2    27 Minnesota   
#> 3    38 North Dakota
#> 4    46 South Dakota
#> 5    55 Wisconsin
```

### Add new labels

As mentioned above, value labels are intended to be used as an
intermediate data structure for preparing newly-imported data. As such,
you’re not likely to need to add new labels, but if you do, use
[`lbl_add()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md),
[`lbl_add_vals()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md),
or
[`lbl_define()`](https://tech.popdata.org/ipumsr/reference/lbl_define.md).

[`lbl_add()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md)
takes an arbitrary number of
[`lbl()`](https://tech.popdata.org/ipumsr/reference/lbl.md) placeholders
that will be added to a given `labelled` vector:

``` r
x <- haven::labelled(
  c(100, 200, 105, 990, 999, 230),
  c(`Unknown` = 990, NIU = 999)
)

lbl_add(
  x,
  lbl(100, "$100"),
  lbl(105, "$105"),
  lbl(200, "$200"),
  lbl(230, "$230")
)
#> <labelled<double>[6]>
#> [1] 100 200 105 990 999 230
#> 
#> Labels:
#>  value   label
#>    100    $100
#>    105    $105
#>    200    $200
#>    230    $230
#>    990 Unknown
#>    999     NIU
```

[`lbl_add_vals()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md)
adds labels for all unlabelled values in a `labelled` vector with an
optional labeller function. (This can be useful if you wish to operate
on a partially labelled vector with a function that requires labelled
input, like
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md).)

``` r
# `.` refers to each label value
lbl_add_vals(x, ~ paste0("$", .))
#> <labelled<double>[6]>
#> [1] 100 200 105 990 999 230
#> 
#> Labels:
#>  value   label
#>    100    $100
#>    105    $105
#>    200    $200
#>    230    $230
#>    990 Unknown
#>    999     NIU
```

[`lbl_define()`](https://tech.popdata.org/ipumsr/reference/lbl_define.md)
makes a `labelled` vector out of an unlabelled one. Use the same syntax
as is used for
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
to define new labels based on the unlabelled values:

``` r
age <- c(10, 12, 16, 18, 20, 22, 25, 27)

# Group age values into two label groups.
# Values not captured by the right hand side functions remain unlabelled
lbl_define(
  age,
  lbl(1, "Pre-college age") ~ .val < 18,
  lbl(2, "College age") ~ .val >= 18 & .val <= 22
)
#> <labelled<double>[8]>
#> [1]  1  1  1  2  2  2 25 27
#> 
#> Labels:
#>  value           label
#>      1 Pre-college age
#>      2     College age
```

Once all labelled variables have been appropriately converted to factors
or numeric values, the data can move forward in the processing pipeline.

## Other resources

The [haven](https://haven.tidyverse.org) package, which underlies
ipumsr’s handling of value labels, provides more details on the
`labelled` class. See
[`vignette("semantics", package = "haven")`](https://haven.tidyverse.org/articles/semantics.html).

The [labelled](https://larmarange.github.io/labelled/) package provides
other methods for manipulating value labels, some of which overlap those
provided by ipumsr.

The [questionr](https://juba.github.io/questionr/) package includes
functions for exploring `labelled` variables. In particular, the
functions `describe`, `freq` and `lookfor` all print out to console
information about the variable using the value labels.

Finally, the [foreign](https://cran.r-project.org/package=foreign) and
[prettyR](https://cran.r-project.org/package=prettyR) packages don’t use
the `labelled` class, but provide similar functionality for handling
value labels, which could be adapted for use with `labelled` vectors.

------------------------------------------------------------------------

1.  Note that if you’re reading this article on the ipumsr website,
    auto-generated links for the function
    [`as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)
    point to the [forcats](https://forcats.tidyverse.org/) package, but
    the documentation for
    [`haven::as_factor.labelled()`](https://haven.tidyverse.org/reference/as_factor.html)
    is more informative for working with labelled variables.
