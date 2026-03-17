# Big IPUMS Data

Browsing for IPUMS data can be a little like grocery shopping when
you’re hungry—you show up to grab a couple things, but everything looks
so good that you end up with an overflowing cart.[¹](#fn1)
Unfortunately, this can lead to extracts so large that they don’t fit in
your computer’s memory.

If you’ve got an extract that’s too big, both the IPUMS website and the
ipumsr package have tools to help. There are four basic strategies:

1.  Get more memory
2.  Reduce the size of your extract
3.  Read data in “chunks” or “yields”
4.  Store data in a database

ipumsr can’t do much for you when it comes to option 1, but it can help
facilitate some of the other options.

## Setup

The examples in this vignette will rely on a few helpful packages. If
you haven’t already installed them, you can do so with:

``` r
# To run the full vignette, you'll also need the following packages. If they
# aren't installed already, do so with:
install.packages("biglm")
install.packages("DBI")
install.packages("RSQLite")
install.packages("dbplyr")
```

``` r
library(ipumsr)
library(dplyr)
```

## Option 1: Trade money for convenience

If you need to work with a dataset that’s too big for your RAM, the
simplest option is to get more space. If upgrading your hardware isn’t
an option, paying for a cloud service like Amazon or Microsoft Azure may
be worth considering. Here are guides for using R on
[Amazon](https://aws.amazon.com/blogs/opensource/getting-started-with-r-on-amazon-web-services/)
and [Microsoft
Azure](https://www.jumpingrivers.com/blog/hosting-rstudio-server-on-azure/).

Of course, this option isn’t feasible for most users—in this case,
updates to the data being used in the analysis or the processing
pipeline may be required.

## Option 2: Reduce extract size

### Remove unused data

The easiest way to reduce the size of your extract is to drop unused
samples and variables. This can be done through the extract interface
for the specific IPUMS project you’re using or within R using the IPUMS
API (for projects that are supported).

If using the API, simply updated your extract definition code to exclude
the specifications that you no longer need. Then, resubmit the extract
request and download the new files.

See the [introduction to the IPUMS
API](https://tech.popdata.org/ipumsr/articles/ipums-api.md) for more
information about making extract requests from ipumsr.

### Select cases

For microdata projects, another good option for reducing extract size is
to select only those cases that are relevant to your research question,
producing an extract containing only data for a particular subset of
values for a given variable.

If you’re using the IPUMS API, you can use
[`var_spec()`](https://tech.popdata.org/ipumsr/reference/var_spec.md) to
specify case selections for a variable in an extract definition. For
instance, the following would produce an extract only including records
for married women:

``` r
define_extract_micro(
  "usa",
  description = "2013 ACS Data for Married Women",
  samples = "us2013a",
  variables = list(
    var_spec("MARST", case_selections = "1"),
    var_spec("SEX", case_selections = "2")
  )
)
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS Data for Married Women
#> 
#> Samples: (1 total) us2013a
#> Variables: (2 total) MARST, SEX
```

If you’re using the online interface, the **Select Cases** option will
be available on the last page before submitting an extract request.

### Use a sampled subset of the data

Yet another option (also only for microdata projects) is to take a
random subsample of the data before producing your extract.

Sampled data is not available via the IPUMS API, but you can use the
**Customize Sample Size** option in the online interface to do so. This
also appears on the final page before submitting an extract request.

If you’ve already submitted the extract, you can click the **REVISE**
link on the *Download or Revise Extracts* page to access these features
and produce a new data extract.

## Option 3: Process the data in pieces

ipumsr provides two related options for reading data sources in
increments:

- *Chunked* functions allow you to specify a function that will be
  called on each chunk of data as it is read in as well as how you would
  like the chunks to be combined at the end. These functions use the
  readr
  [framework](https://readr.tidyverse.org/reference/read_delim_chunked.html)
  for reading chunked data.
- *Yielded* functions allow more flexibility by returning control to the
  user between the loading of each piece of data. These functions are
  unique to ipumsr and fixed-width data.

### Reading chunked data

Use
[`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_chunked.md)
and
[`read_ipums_micro_list_chunked()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_chunked.md)
to read data in chunks. These are analogous to the standard
[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro.md)
and
[`read_ipums_micro_list()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro.md)
functions, but allow you to specify a function that will be applied to
each data chunk and control how the results from these chunks are
combined.

Below, we’ll use chunking to outline solutions to three common use-cases
for IPUMS data: tabulation, regression and case selection.

First, we’ll load our example data. *Note that we have down-sampled the
data in this example for storage reasons; none of the output “results”
reflected in this vignette should be considered legitimate!*

``` r
cps_ddi_file <- ipums_example("cps_00097.xml")
```

#### Chunked tabulation

Imagine we wanted to find the percent of people in the workforce grouped
by their self-reported health. Since our example extract is small enough
to fit in memory, we could load the full dataset with
[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro.md),
use
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
to relabel the `EMPSTAT` variable into a binary variable, and count the
people in each group.

``` r
read_ipums_micro(cps_ddi_file, verbose = FALSE) %>%
  mutate(
    HEALTH = as_factor(HEALTH),
    AT_WORK = as_factor(
      lbl_relabel(
        EMPSTAT,
        lbl(1, "Yes") ~ .lbl == "At work",
        lbl(0, "No") ~ .lbl != "At work"
      )
    )
  ) %>%
  group_by(HEALTH, AT_WORK) %>%
  summarize(n = n(), .groups = "drop")
#> # A tibble: 10 × 3
#>    HEALTH    AT_WORK     n
#>    <fct>     <fct>   <int>
#>  1 Excellent No       4055
#>  2 Excellent Yes      2900
#>  3 Very good No       3133
#>  4 Very good Yes      3371
#>  5 Good      No       2480
#>  6 Good      Yes      2178
#>  7 Fair      No       1123
#>  8 Fair      Yes       443
#>  9 Poor      No        603
#> 10 Poor      Yes        65
```

For the sake of this example, let’s imagine we can only store 1,000 rows
in memory at a time. In this case, we need to use a `chunked` function,
tabulate for each chunk, and then calculate the counts across all of the
chunks.

The `chunked` functions will apply a user-defined callback function to
each chunk. The callback takes two arguments: `x`, which represents the
data contained in a given chunk, and `pos`, which represents the
position of the chunk, expressed as the line in the input file at which
the chunk starts. Generally you will only need to use `x`, but the
callback must always take both arguments.

In this case, the callback will implement the same processing steps that
we demonstrated above:

``` r
cb_function <- function(x, pos) {
  x %>%
    mutate(
      HEALTH = as_factor(HEALTH),
      AT_WORK = as_factor(
        lbl_relabel(
          EMPSTAT,
          lbl(1, "Yes") ~ .lbl == "At work",
          lbl(0, "No") ~ .lbl != "At work"
        )
      )
    ) %>%
    group_by(HEALTH, AT_WORK) %>%
    summarize(n = n(), .groups = "drop")
}
```

Next, we need to create a callback object, which will determine how we
want to combine the ultimate results for each chunk. ipumsr provides
three main types of callback objects that preserve variable metadata:

- `IpumsDataFrameCallback` combines the results from each chunk together
  by row binding them together
- `IpumsListCallback` returns a list with one item per chunk containing
  the results for that chunk. Use this when you don’t want to (or can’t)
  immediately combine the results.
- `IpumsSideEffectCallback` does not return any results. Use this when
  your callback function is intended only for its side effects (for
  instance, if you are saving the results for each chunk to disk).

(ipumsr also provides a fourth callback used for running linear
regression models discussed [below](#chunked-reg)).

In this case, we want to row-bind the data frames returned by
`cb_function()`, so we use `IpumsDataFrameCallback`.

Callback objects are [R6](https://r6.r-lib.org) objects, but you don’t
need to be familiar with R6 to use them.[²](#fn2) To initialize a
callback object, simply use `$new()`:

``` r
cb <- IpumsDataFrameCallback$new(cb_function)
```

At this point, we’re ready to load the data in chunks. We use
[`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_chunked.md)
to specify the callback and chunk size:

``` r
chunked_tabulations <- read_ipums_micro_chunked(
  cps_ddi_file,
  callback = cb,
  chunk_size = 1000,
  verbose = FALSE
)

chunked_tabulations
#> # A tibble: 209 × 3
#>    HEALTH    AT_WORK     n
#>    <fct>     <fct>   <int>
#>  1 Excellent No        183
#>  2 Excellent Yes       147
#>  3 Very good No        134
#>  4 Very good Yes       217
#>  5 Good      No        111
#>  6 Good      Yes       105
#>  7 Fair      No         53
#>  8 Fair      Yes        22
#>  9 Poor      No         27
#> 10 Poor      Yes         1
#> # ℹ 199 more rows
```

Now we have a data frame with the counts by health and work status
within each chunk. To get the full table, we just need to sum by health
and work status one more time:

``` r
chunked_tabulations %>%
  group_by(HEALTH, AT_WORK) %>%
  summarize(n = sum(n), .groups = "drop")
#> # A tibble: 10 × 3
#>    HEALTH    AT_WORK     n
#>    <fct>     <fct>   <int>
#>  1 Excellent No       4055
#>  2 Excellent Yes      2900
#>  3 Very good No       3133
#>  4 Very good Yes      3371
#>  5 Good      No       2480
#>  6 Good      Yes      2178
#>  7 Fair      No       1123
#>  8 Fair      Yes       443
#>  9 Poor      No        603
#> 10 Poor      Yes        65
```

#### Chunked regression

With the biglm package, it is possible to use R to perform a regression
on data that is too large to store in memory all at once. The ipumsr
package provides another callback designed to make this simple:
`IpumsBiglmCallback`.

In this example, we’ll conduct a regression with total hours worked
(`AHRSWORKT`) as the outcome and age (`AGE`) and self-reported health
(`HEALTH`) as predictors. (Note that this is intended as a code
demonstration, so we ignore many complexities that should be addressed
in real analyses.)

If we were running the analysis on our full dataset, we’d first load our
data and prepare the variables in our analysis for use in the model:

``` r
data <- read_ipums_micro(cps_ddi_file, verbose = FALSE) %>%
  mutate(
    HEALTH = as_factor(HEALTH),
    AHRSWORKT = lbl_na_if(AHRSWORKT, ~ .lbl == "NIU (Not in universe)"),
    AT_WORK = as_factor(
      lbl_relabel(
        EMPSTAT,
        lbl(1, "Yes") ~ .lbl == "At work",
        lbl(0, "No") ~ .lbl != "At work"
      )
    )
  ) %>%
  filter(AT_WORK == "Yes")
```

Then, we’d provide our model formula and data to `lm`:

``` r
model <- lm(AHRSWORKT ~ AGE + I(AGE^2) + HEALTH, data = data)
summary(model)
#> 
#> Call:
#> lm(formula = AHRSWORKT ~ AGE + I(AGE^2) + HEALTH, data = data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -41.217  -4.734  -0.077   5.957  63.994 
#> 
#> Coefficients:
#>                   Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)      5.2440289  1.1823985   4.435 9.31e-06 ***
#> AGE              1.5868169  0.0573268  27.680  < 2e-16 ***
#> I(AGE^2)        -0.0170043  0.0006568 -25.888  < 2e-16 ***
#> HEALTHVery good -0.2550306  0.3276759  -0.778 0.436412    
#> HEALTHGood      -0.9637395  0.3704123  -2.602 0.009289 ** 
#> HEALTHFair      -3.8899430  0.6629725  -5.867 4.58e-09 ***
#> HEALTHPoor      -5.7597200  1.6197136  -3.556 0.000378 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 12.88 on 8950 degrees of freedom
#> Multiple R-squared:  0.08711,    Adjusted R-squared:  0.0865 
#> F-statistic: 142.3 on 6 and 8950 DF,  p-value: < 2.2e-16
```

To do the same regression, but with only 1,000 rows loaded at a time, we
work in a similar manner.

First we make an `IpumsBiglmCallback` callback object. We provide the
model formula as well as the code used to process the data before
running the regression:

``` r
library(biglm)
#> Loading required package: DBI
```

``` r
biglm_cb <- IpumsBiglmCallback$new(
  model = AHRSWORKT ~ AGE + I(AGE^2) + HEALTH,
  prep = function(x, pos) {
    x %>%
      mutate(
        HEALTH = as_factor(HEALTH),
        AHRSWORKT = lbl_na_if(AHRSWORKT, ~ .lbl == "NIU (Not in universe)"),
        AT_WORK = as_factor(
          lbl_relabel(
            EMPSTAT,
            lbl(1, "Yes") ~ .lbl == "At work",
            lbl(0, "No") ~ .lbl != "At work"
          )
        )
      ) %>%
      filter(AT_WORK == "Yes")
  }
)
```

And then we read the data using
[`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_chunked.md),
passing the callback that we just made.

``` r
chunked_model <- read_ipums_micro_chunked(
  cps_ddi_file,
  callback = biglm_cb,
  chunk_size = 1000,
  verbose = FALSE
)

summary(chunked_model)
#> Large data regression model: biglm(AHRSWORKT ~ AGE + I(AGE^2) + HEALTH, data, ...)
#> Sample size =  8957 
#>                    Coef    (95%     CI)     SE      p
#> (Intercept)      5.2440  2.8792  7.6088 1.1824 0.0000
#> AGE              1.5868  1.4722  1.7015 0.0573 0.0000
#> I(AGE^2)        -0.0170 -0.0183 -0.0157 0.0007 0.0000
#> HEALTHVery good -0.2550 -0.9104  0.4003 0.3277 0.4364
#> HEALTHGood      -0.9637 -1.7046 -0.2229 0.3704 0.0093
#> HEALTHFair      -3.8899 -5.2159 -2.5640 0.6630 0.0000
#> HEALTHPoor      -5.7597 -8.9991 -2.5203 1.6197 0.0004
```

### Reading yielded data

In addition to chunked reading, ipumsr also provides the similar but
more flexible “yielded” reading.

[`read_ipums_micro_yield()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_yield.md)
and
[`read_ipums_micro_list_yield()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_yield.md)
grant you more freedom in determining what R code to run between chunks
and include the ability to have multiple files open at once.
Additionally, yields are compatible with the `bigglm` function from
biglm, which allows you to run glm models on data larger than memory.

The downside to this greater control is that yields have an API that is
unique to IPUMS data and the way they work is unusual for R code.

#### Yielded tabulation

We’ll compare the `yield` and `chunked` functions by conducting the same
[tabulation example](#chunked-tab) from above using yields.

First, we create the yield object with the function
[`read_ipums_micro_yield()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_yield.md):

``` r
data <- read_ipums_micro_yield(cps_ddi_file, verbose = FALSE)
```

This function returns an `R6` object which contains methods for reading
the data. The most important method is the `yield()` method which will
return `n` rows of data:

``` r
# Return the first 10 rows of data
data$yield(10)
#> # A tibble: 10 × 14
#>     YEAR SERIAL MONTH      CPSID ASECFLAG ASECWTH FOODSTMP PERNUM  CPSIDP ASECWT
#>    <dbl>  <dbl> <int+lb>   <dbl> <int+lb>   <dbl> <int+lb>  <dbl>   <dbl>  <dbl>
#>  1  2011     33 3 [Marc… 2.01e13 1 [ASEC]    308. 1 [No]        1 2.01e13   308.
#>  2  2011     33 3 [Marc… 2.01e13 1 [ASEC]    308. 1 [No]        2 2.01e13   217.
#>  3  2011     33 3 [Marc… 2.01e13 1 [ASEC]    308. 1 [No]        3 2.01e13   249.
#>  4  2011     46 3 [Marc… 2.01e13 1 [ASEC]    266. 1 [No]        1 2.01e13   266.
#>  5  2011     46 3 [Marc… 2.01e13 1 [ASEC]    266. 1 [No]        2 2.01e13   266.
#>  6  2011     46 3 [Marc… 2.01e13 1 [ASEC]    266. 1 [No]        3 2.01e13   265.
#>  7  2011     46 3 [Marc… 2.01e13 1 [ASEC]    266. 1 [No]        4 2.01e13   296.
#>  8  2011     64 3 [Marc… 2.01e13 1 [ASEC]    241. 1 [No]        1 2.01e13   241.
#>  9  2011     64 3 [Marc… 2.01e13 1 [ASEC]    241. 1 [No]        2 2.01e13   241.
#> 10  2011     64 3 [Marc… 2.01e13 1 [ASEC]    241. 1 [No]        3 2.01e13   278.
#> # ℹ 4 more variables: AGE <int+lbl>, EMPSTAT <int+lbl>, AHRSWORKT <dbl+lbl>,
#> #   HEALTH <int+lbl>
```

Note that the row position in the data is stored in the object, so
running the same code again will produce *different* rows of data:

``` r
# Return the next 10 rows of data
data$yield(10)
#> # A tibble: 10 × 14
#>     YEAR SERIAL MONTH      CPSID ASECFLAG ASECWTH FOODSTMP PERNUM  CPSIDP ASECWT
#>    <dbl>  <dbl> <int+lb>   <dbl> <int+lb>   <dbl> <int+lb>  <dbl>   <dbl>  <dbl>
#>  1  2011     82 3 [Marc… 0       1 [ASEC]    373. 1 [No]        1 0         373.
#>  2  2011     82 3 [Marc… 0       1 [ASEC]    373. 1 [No]        2 0         373.
#>  3  2011     82 3 [Marc… 0       1 [ASEC]    373. 1 [No]        3 0         326.
#>  4  2011     86 3 [Marc… 2.01e13 1 [ASEC]    554. 1 [No]        1 2.01e13   554.
#>  5  2011    104 3 [Marc… 2.01e13 1 [ASEC]    543. 1 [No]        1 2.01e13   543.
#>  6  2011    104 3 [Marc… 2.01e13 1 [ASEC]    543. 1 [No]        2 2.01e13   543.
#>  7  2011    106 3 [Marc… 2.01e13 1 [ASEC]    543. 1 [No]        1 2.01e13   543.
#>  8  2011    137 3 [Marc… 2.01e13 1 [ASEC]    271. 1 [No]        1 2.01e13   271.
#>  9  2011    137 3 [Marc… 2.01e13 1 [ASEC]    271. 1 [No]        2 2.01e13   271.
#> 10  2011    137 3 [Marc… 2.01e13 1 [ASEC]    271. 1 [No]        3 2.01e13   365.
#> # ℹ 4 more variables: AGE <int+lbl>, EMPSTAT <int+lbl>, AHRSWORKT <dbl+lbl>,
#> #   HEALTH <int+lbl>
```

Use `cur_pos` to get the current position in the data file:

``` r
data$cur_pos
#> [1] 21
```

The `is_done()` method tells us whether we have read the entire file
yet:

``` r
data$is_done()
#> [1] FALSE
```

In preparation for our actual example, we’ll use `reset()` to reset to
the beginning of the data:

``` r
data$reset()
```

Using `yield()` and `is_done()`, we can set up our processing pipeline.
First, we create an empty placeholder tibble to store our results:

``` r
yield_results <- tibble(
  HEALTH = factor(levels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
  AT_WORK = factor(levels = c("No", "Yes")),
  n = integer(0)
)
```

Then, we iterate through the data, yielding 1,000 rows at a time and
processing the results as we did in the chunked example. The iteration
will end when we’ve finished reading the entire file.

``` r
while (!data$is_done()) {
  # Yield new data and process
  new <- data$yield(n = 1000) %>%
    mutate(
      HEALTH = as_factor(HEALTH),
      AT_WORK = as_factor(
        lbl_relabel(
          EMPSTAT,
          lbl(1, "Yes") ~ .lbl == "At work",
          lbl(0, "No") ~ .lbl != "At work"
        )
      )
    ) %>%
    group_by(HEALTH, AT_WORK) %>%
    summarize(n = n(), .groups = "drop")

  # Combine the new yield with the previously processed yields
  yield_results <- bind_rows(yield_results, new) %>%
    group_by(HEALTH, AT_WORK) %>%
    summarize(n = sum(n), .groups = "drop")
}

yield_results
#> # A tibble: 10 × 3
#>    HEALTH    AT_WORK     n
#>    <fct>     <fct>   <int>
#>  1 Excellent No       4055
#>  2 Excellent Yes      2900
#>  3 Very good No       3133
#>  4 Very good Yes      3371
#>  5 Good      No       2480
#>  6 Good      Yes      2178
#>  7 Fair      No       1123
#>  8 Fair      Yes       443
#>  9 Poor      No        603
#> 10 Poor      Yes        65
```

#### Yielded GLM regression

One of the major benefits of the yielded reading over chunked reading is
that it is compatible with the GLM functions from biglm, allowing for
the use of more complicated models.

To run a logistic regression, we first need to reset our yield object
from the previous example:

``` r
data$reset()
```

Next we make a function that takes a single argument: `reset`. When
`reset` is `TRUE`, it resets the data to the beginning. This is dictated
by `bigglm` from biglm.

To create this function, we use the the `reset()` method from the yield
object:

``` r
get_model_data <- function(reset) {
  if (reset) {
    data$reset()
  } else {
    yield <- data$yield(n = 1000)

    if (is.null(yield)) {
      return(yield)
    }

    yield %>%
      mutate(
        HEALTH = as_factor(HEALTH),
        WORK30PLUS = lbl_na_if(AHRSWORKT, ~ .lbl == "NIU (Not in universe)") >= 30,
        AT_WORK = as_factor(
          lbl_relabel(
            EMPSTAT,
            lbl(1, "Yes") ~ .lbl == "At work",
            lbl(0, "No") ~ .lbl != "At work"
          )
        )
      ) %>%
      filter(AT_WORK == "Yes")
  }
}
```

Finally we feed this function and a model specification to the
[`bigglm()`](https://rdrr.io/pkg/biglm/man/bigglm.html) function:

``` r
results <- bigglm(
  WORK30PLUS ~ AGE + I(AGE^2) + HEALTH,
  family = binomial(link = "logit"),
  data = get_model_data
)

summary(results)
#> Large data regression model: bigglm(WORK30PLUS ~ AGE + I(AGE^2) + HEALTH, family = binomial(link = "logit"), 
#>     data = get_model_data)
#> Sample size =  8957 
#>                    Coef    (95%     CI)     SE      p
#> (Intercept)     -4.0021 -4.4297 -3.5744 0.2138 0.0000
#> AGE              0.2714  0.2498  0.2930 0.0108 0.0000
#> I(AGE^2)        -0.0029 -0.0032 -0.0027 0.0001 0.0000
#> HEALTHVery good  0.0038 -0.1346  0.1423 0.0692 0.9557
#> HEALTHGood      -0.1129 -0.2685  0.0426 0.0778 0.1465
#> HEALTHFair      -0.6637 -0.9160 -0.4115 0.1261 0.0000
#> HEALTHPoor      -0.7879 -1.3697 -0.2062 0.2909 0.0068
```

## Option 4: Use a database

Storing your data in a database is another way to work with data that
cannot fit into memory as a data frame. If you have access to a database
on a remote machine, then you can easily select and use parts of the
data for your analysis. Even databases on your own machine may provide
more efficient data storage or use your hard drive, enabling the data to
be loaded into R.

There are many different kinds of databases, each with their own
benefits and drawbacks, and the database you choose to use will be
specific to your use case. However, once you’ve chosen a database, there
will be two general steps:

1.  Importing data into the database
2.  Connecting the database to R

R has several tools that support database integration, including
[DBI](https://dbi.r-dbi.org), [dbplyr](https://dbplyr.tidyverse.org/),
[sparklyr](https://spark.posit.co/),
[bigrquery](https://bigrquery.r-dbi.org), and others. In this example,
we’ll use [RSQLite](https://rsqlite.r-dbi.org) to load the data into an
in-memory database. (We use RSQLite because it is easy to set up, but it
is likely not efficient enough to fully resolve issues with large IPUMS
data, so it may be wise to consider an alternative in practice.)

#### Importing data into the database

For rectangular extracts, it is likely simplest to load your data into
the database in CSV format, which is widely supported. If you are
working with a hierarchical extract (or your database software doesn’t
support CSV format), then you can use an ipumsr `chunked` function to
load the data into a database without needing to store the entire
dataset in R.

See the [IPUMS data reading
vignette](https://tech.popdata.org/ipumsr/articles/ipums-read.html#hierarchical-extracts)
for more about rectangular vs. hierarchical extracts.

``` r
library(DBI)
library(RSQLite)

# Connect to database
con <- dbConnect(SQLite(), path = ":memory:")

# Load file metadata
ddi <- read_ipums_ddi(cps_ddi_file)

# Write data to database in chunks
read_ipums_micro_chunked(
  ddi,
  readr::SideEffectChunkCallback$new(
    function(x, pos) {
      if (pos == 1) {
        dbWriteTable(con, "cps", x)
      } else {
        dbWriteTable(con, "cps", x, row.names = FALSE, append = TRUE)
      }
    }
  ),
  chunk_size = 1000,
  verbose = FALSE
)
```

#### Connecting to a database with dbplyr

There are a variety of ways to access your data once it is stored in the
database. In this example, we use dbplyr. For more details about dbplyr,
see
[`vignette("dbplyr", package = "dbplyr")`](https://dbplyr.tidyverse.org/articles/dbplyr.html).

To run a simple query for `AGE`, we can use the same syntax we would use
with dplyr:

``` r
example <- tbl(con, "cps")

example %>%
  filter("AGE" > 25)
#> # Source:   SQL [?? x 14]
#> # Database: sqlite 3.51.2 []
#>     YEAR SERIAL MONTH   CPSID ASECFLAG ASECWTH FOODSTMP PERNUM  CPSIDP ASECWT
#>    <dbl>  <dbl> <int>   <dbl>    <int>   <dbl>    <int>  <dbl>   <dbl>  <dbl>
#>  1  2011     33     3 2.01e13        1    308.        1      1 2.01e13   308.
#>  2  2011     33     3 2.01e13        1    308.        1      2 2.01e13   217.
#>  3  2011     33     3 2.01e13        1    308.        1      3 2.01e13   249.
#>  4  2011     46     3 2.01e13        1    266.        1      1 2.01e13   266.
#>  5  2011     46     3 2.01e13        1    266.        1      2 2.01e13   266.
#>  6  2011     46     3 2.01e13        1    266.        1      3 2.01e13   265.
#>  7  2011     46     3 2.01e13        1    266.        1      4 2.01e13   296.
#>  8  2011     64     3 2.01e13        1    241.        1      1 2.01e13   241.
#>  9  2011     64     3 2.01e13        1    241.        1      2 2.01e13   241.
#> 10  2011     64     3 2.01e13        1    241.        1      3 2.01e13   278.
#> # ℹ more rows
#> # ℹ 4 more variables: AGE <int>, EMPSTAT <int>, AHRSWORKT <dbl>, HEALTH <int>
```

dbplyr shows us a nice preview of the first rows of the result of our
query, but the data still exist only in the database. You can use
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to load the full results of the query into the current R session.
However, this would omit the variable metadata attached to IPUMS data,
since the database doesn’t store this metadata:

``` r
data <- example %>%
  filter("AGE" > 25) %>%
  collect()

# Variable metadata is missing
ipums_val_labels(data$MONTH)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: val <dbl>, lbl <chr>
```

Instead, use
[`ipums_collect()`](https://tech.popdata.org/ipumsr/reference/ipums_collect.md),
which uses a provided `ipums_ddi` object to reattach the metadata while
loading into the R environment:

``` r
data <- example %>%
  filter("AGE" > 25) %>%
  ipums_collect(ddi)

ipums_val_labels(data$MONTH)
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

See the [value labels
vignette](https://tech.popdata.org/ipumsr/articles/value-labels.md) more
about variable metadata in IPUMS data.

## Learning more

Big data isn’t just a problem for IPUMS users, so there are many R
resources available.

See the documentation for the packages mentioned in the
[databases](#databases) section for more information about those
options.

For some past blog posts and articles on the topic, see the following:

- [*Big Data in
  R*](https://github.com/smooney27/RWorkshopSER2024/blob/main/EPIC_R_BigData.pdf) -
  Part of Stephen Mooney’s EPIC: Epidemiologic Analysis Using R, June
  2015 class
- [*Statistical Analysis with Open-Source R and RStudio on Amazon
  EMR*](https://aws.amazon.com/blogs/big-data/statistical-analysis-with-open-source-r-and-rstudio-on-amazon-emr/) -
  Markus Schmidberger on the AWS Big Data Blog
- [*Hosting RStudio Server on
  Azure*](https://www.jumpingrivers.com/blog/hosting-rstudio-server-on-azure/) -
  Colin Gillespie’s blog post on using RStudio on Azure
- [*Building Data
  Highways*](https://r-consortium.org/posts/building-data-highways-kirill-mullers-journey-in-enhancing-rs-database/) -
  Interview with Kirill Müller, author of the DBI package

------------------------------------------------------------------------

1.  Bonus joke: why is the IPUMS website better than any grocery store?
    More free samples.

2.  If you’re interested in learning more about R6, check out Hadley
    Wickham’s [Advanced R](https://adv-r.hadley.nz/r6.html?q=R6#r6)
    book, which is available for free online.
