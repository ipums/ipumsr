# ipumsr (development version)

## IPUMS API updates

* Adds API support for IPUMS NHGIS extract requests! Use 
  `define_extract_nhgis()` to make an NHGIS extract definition.
  
* Adds API support for IPUMS NHGIS metadata. Use `get_nhgis_metadata()` to
  browse NHGIS data sources when creating an NHGIS extract request.

* `get_extract_history()` replaces `get_recent_extracts_info_*()` functions.
  Set `as_table = TRUE` to get recent extract information in tabular format.
  
* Adds `combine_extracts()` to allow for the merging of multiple extract
  definitions.
  
* Allows users to set a default IPUMS collection using
  `set_ipums_default_collection()`. Users with a default collection do not 
  need to specify the IPUMS collection in functions that require it; instead,
  the default collection is used. This is a convenience for users who rely
  primarily on a single IPUMS collection.
  
* `wait_for_extract()` wait intervals no longer double after each status
  check. Instead, intervals increase by 10 seconds with each subsequent check.
  
* Extract definitions are now validated more frequently in extract-handling 
  functions.

* Warnings encourage users to resubmit extracts that have expired. Previously,
  the distinction between expired and in-progress extract requests was not 
  always made (e.g. in `is_extract_ready()`)


## IPUMS reader updates

* Adds handling for fixed-width extracts in `read_nhgis()`.

* `read_ipums_sf()` no longer defaults to `bind_multiple = TRUE`.

* `read_nhgis_codebook()` replaces `read_ipums_codebook()` and allows reading
  of raw codebook lines (as opposed to extracting codebook information into 
  an `ipums_ddi` object) by setting `raw = TRUE`. Furthermore, `var_info`
  generated from NHGIS codebook files has been updated to include more
  contextual information about the data variables.
  
* `read_nhgis()` now supports the passing of arguments to underlying readr
  functions `readr::read_csv()` and `readr::read_fwf()`. 
  `read_nhgis()` defaults will typically be appropriate, but users now can 
  adjust data-reading parameters as needed. Note that some of these
  options may impact ipumsr reading functionality (e.g. attaching variable 
  metadata to data files).
  
* `read_nhgis()` now allows users to retain the extra header row included in
  some NHGIS files. Set `remove_extra_header = FALSE` to do so. In general,
  the information contained in the extra header is attached to the data from
  the NHGIS codebook file, but in some cases the extra header may differ
  slightly from the information found in the codebook.
  
* `ipums_example()` includes updated example files.

* It is now possible to consistently read unzipped extract files. Previously,
  some functions required that extracts be in the zipped format provided
  upon download.

## Deprecations

* `data_layer` and `shape_layer` arguments have been deprecated in favor of
  `file_select` throughout ipumsr. This provides clarity on the intended purpose
  of this argument. Deprecated functions that use the original argument names 
  remain unchanged.
  
* `get_recent_extracts_info_list()` and `get_recent_extracts_info_tbl()` have
  been deprecated in favor of `get_extract_history()`.
  
* `read_nhgis_sf()` and `read_nhgis_sp()` have been deprecated. Use
  `read_ipums_sf()` and `read_nhgis()` to load spatial and tabular
  data, respectively. Join data with an `ipums_shape_*_join()` function.
  
* Support for objects from the sp package has been deprecated because of the 
  upcoming retirement of rgdal. Use `read_ipums_sf()` to load spatial data
  as an `sf` object. To convert to a `Spatial*` object, use `sf::as_Spatial()`.
  For more, click [here](https://r-spatial.org/r/2022/04/12/evolution.html).
  
* Support for IPUMS Terra has been deprecated, as the IPUMS Terra project is
  being decommissioned. This includes all `read_terra_*()` functions. 
  For more, click [here](https://terra.ipums.org/decommissioning).
  
* `read_ipums_codebook()` has been deprecated in favor of 
  `read_nhgis_codebook()`. Previously, `read_ipums_codebook()` handled
  IPUMS Terra as well as IPUMS NHGIS extracts.
  
* `ipums_list_data()`, `ipums_list_shape()` and `ipums_list_raster()` have
  been deprecated in favor of `ipums_list_files()`.
  
* The `verbose` argument has been deprecated in `read_ipums_sf()` for
  consistency with the terminology used in the sf package. Please use `quiet`
  instead.
  
## Miscellaneous

* Various bug fixes

* Updates to documentation and vignettes for clarity

# ipumsr 0.5.2

* Add progress bar when downloading extracts

* Removed validate argument from extract revision functions and improved warning
  messages for invalid extract field names when used as arguments.
  
* Fixed bug preventing users from providing their API key directly to
  `submit_extract` and `wait_for_extract`.

# ipumsr 0.5.1

* Added the "Rmd for Reproducible Research" template, which sets up a workflow 
  that leverages the IPUMS API to facilitate sharing your 
  analysis. For more details, see 
  [the blog post](https://blog.popdata.org/reproducible-research-r-markdown-ipumsr-ipums-api/). 
  Credit to @ehrlichd for the template and blog post!
  
* Moved the raster package to Suggests so that it is no longer installed 
  automatically when you install ipumsr. The raster package is only required if 
  you need to read raster extracts from the IPUMS Terra collection, and IPUMS 
  Terra is slated to be decommissioned shortly.

# ipumsr 0.5.0

* Added functions for interacting with the IPUMS API for IPUMS USA and IPUMS 
  CPS. For an overview of this new functionality, see the API vignette with 
  `vignette("ipums-api", package = "ipumsr")`. Special thanks to @robe2037, 
  @renae-r, and @ehrlichd for their work on the API functions!

# ipumsr 0.4.5

* Fixed bug causing a read error for some labeled string variables (#61, thanks 
  @chengchou).
  
* ipumsr now always uses the `haven::labelled()` function to create `labelled`
  vectors, in order to maintain compatibility with developments in the haven and 
  vctrs packages (thanks @gergness!).
  
* ipumsr now requires R 3.5 or greater, in line with the new requirements of 
  package dependency raster.

# ipumsr 0.4.4

* Modify `lbl_define()` test to reflect changes to haven's `labelled` class 
  definition.

# ipumsr 0.4.3

* Add `lbl_define()` function to enable the use of `lbl_relabel()` syntax when  
  creating a new labelled vector from an unlabelled one (#51, thanks 
  @chengchou).
  
* Remove pillar printing from ipumsr, getting rid of pesky warning (#47).

* Improved documentation for `lower_vars` argument (#56, thanks @hrecht).

# ipumsr 0.4.2

* Incorporate bug fix in knitr 1.23 that affected encoding in NHGIS vignette.

# ipumsr 0.4.1
* Remove stringr & tidyr dependencies so installation is a little easier (#41).

* Fix bug in pillar printing of haven's `labelled` objects (#43)
 
# ipumsr 0.4.0
* Add `read_ipums_micro_yield()` and `read_ipums_micro_list_yield()` that 
  read data in 'yields', a concept similar to 'chunks', but with a little
  more flexibility. See the big data vignette 
  (`vignette("ipums-bigdata", package = "ipumsr")`) for more details.

* Fixed a bug when trying to set variable attributes but not value labels (#34).

* Fixed a bug where implicit decimals would be double counted for csv files.

* Argument `rectype_convert` has been removed because it no longer did anything.

* Fixed a typo in vignette "ipums-geography" (#37, @jacobkap).

* Creates a pkgdown site (#38, @jacobkap).

# ipumsr 0.3.0
* Lots of improvements for users who wish to use "big data" sized IPUMS extracts. See 
  the vignette using command `vignette("ipums-bigdata", package = "ipusmr")` for
  the full details. 
  
  * There are now chunked versions of the microdata reading functions 
    which let you perform functions on subsets of the data as you read
    it in (`read_ipums_micro_chunked()` & `ipumsr::read_ipums_micro_list_chunked()`)
    
  * There is a new function `ipums_collect()` which combined `dplyr::collect()` with
    `set_ipums_attributes()` to add value and variable labels to data collected from
    a database.
    
  * When reading gzipped files, ipumsr no longer has to store the full text in memory.
  
* Added pillar printing for labelled classes in tibbles. This means that the 
  label will print the labels alongside the values when printed in a tibble 
  (in a subtle grey color when the terminal supports it). To turn this feature off,
  use command `options("ipumsr.show_pillar_labels" = FALSE).
  
* The approach to reading hierarchical data files is much faster.

* Arguments to `read_ipums_sp()` are now in the same order as `read_ipums_sf()`

* `read_ipums_sf()` and `read_ipums_sp()` gain 2 new arguments `vars` which 
  allows you to pick a subset of variables, and `add_layer_var` which lets 
  you add a variable indicating which layer it came from.

* You can now use your inside voice for variable names with the new argument
  `lower_vars` for `read_ipums_ddi()` and `read_ipums_micro()` family of functions
  so that the variable names are lower case.

* ipumsr is compatible with versions of haven newer than 2.0 (while maintaining 
  compatibility with earlier versions). (#31)

# ipumsr 0.2.0
* IPUMS Terra is now officially supported! Read raster, area or microdata extracts
  using functions `read_terra_raster()`, `read_terra_raster_list()`, 
  `read_terra_area()`, `read_terra_area_sf()`, and `read_terra_micro()`

* Add support for keyvar in DDI, which will (eventually) help link data across
  record types in hierarchical extracts. To be effective, this requires more 
  support on the ipums.org website, which is hopefully coming soon (#25 - thanks 
  @mpadge!)

* Improved main vignette instructions for Safari users (#27)

* Fix for selecting columns from csv extracts (#26 - thanks forum user JCambon_OIS!)

* Fixes for the `ipums_list_*()` family of functions.

# ipumsr 0.1.1

* Fixed a bug in ipums_shape_*_join functions when using integer ID columns. (#16)

* Allow for unzipped folders because Safari on macOS unzips folders by default (#17)

* lbl_relabel behavior is improved so that labels aren't assigned sequentially (#21)
