# ipumsr (development version)

* Improved parsing of labeled values included in "codInstr" tags in the DDI 
  codebook for microdata extracts.

# ipumsr 0.9.0

## New features

-   This release adds comprehensive support for IPUMS IHGIS extract submission, 
    metadata retrieval, and data loading! 
    
    As IPUMS IHGIS is an aggregate data 
    project alongside IPUMS NHGIS, many functions that previously were 
    NHGIS-specific have been generalized to accommodate both collections. This 
    includes the following new functions:
    
    -   `read_ipums_agg()` loads downloaded extracts for both NHGIS and IHGIS.
        This replaces `read_nhgis()`, which is now deprecated.
        
        `read_ipums_agg()` also includes a new `file_encoding` argument, as
        IHGIS and NHGIS files often have different encoding. Typically,
        the default `file_encoding` should load an aggregate data extract
        file correctly. If not, you can adjust the encoding here.
        
    -   `define_extract_agg()` defines extract requests for both NHGIS and
        IHGIS. Use the `collection` argument to specify the data collection for
        a given extract. This replaces `define_extract_nhgis()`, which is now
        deprecated.
        
    -   `get_metadata_catalog()` retrieves summary metadata about NHGIS and 
        IHGIS data sources. `get_metadata()` retrieves
        detailed metadata about a particular NHGIS or IHGIS data source. These
        functions replace `get_metadata_nhgis()`, which is now deprecated.
        
-   Adds `read_ihgis_codebook()` to load codebook files containing
    file-level metadata for downloaded IHGIS extracts. This function is
    currently experimental.
    
-   Enables monetary value adjustment for supported IPUMS USA and IPUMS
    CPS variables. Use the `adjust_monetary_values` argument to `var_spec()`
    to include an additional adjusted variable in your extract. See the
    [IPUMS CPS](https://cps.ipums.org/cps/adjusted_monetary_values.shtml) 
    and [IPUMS USA](https://usa.ipums.org/usa/adjusted_monetary_values.shtml) 
    documentation for more information on monetary adjustment.

## Function + argument retirements
    
-   `data_layer` and `shape_layer` arguments are now defunct. In cases
    where this functionality is still supported, please use the
    `file_select` argument instead.
    
-   `get_recent_extracts_info_list()`, `get_recent_extracts_info_tbl()`,
    and `extract_tbl_to_list()` are now defunct. Use `get_extract_history()`
    to obtain a list of previously-submitted extracts.
    
-   The ability to read files through directories is now defunct. This
    affects most reader functions. If you have unzipped an IPUMS extract
    archive, please provide the path to the individual file you wish
    to load, not its containing directory.
    
-   `project` and `var_label` arguments in `ipums_website()` are now 
    defunct.

# ipumsr 0.8.2
    
-   Adds codebook files to output of `ipums_list_files()` (#85).

-   Removes deprecated `ipums_list_files()` arguments `data_layer`, 
    `shape_layer`, and `raster_layer`. Use `file_select` instead. Also removes
    support for listing raster file types (#85).

-   The `"*"` wildcard is no longer required to select all `geographic_extents`
    in `define_extract_nhgis()`. Instead, all available geographic extents
    are selected by default. The `"*"` syntax is still supported.

-   Adds `download_supplemental_data()` to enable access to supplemental
    data files via the IPUMS API (#83).

-   Fixed bug in `shape_join.R` that would always give the result of a 
    right join no matter whether `ipums_shape_left_join()` or 
    `ipums_shape_right_join()` was used (#82) (thanks @JiaZhang42!).

# ipumsr 0.8.1

## Minor improvements and fixes

-   Small fix to vignette that was previously throwing an error in R CMD Check 
    with `--no-manual` and `--no-build-vignettes` options (#80).

# ipumsr 0.8.0

## Breaking changes + deprecations

-   Collection-specific definition functions for IPUMS microdata projects
    have been deprecated in favor of `define_extract_micro()`. This
    streamlines syntax across all supported microdata collections. Use the 
    `collection` argument to specify the collection for your
    microdata extract. 
    
    IPUMS NHGIS extracts are still created with
    `define_extract_nhgis()`.

-   Previously deprecated methods for retrieving extract history are now
    defunct. Use `get_extract_history()` instead.

## New features

-   Adds support for IPUMS API extract endpoints for IPUMS Time Use and
    IPUMS Health Surveys projects! This includes:
    
    -   IPUMS Time Use
    
        -   IPUMS ATUS (`"atus"`)
        
        -   IPUMS AHTUS (`"ahtus"`)
        
        -   IPUMS MTUS (`"mtus"`)
    
    -   IPUMS Health Surveys
    
        -   IPUMS NHIS (`"nhis"`)
        
        -   IPUMS MEPS (`"meps"`)

-   Adds support for new rectangularization options for certain
    microdata collections. Use the `rectagular_on` argument in your
    extract definition to rectangularize on records other than person
    (`"P"`) records.

-   Time use variables can be included in IPUMS Time Use extracts. Use
    the `tu_var_spec()` helper with the `time_use_variables` argument of
    `define_extract_micro()` to create a time use variable
    specification.

    This includes both IPUMS-defined and user-defined time use variables. Note 
    that user-defined time use variables cannot currently be created via API. 
    However, if you have defined a time use variable through the IPUMS web 
    interface, you can request it in an extract via API.

-   Sample members can be specified in IPUMS ATUS extracts. Use the
    `sample_members` argument of `define_extract_micro()` to do so.

-   `"household_only"` is now a supported value for `data_structure` in
    IPUMS USA extracts.
    
## Minor improvements and fixes

-   Fixes bug in `add_to_extract.micro_extract()` that caused an error if a user
    attempted to update the value of `data_quality_flags` in a detailed variable
    specification.

# ipumsr 0.7.2

-   Fix for CRAN checks

# ipumsr 0.7.1

-   Minor updates to README and vignettes

# ipumsr 0.7.0

## Function retirements

-   ipumsr no longer suggests raster, rgdal, and sp (#23). Removing
    these dependencies requires the retirement of several previously
    deprecated functions:

    -   All `read_terra_*()` functions
    -   `read_ipums_sp()` (use `read_ipums_sf()` to load spatial data in
        sf format)
    -   `read_ipums_codebook()` (use `read_nhgis_codebook()` to load an
        NHGIS codebook)
    -   `ipums_list_*()` helper functions (instead, use
        `ipums_list_files()`)
    -   `read_nhgis_sf()` and `read_nhgis_sp()` (instead, use
        `read_ipums_sf()` and `read_nhgis()` to load spatial and tabular
        data separately, then join with an `ipums_shape_join_*()`
        function)

## Other updates

-   Fixes bug in `ipums_view()` where content would not display properly
    in viewer pane (#19)

-   RStudio is now explicitly required to launch files in the viewer
    pane using `ipums_view()`. R console users can still generate
    stand-alone HTML files that can be viewed in a different browser.

-   Updates the UI and fixes various bugs in `ipums_website()` (#54):

    -   Accepted project names are now consistent with those provided in
        `ipums_ddi` objects. Shorthand project names as used by the
        IPUMS API are also accepted.
    -   Users can now use the same syntax regardless of whether
        providing an `ipums_ddi` object or a project name. The `project`
        argument has been deprecated.
    -   MacOS is now supported
    -   The `var` argument is no longer required. Omitting `var` will
        launch the URL to the homepage for the specified IPUMS project.
    -   The `homepage_if_missing` argument now defaults to `FALSE`.
    -   The `var_label` argument has been deprecated

-   Updates IPUMS projects listed in `ipums_data_collections()`.

-   Various documentation updates

# ipumsr 0.6.3

-   The ability to read an IPUMS DDI file contained within a zip archive
    using `read_ipums_ddi()` has been deprecated. Users must now load
    DDI files by providing the direct path to the uncompressed .xml
    file.

    This resolves an inconsistency between the behavior of
    `read_ipums_micro_*()` functions when provided a DDI file path as
    compared to an `ipums_ddi` object created with `read_ipums_ddi()`.

-   The ability to read IPUMS files by providing the path to their
    containing directory has been deprecated. This affects:

    -   `read_nhgis()`
    -   `read_ipums_sf()`
    -   `read_nhgis_codebook()`
    -   `read_ipums_ddi()`
    -   `ipums_list_files()`

    These functions now require either a zip archive (with the exception
    of `read_ipums_ddi()`—see above) or a direct file path as input.

    This has the consequence that `read_ipums_sf()` with
    `bind_multiple = TRUE` requires a zip archive as input if multiple
    files are to be read and combined.

-   Documentation updates for clarity

# ipumsr 0.6.2

-   Fixes for CRAN checks.

# ipumsr 0.6.1

-   Fixes for CRAN checks.

# ipumsr 0.6.0

## Breaking Changes + Deprecations

### IPUMS API

-   ipumsr now supports **IPUMS API version 2**, and no longer supports
    either the beta version or version 1 of the IPUMS API.

    This means that extract definitions saved in JSON format will no
    longer be compatible with ipumsr via `define_extract_from_json()`.
    To load extract definitions created under previous API versions,
    there are two options:

    -   Rewrite the extract definition represented in the JSON file
        using the `define_extract_*()` function for the relevant IPUMS
        collection, then update the saved file with
        `save_extract_to_json()`.

    -   Update the JSON file by converting all `snake_case` fields to
        `camelCase`. For instance, `"data_format"` would become
        `"dataFormat"`. The `"api_version"` field will also need to be
        changed to `"version"` and set equal to `2`.

    See the IPUMS developer documentation for more details on [API
    versioning](https://developer.ipums.org/docs/apiprogram/versioning/)
    and [breaking
    changes](https://developer.ipums.org/docs/apiprogram/changelog/)
    introduced in version 2.

-   The `ipums_extract` object structure has been updated. For IPUMS
    microdata projects, `variables` and `samples` are no longer stored
    as character vectors, but as lists. This accommodates new API
    version 2 features (see below). Use `names(x$variables)` instead of
    `x$variables` to access variable (and sample) names as a character
    vector.

-   `get_recent_extracts_info_*()` functions have been deprecated.
    Additionally, tabular-formatted extract history is no longer
    supported, and conversion functions `extract_tbl_to_list()` and
    `extract_list_to_tbl()` have therefore been deprecated as well.

    Use `get_extract_history()` to browse previous extract definitions
    in list format.

### IPUMS Readers

-   `read_nhgis_sf()` and `read_nhgis_sp()` have been deprecated. Use
    `read_ipums_sf()` and `read_nhgis()` to load spatial and tabular
    data, respectively. Join data with an `ipums_shape_*_join()`
    function.

-   `data_layer` and `shape_layer` arguments have been deprecated in
    favor of `file_select` throughout ipumsr. This provides clarity on
    the intended purpose of this argument. Deprecated functions that use
    the original argument names remain unchanged.

-   Support for objects from the sp package has been deprecated because
    of the upcoming retirement of rgdal. Use `read_ipums_sf()` to load
    spatial data as an `sf` object. To convert to a `Spatial*` object,
    use `sf::as_Spatial()`.

    For more, see r-spatial's
    [post](https://r-spatial.org/r/2022/04/12/evolution.html) covering
    the evolution of several spatial packages.

-   `read_ipums_sf()` no longer defaults to `bind_multiple = TRUE`.

-   Individual `ipums_list_*()` functions have been moved to
    `ipums_list_files()`.

-   `read_ipums_codebook()` has been deprecated. Use
    `read_nhgis_codebook()` to read NHGIS codebook files. IPUMS Terra
    codebook files are no longer supported (see below)

-   Example files in `ipums_example()` have been updated and include new
    file names.

### IPUMS Terra

-   Support for IPUMS Terra has been discontinued. This includes
    deprecations to all `read_terra_*()` functions, the
    `types = "raster"` option in `ipums_list_files()`, and
    `read_ipums_codebook()`.

    For more on IPUMS Terra decommissioning, click
    [here](https://terra.ipums.org/decommissioning).

## New Features

### IPUMS API

-   Adds API support for IPUMS NHGIS and IPUMS International!

    -   Use `define_extract_nhgis()` to create an NHGIS extract
        definition.
    -   Use `define_extract_ipumsi()` to create an IPUMS International
        extract definition.

-   Adds support for IPUMS API version 2 features! This includes:

    -   Detailed variable specifications for IPUMS microdata extract
        definitions, including case selections, attached
        characteristics, and data quality flags. Use `var_spec()` to add
        these specifications to variables in your extract definition.
    -   Additional definition-wide parameters for IPUMS microdata
        extracts, including `data_quality_flags` and `case_select_who`.
    -   Hierarchical extracts for IPUMS microdata extracts. Set
        `data_structure = "hiearchical"` to create a hierarchical
        extract definition.
    -   Year selection for time series tables in IPUMS NHGIS extract
        definitions. Use `tst_spec()` to add year selections for time
        series tables.

-   Adds API support for IPUMS NHGIS metadata

    -   Use `get_metadata_nhgis()` to browse NHGIS data sources.
        Metadata is available in summary form for all datasets, data
        tables, time series tables, and shapefiles as well as for
        individual datasets, data tables, and time series tables.

-   Allows users to set a default IPUMS collection using
    `set_ipums_default_collection()`. Users with a default collection do
    not need to specify the IPUMS collection in functions that require
    it; instead, the default collection is used. This is a convenience
    for users who rely primarily on a single IPUMS collection.

-   `wait_for_extract()` wait intervals no longer double after each
    status check. Instead, intervals increase by 10 seconds with each
    subsequent check.

### IPUMS readers

-   Adds handling for fixed-width NHGIS extracts in `read_nhgis()`.

-   `read_nhgis_codebook()` allows reading of raw codebook lines (as
    opposed to extracting codebook information into an `ipums_ddi`
    object) by setting `raw = TRUE`. Furthermore, `var_info` generated
    from NHGIS codebook files has been updated to include more
    contextual information about the data variables.

-   `read_nhgis()` now supports additional arguments to refine the data
    loading process. Users can now specify `col_types` manually and read
    a subset of a data file using `vars` and `n_max`.

-   `read_nhgis()` now allows users to retain the extra header row
    included in some NHGIS files. Set `remove_extra_header = FALSE` to
    do so. In general, the information contained in the extra header is
    attached to the data from the NHGIS codebook file, but in some cases
    the extra header may differ slightly from the information found in
    the codebook.

## Miscellaneous

-   Various bug fixes

-   Updates to documentation and vignettes for clarity

# ipumsr 0.5.2

-   Add progress bar when downloading extracts

-   Removed validate argument from extract revision functions and
    improved warning messages for invalid extract field names when used
    as arguments.

-   Fixed bug preventing users from providing their API key directly to
    `submit_extract` and `wait_for_extract`.

# ipumsr 0.5.1

-   Added the "Rmd for Reproducible Research" template, which sets up a
    workflow that leverages the IPUMS API to facilitate sharing your
    analysis. For more details, see [the blog
    post](https://blog.popdata.org/reproducible-research-r-markdown-ipumsr-ipums-api/).
    Credit to @ehrlichd for the template and blog post!

-   Moved the raster package to Suggests so that it is no longer
    installed automatically when you install ipumsr. The raster package
    is only required if you need to read raster extracts from the IPUMS
    Terra collection, and IPUMS Terra is slated to be decommissioned
    shortly.

# ipumsr 0.5.0

-   Added functions for interacting with the IPUMS API for IPUMS USA and
    IPUMS CPS. For an overview of this new functionality, see the API
    vignette with `vignette("ipums-api", package = "ipumsr")`. Special
    thanks to @robe2037, @renae-r, and @ehrlichd for their work on the
    API functions!

# ipumsr 0.4.5

-   Fixed bug causing a read error for some labeled string variables
    (#61, thanks @chengchou).

-   ipumsr now always uses the `haven::labelled()` function to create
    `labelled` vectors, in order to maintain compatibility with
    developments in the haven and vctrs packages (thanks @gergness!).

-   ipumsr now requires R 3.5 or greater, in line with the new
    requirements of package dependency raster.

# ipumsr 0.4.4

-   Modify `lbl_define()` test to reflect changes to haven's `labelled`
    class definition.

# ipumsr 0.4.3

-   Add `lbl_define()` function to enable the use of `lbl_relabel()`
    syntax when\
    creating a new labelled vector from an unlabelled one (#51, thanks
    @chengchou).

-   Remove pillar printing from ipumsr, getting rid of pesky warning
    (#47).

-   Improved documentation for `lower_vars` argument (#56, thanks
    @hrecht).

# ipumsr 0.4.2

-   Incorporate bug fix in knitr 1.23 that affected encoding in NHGIS
    vignette.

# ipumsr 0.4.1

-   Remove stringr & tidyr dependencies so installation is a little
    easier (#41).

-   Fix bug in pillar printing of haven's `labelled` objects (#43)

# ipumsr 0.4.0

-   Add `read_ipums_micro_yield()` and `read_ipums_micro_list_yield()`
    that read data in 'yields', a concept similar to 'chunks', but with
    a little more flexibility. See the big data vignette
    (`vignette("ipums-bigdata", package = "ipumsr")`) for more details.

-   Fixed a bug when trying to set variable attributes but not value
    labels (#34).

-   Fixed a bug where implicit decimals would be double counted for csv
    files.

-   Argument `rectype_convert` has been removed because it no longer did
    anything.

-   Fixed a typo in vignette "ipums-geography" (#37, @jacobkap).

-   Creates a pkgdown site (#38, @jacobkap).

# ipumsr 0.3.0

-   Lots of improvements for users who wish to use "big data" sized
    IPUMS extracts. See the vignette using command
    `vignette("ipums-bigdata", package = "ipusmr")` for the full
    details.

    -   There are now chunked versions of the microdata reading
        functions which let you perform functions on subsets of the data
        as you read it in (`read_ipums_micro_chunked()` &
        `ipumsr::read_ipums_micro_list_chunked()`)

    -   There is a new function `ipums_collect()` which combined
        `dplyr::collect()` with `set_ipums_attributes()` to add value
        and variable labels to data collected from a database.

    -   When reading gzipped files, ipumsr no longer has to store the
        full text in memory.

-   Added pillar printing for labelled classes in tibbles. This means
    that the label will print the labels alongside the values when
    printed in a tibble (in a subtle grey color when the terminal
    supports it). To turn this feature off, use command
    \`options("ipumsr.show_pillar_labels" = FALSE).

-   The approach to reading hierarchical data files is much faster.

-   Arguments to `read_ipums_sp()` are now in the same order as
    `read_ipums_sf()`

-   `read_ipums_sf()` and `read_ipums_sp()` gain 2 new arguments `vars`
    which allows you to pick a subset of variables, and `add_layer_var`
    which lets you add a variable indicating which layer it came from.

-   You can now use your inside voice for variable names with the new
    argument `lower_vars` for `read_ipums_ddi()` and
    `read_ipums_micro()` family of functions so that the variable names
    are lower case.

-   ipumsr is compatible with versions of haven newer than 2.0 (while
    maintaining compatibility with earlier versions). (#31)

# ipumsr 0.2.0

-   IPUMS Terra is now officially supported! Read raster, area or
    microdata extracts using functions `read_terra_raster()`,
    `read_terra_raster_list()`, `read_terra_area()`,
    `read_terra_area_sf()`, and `read_terra_micro()`

-   Add support for keyvar in DDI, which will (eventually) help link
    data across record types in hierarchical extracts. To be effective,
    this requires more support on the ipums.org website, which is
    hopefully coming soon (#25 - thanks @mpadge!)

-   Improved main vignette instructions for Safari users (#27)

-   Fix for selecting columns from csv extracts (#26 - thanks forum user
    JCambon_OIS!)

-   Fixes for the `ipums_list_*()` family of functions.

# ipumsr 0.1.1

-   Fixed a bug in ipums_shape\_\*\_join functions when using integer ID
    columns. (#16)

-   Allow for unzipped folders because Safari on macOS unzips folders by
    default (#17)

-   lbl_relabel behavior is improved so that labels aren't assigned
    sequentially (#21)
