# Package index

## Read IPUMS data

Load IPUMS data and metadata files into R

### Microdata projects

- [`read_ipums_micro()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro.md)
  [`read_ipums_micro_list()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro.md)
  : Read data from an IPUMS microdata extract
- [`read_ipums_ddi()`](https://tech.popdata.org/ipumsr/reference/read_ipums_ddi.md)
  : Read metadata about an IPUMS microdata extract from a DDI codebook
  (.xml) file
- [`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_chunked.md)
  [`read_ipums_micro_list_chunked()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_chunked.md)
  : Read data from an IPUMS microdata extract by chunk
- [`read_ipums_micro_yield()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_yield.md)
  [`read_ipums_micro_list_yield()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro_yield.md)
  : Read data from an IPUMS microdata extract in yields

### Aggregate data projects

- [`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md)
  : Read data from an IPUMS aggregate data extract
- [`read_nhgis_codebook()`](https://tech.popdata.org/ipumsr/reference/read_nhgis_codebook.md)
  **\[experimental\]** : Read metadata from an NHGIS codebook (.txt)
  file
- [`read_ihgis_codebook()`](https://tech.popdata.org/ipumsr/reference/read_ihgis_codebook.md)
  **\[experimental\]** : Read metadata from an IHGIS extract's codebook
  files

### General use

- [`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md)
  : Read spatial data from an IPUMS extract
- [`ipums_list_files()`](https://tech.popdata.org/ipumsr/reference/ipums_list_files.md)
  : List files contained within a zipped IPUMS extract

## Interact with the IPUMS API

Request and download IPUMS data within R

- [`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
  : Define an extract request for an IPUMS microdata collection

- [`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
  : Define an extract request for an IPUMS aggregate data collection

- [`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/reference/get_metadata_catalog.md)
  [`catalog_types()`](https://tech.popdata.org/ipumsr/reference/get_metadata_catalog.md)
  : Retrieve a catalog of available data sources for an IPUMS collection

- [`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md)
  : Retrieve detailed metadata about an IPUMS data source

- [`get_sample_info()`](https://tech.popdata.org/ipumsr/reference/get_sample_info.md)
  : List available samples for IPUMS microdata collections

- [`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
  : Submit an extract request via the IPUMS API

- [`wait_for_extract()`](https://tech.popdata.org/ipumsr/reference/wait_for_extract.md)
  [`is_extract_ready()`](https://tech.popdata.org/ipumsr/reference/wait_for_extract.md)
  : Wait for an extract request to finish processing

- [`download_extract()`](https://tech.popdata.org/ipumsr/reference/download_extract.md)
  : Download a completed IPUMS data extract

- [`get_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md)
  [`get_last_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md)
  : Retrieve the definition and latest status of an extract request

- [`get_extract_history()`](https://tech.popdata.org/ipumsr/reference/get_extract_history.md)
  : Browse definitions of previously submitted extract requests

- [`save_extract_as_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md)
  [`define_extract_from_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md)
  : Store an extract definition in JSON format

- [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md)
  : Set your IPUMS API key

- [`set_ipums_default_collection()`](https://tech.popdata.org/ipumsr/reference/set_ipums_default_collection.md)
  : Set your default IPUMS collection

- [`ipums_data_collections()`](https://tech.popdata.org/ipumsr/reference/ipums_data_collections.md)
  : List IPUMS data collections

- [`ipums_extract-class`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
  [`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
  :

  `ipums_extract` class

- [`download_supplemental_data()`](https://tech.popdata.org/ipumsr/reference/download_supplemental_data.md)
  : Download IPUMS supplemental data files

## Explore IPUMS variable metadata

Get additional information about IPUMS data contents

- [`ipums_var_info()`](https://tech.popdata.org/ipumsr/reference/ipums_var_info.md)
  [`ipums_var_label()`](https://tech.popdata.org/ipumsr/reference/ipums_var_info.md)
  [`ipums_var_desc()`](https://tech.popdata.org/ipumsr/reference/ipums_var_info.md)
  [`ipums_val_labels()`](https://tech.popdata.org/ipumsr/reference/ipums_var_info.md)
  : Get contextual information about variables in an IPUMS data source
- [`ipums_file_info()`](https://tech.popdata.org/ipumsr/reference/ipums_file_info.md)
  [`ipums_conditions()`](https://tech.popdata.org/ipumsr/reference/ipums_file_info.md)
  : Get file information for an IPUMS extract
- [`ipums_view()`](https://tech.popdata.org/ipumsr/reference/ipums_view.md)
  : View a static webpage with variable metadata from an IPUMS extract
- [`ipums_website()`](https://tech.popdata.org/ipumsr/reference/ipums_website.md)
  : Launch a browser window to an IPUMS metadata page

## Work with value labels

Translate IPUMS value labels to standard R data structures

- [`lbl_na_if()`](https://tech.popdata.org/ipumsr/reference/lbl_na_if.md)
  : Convert labelled data values to NA
- [`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
  [`lbl_collapse()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
  : Modify value labels for a labelled vector
- [`lbl_define()`](https://tech.popdata.org/ipumsr/reference/lbl_define.md)
  : Define labels for an unlabelled vector
- [`zap_ipums_attributes()`](https://tech.popdata.org/ipumsr/reference/zap_ipums_attributes.md)
  : Remove label attributes from a data frame or labelled vector
- [`lbl_clean()`](https://tech.popdata.org/ipumsr/reference/lbl_clean.md)
  : Clean unused labels
- [`lbl_add()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md)
  [`lbl_add_vals()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md)
  : Add labels for unlabelled values
- [`lbl()`](https://tech.popdata.org/ipumsr/reference/lbl.md) : Make a
  label placeholder object

## Other helpers

Miscellaneous functions for working with IPUMS data

- [`ipums_bind_rows()`](https://tech.popdata.org/ipumsr/reference/ipums_bind_rows.md)
  : Bind multiple data frames by row, preserving labelled attributes
- [`ipums_shape_left_join()`](https://tech.popdata.org/ipumsr/reference/ipums_shape_join.md)
  [`ipums_shape_right_join()`](https://tech.popdata.org/ipumsr/reference/ipums_shape_join.md)
  [`ipums_shape_inner_join()`](https://tech.popdata.org/ipumsr/reference/ipums_shape_join.md)
  [`ipums_shape_full_join()`](https://tech.popdata.org/ipumsr/reference/ipums_shape_join.md)
  : Join tabular data to geographic boundaries
- [`set_ipums_var_attributes()`](https://tech.popdata.org/ipumsr/reference/set_ipums_var_attributes.md)
  : Add IPUMS variable attributes to a data frame
- [`ipums_collect()`](https://tech.popdata.org/ipumsr/reference/ipums_collect.md)
  : Collect data into R session with IPUMS attributes
- [`ipums_example()`](https://tech.popdata.org/ipumsr/reference/ipums_example.md)
  : Get path to IPUMS example datasets
