# `ipums_ddi` class

The `ipums_ddi` class provides a data structure for storing the metadata
information contained in IPUMS codebook files. These objects are
primarily used when loading IPUMS data, but can also be used to explore
metadata for an IPUMS extract.

- For microdata projects, this information is provided in [DDI
  codebook](https://ddialliance.org/introduction-to-ddi) (.xml) files.

- For NHGIS, this information is provided in .txt codebook files.

- For IHGIS, this information is provided in a collection of .csv files.

The codebook file contains metadata about the extract files themselves,
including file name, file path, and extract date as well as information
about variables present in the data, including variable names,
descriptions, data types, implied decimals, and positions in the
fixed-width files.

This information is used to correctly parse IPUMS fixed-width files and
attach additional variable metadata to data upon load.

Note that codebook metadata for aggregate data extracts can also be
stored in an `ipums_ddi` object, even though these codebooks are not
distributed as .xml files. These files do not adhere to the same
standards as the DDI codebook files, so some `ipums_ddi` fields will be
left blank when reading aggregate data codebooks.

### Creating an `ipums_ddi` object

- To create an `ipums_ddi` object from an IPUMS microdata extract, use
  [`read_ipums_ddi()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_ddi.md).

- To create an `ipums_ddi` object from an IPUMS NHGIS extract, use
  [`read_nhgis_codebook()`](https://tech.popdata.org/ipumsr/dev/reference/read_nhgis_codebook.md).

- To create an `ipums_ddi` object from an IPUMS IHGIS extract, use
  [`read_ihgis_codebook()`](https://tech.popdata.org/ipumsr/dev/reference/read_ihgis_codebook.md).

### Loading data

- To load the data associated with an `ipums_ddi` object, use
  [`read_ipums_micro()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro.md),
  [`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_chunked.md),
  or
  [`read_ipums_micro_yield()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_yield.md)

### View metadata

- Use
  [`ipums_var_info()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_var_info.md)
  to explore variable-level metadata for the variables included in a
  dataset.

- Use
  [`ipums_file_info()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_file_info.md)
  to explore file-level metadata for an extract.
