# Can read NHGIS extract: single dataset

    Code
      nhgis_csv <- read_nhgis(nhgis_single_csv)
    Message
      Use of data from NHGIS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
      Rows: 71 Columns: 25
      -- Column specification --------------------------------------------------------
      Delimiter: ","
      chr  (9): GISJOIN, STUSAB, CMSA, PMSA, PMSAA, AREALAND, AREAWAT, ANPSADPI, F...
      dbl (13): YEAR, MSA_CMSAA, INTPTLAT, INTPTLNG, PSADC, D6Z001, D6Z002, D6Z003...
      lgl  (3): DIVISIONA, REGIONA, STATEA
      
      i Use `spec()` to retrieve the full column specification for this data.
      i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# We get informative error messages when reading NHGIS extracts

    Code
      read_nhgis(nhgis_multi_ds, file_select = 3, verbose = FALSE)
    Condition
      Error in `find_files_in()`:
      ! Can't select files past the end.
      i Location 3 doesn't exist.
      i There are only 2 files.
      Available files:
      * nhgis0731_csv/nhgis0731_ds239_20185_nation.csv
      * nhgis0731_csv/nhgis0731_ts_nominal_state.csv

