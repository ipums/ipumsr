# We get informative error messages when reading NHGIS extracts

    Code
      read_nhgis(nhgis_multi_ds, file_select = 3, verbose = FALSE)
    Condition
      [1m[33mError[39m in `find_files_in()`:[22m
      [33m![39m Can't select files past the end.
      ℹ Location 3 doesn't exist.
      ℹ There are only 2 files.
      Available files:
      • nhgis0731_csv/nhgis0731_ds239_20185_nation.csv
      • nhgis0731_csv/nhgis0731_ts_nominal_state.csv

