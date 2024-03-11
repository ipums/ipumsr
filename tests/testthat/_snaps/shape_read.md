# We get informative errors when reading shapefiles

    Code
      read_ipums_sf(nhgis_multi_shp, file_select = 4)
    Condition
      Error in `find_files_in()`:
      ! Can't select files past the end.
      i Location 4 doesn't exist.
      i There are only 3 files.
      Available files:
      * nhgis0712_shape/nhgis0712_shapefile_cenpop2000_us_state_cenpop_2000.zip
      * nhgis0712_shape/nhgis0712_shapefile_tl2000_us_pmsa_1990.zip
      * nhgis0712_shape/nhgis0712_shapefile_tl2000_us_pmsa_2000.zip

