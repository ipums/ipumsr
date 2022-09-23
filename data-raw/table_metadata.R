# Use list of all NHGIS datasets to create a master data file of all
# data tables, which is not provided by API directly.

# Note that because data table summary metadata is not provided
# by API directly, the constructed table will not be kept up to date
# with new datasets automatically.

datasets <- get_nhgis_metadata("datasets")$name

table_metadata <- purrr::map_dfr(
  datasets,
  ~{
    meta <- dplyr::mutate(
      get_nhgis_metadata(dataset = .x)$data_tables,
      dataset = .x,
      .after = name
    )

    Sys.sleep(1) # Avoid API request limit

    meta
  }
)

usethis::use_data(table_metadata, overwrite = TRUE, internal = TRUE)
