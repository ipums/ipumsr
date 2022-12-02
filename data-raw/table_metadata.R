# Use list of all NHGIS datasets to create a master data file of all
# data tables, which is not provided by API directly.

# Note that because data table summary metadata is not provided
# by API directly, the constructed table will not be kept up to date
# with new datasets automatically.

datasets <- get_nhgis_metadata("datasets")$name

# Load internal data, if it exists
table_metadata <- tryCatch(
  ipumsr:::table_metadata,
  error = function(cnd) NULL
)

datasets_to_add <- setdiff(datasets, table_metadata$dataset)

new_table_metadata <- purrr::map_dfr(
  datasets_to_add,
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

table_metadata <- dplyr::bind_rows(
  table_metadata,
  new_table_metadata
)

# Order in same dataset order as dataset metadata
table_metadata <- dplyr::arrange(
  table_metadata,
  match(.data[["dataset"]], datasets)
)

# Note: this will remove other internal data sources if they are not
# included in this call.
usethis::use_data(table_metadata, overwrite = TRUE, internal = TRUE)
