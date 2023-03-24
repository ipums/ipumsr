# State-level geographic extents are provided with each dataset,
# but are a consistent mapping between codes and state names.

# Use this data internally to enable the use of state name strings
# where otherwise the codes might be required

# Any dataset that has extent selection will do for state-level codes
placeholder_ds <- "1990_STF1"

state_extent_codes <- get_nhgis_metadata(
  dataset = placeholder_ds
)$geographic_instances

state_names <- tolower(state_extent_codes$description)
state_codes <- state_extent_codes$name

state_abb <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
  "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
  "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
  "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

code_lookup <- purrr::set_names(
  rep(state_codes, 2),
  c(state_names, tolower(state_abb))
)

name_lookup <- purrr::set_names(
  rep(state_names, 2),
  c(state_codes, tolower(state_abb))
)

abb_lookup <- purrr::set_names(
  rep(state_abb, 2),
  c(state_codes, state_names)
)

state_geog_lookup <- list(
  codes = code_lookup,
  names = name_lookup,
  abbs  = abb_lookup
)

usethis::use_data(state_geog_lookup, overwrite = TRUE, internal = TRUE)
