# Create/Add datasets that are used internally by 'iLab' package
# Note these files are not readily available to the user

# > Park Identification ----
parkID <- read.csv("data-raw/marine_park_identifiers.csv")

# > Create Internal data ----

# Create/Overwrite R/sysdata.rda
usethis::use_data(parkID,
                  # x, y, z, # Add multiple datasets to internal datasets
                  internal = TRUE, overwrite = TRUE)

