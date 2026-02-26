# Create/Add datasets that are exported with the 'iLab' package for both internal and external use

library(dplyr)

# > Family vectors for fish, sharks/rays, other SoE -----
# extract family names from caab for specific groupings to use in data extraction/filtering

# TODO - move/update (remove filtering step) get caab dump from essential files
# Execute request from CAAB
httr::GET("https://www.cmar.csiro.au/data/caab/create_caab_extract.cfm")

# Set url
download.url <- paste0("https://www.cmar.csiro.au/data/temp/caab/caab_species_", format(Sys.Date(), "%Y%m%d"), ".xls")

# Download file and save over old version
GET(download.url, write_disk(
  file.path(get_dir("ilab_fish", "!Essential_Files/CAAB_Species_Files/CAAB_Creation"),"caab_species_dump.xls"),
  overwrite = TRUE))

# Read in CAAB file and make fixes
caab.dump <- readxl::read_xls(file.path(get_dir("ilab_fish", "!Essential_Files/CAAB_Species_Files/CAAB_Creation"),"caab_species_dump.xls"), sheet = 1)%>%
  janitor::clean_names()%>%
  dplyr::filter(!stringr::str_detect(scientific_name, "non-current code")) %>% # Remove old codes
  mutate(family = trimws(family))%>% # Remove whitespace from family
  dplyr::filter(!stringr::str_detect(family, "&")) %>% # Remove pooled families
  dplyr::filter(!stringr::str_detect(family, ",")) %>% # Remove pooled families
  dplyr::filter(!stringr::str_detect(family, " ")) # remove pooled families
  

# Vector of Shark/Ray families
shark_families<- unique(caab.dump$family[caab.dump$class %in% c("Holocephali", "Elasmobranchii")])

# Vector of all fish (inc. sharks/rays)
fish_families <- unique(caab.dump$family[caab.dump$class %in% c("Holocephali", "Elasmobranchii", "Cephalaspidomorphi", "Myxini", "Sarcopterygii", "Actinopterygii")])

# Vector of other species of interest (i.e. non-Fish)
other_families <- unique(caab.dump$family[!caab.dump$family %in% c(fish_families)])

# Combine to list
families <- list("fish" = fish_families,
                 "sharks" = shark_families,
                 "other" = other_families)


# > Create Internal data ----
usethis::use_data(families,
                  # x, y, z, # Add multiple datasets to internal datasets
                  internal = FALSE, overwrite = TRUE)
