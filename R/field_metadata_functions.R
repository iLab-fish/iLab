#' Clean field metadata variable names
#'
#' @description
#' Standardises column names in DBCA BRUV/DOV field metadata based on 
#' known/potential synonyms. 
#'
#' @param x A data frame containing field metadata. 
#'
#' @examples
#' \dontrun{
#' field.data <- field.data%>%
#'   janitor::clean_names()%>%         # standardise separators and case
#'   clean_field_names() #updates synonyms depending on method
#' }
#'
#' @importFrom dplyr rename
#' @export

# usethis::use_package("dplyr")

clean_field_names <- function(x) {
  
  stopifnot("data.frame expected for input 'x'"  = is.data.frame(x))
  
  # list of expected column names (left) and synonyms (right)
  lookup <- c(
    sample = "opcode",
    latitude = "latitude_dd",
    longitude = "longitude_dd",
    latitude = "lat",
    longitude = "lon",
    date_time = "datetime",
    date_time = "time_date",
    date_time = "date.time",
    depth = "depth_m",# BRUV only
    left_right_cam_card = "l_r_cam_card",
    left_right_cam_card = "left_right_camera_number",
    lcam = "left_camera_number",
    rcam = "right_camera_number",
    lcam = "left_camera",
    rcam = "right_camera",
    lcam = "left_cam",
    rcam = "right_cam",
    operator = "divers", # DOV only
    operator = "operators", # DOV/ROV only
    visibility = "vis", 
    visibility = "vis.", 
    visibility = "vis_", 
    fov = "field_Of_view", # BRUV only
    field_note = "field_notes",
    field_note = "comment"
    )
 
    
  x %>%
    dplyr::rename(any_of(lookup))
  
}


#' Get required field metadata columns
#'
#' @description
#' Get vectors of expected and vital variables required in field metadata dependent
#' on sampling method. Returns a list containing the named vectors `expected` and 
#' `vital`.
#'
#' @param method Character; one of `c("BRUV","DOV", "ROV")`. Determines the sheet
#'   structure, required columns, and data validations applied.
#'   
#' @examples
#' \dontrun{
#' var <- fields_variables("BRUV")
#' }
#'
#' @export

field_variables <- function(method = NULL) {
  if (!method %in%c("BRUV","DOV","ROV")) {
    stop(paste0('method "', method,'" does not match BRUV, DOV, or ROV'))
  }
  
  # if (method == "ROV") {
  #   warning('ROV specific method not implemented, using `method = "DOV" may work.')
  # }
  
  if (method == "ROV") {
    # Variables expected in field metadata
    expected <- c("sample",
                  "date_time",
                  "location",
                  "site",
                  "status",
                  "dbca_zone",
                  "dbca_sanctuary",
                  "depth",
                  "field_note",
                  "longitude",
                  "latitude",
                  "lcam",
                  "rcam",
                  "pilot",
                  "transects",
                  "raw_hdd",
                  "backup_hdd",
                  "visibility",
                  "footage_useable"
    )
    
    # Variables which if missing will cause an error
    vital <- c("sample",
               "date_time",
               "longitude",
               "latitude",
               "lcam",
               "rcam",
               "transects"
    )
  }
  
  if (method == "BRUV") {
    # Variables expected in field metadata
    expected <- c("sample",
                  "date_time",
                  "location",
                  "site",
                  "status",
                  "dbca_zone",
                  "dbca_sanctuary",
                  "depth",
                  "field_note",
                  "longitude",
                  "latitude",
                  "lcam",
                  "rcam",
                  "raw_hdd",
                  "backup_hdd",
                  "visibility",
                  "fov",
                  "footage_useable"
    )
    
    # Variables which if missing will cause an error
    vital <- c("sample",
               "date_time",
               "longitude",
               "latitude",
               "lcam",
               "rcam"
    )
  }
  
  if (method == "DOV") {
    # Variables expected in field metadata
    expected <- c("sample",
                  "date_time",
                  "location",
                  "site",
                  "status",
                  "dbca_zone",
                  "dbca_sanctuary",
                  "depth",
                  "field_note",
                  "longitude",
                  "latitude",
                  "lcam",
                  "rcam",
                  "operator",
                  "transects",
                  "raw_hdd",
                  "backup_hdd",
                  "visibility",
                  "footage_useable"
    )
    
    # Variables which if missing will cause an error
    vital <- c("sample",
               "date_time",
               "longitude",
               "latitude",
               "lcam",
               "rcam",
               "transects"
    )
  }
  
  # Output list
  list("expected" = expected,
       "vital" = vital)
  
}

#' Add missing variables to field metadata
#'
#' @description
#' Add blank columns for non-vital variables missing from DBCA BRUV/DOV field 
#' metadata. 
#'
#' @param x A data frame containing field metadata. 
#' @param missing A vector containing the names of expected variables not found
#' field metadata 
#'
#' @examples
#' \dontrun{
#' field.data <- add_missing_variables(field.data, missing)
#' }
#'
#' @export

# TODO - test if adding formatted NAs makes any difference. If not we can simplify by getting details of missing non-vital columns (e.g., test <- field_variables() then test$expected[!test$expected %in% vital])


add_missing_variables <- function(x, missing) {
  
  # Formats for columns that are sometimes missing from field data (note vital columns cannot be missing)
  # Non-vital columns from BRUVS/DOVs
  missing.col.formats <- list(
    "location" = NA_character_,
    "site" = NA_character_,
    "status" = NA_character_,
    "dbca_zone" = NA_character_,
    "dbca_sanctuary" = NA_character_,
    "depth" = NA_character_,
    "field_note" = NA_character_,
    "raw_hdd" = NA_integer_,
    "backup_hdd" = NA_integer_,
    "visibility" = NA_integer_,
    "operator" = NA_character_, # DOV only
    "fov" = NA_integer_,        # BRUV only
    "footage_useable"  = NA_character_
  )
  
  
  missing.col.formats <- missing.col.formats[missing]
  
  x%>%
    cbind(missing.col.formats)
}
