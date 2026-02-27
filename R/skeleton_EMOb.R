#' Create an EMOb Skeleton
#'
#' @description
#' `skeleton_EMOb()` processes BRUV/DOV/ROV analysis datasheets and exports a 
#' structured EMOb skeleton `.txt` file for use in EventMeasure. The function writes 
#' a formatted EMOb skeleton file to disk and no object is returned to the R 
#' environment. The function is designed for both scripted and interactive use,
#'  depending on whether the input `x` is supplied.
#'
#' @details
#' When `x` is provided, it must be a `data.frame`.  If `x` is `NULL`, the 
#' user is prompted to choose an input dataset to use as `x`. This should be 
#' an analysis datasheet within a campaign on iLab_Fish. 
#' 
#' The columns in `x` are validated to confirm that all columns listed in the 
#' internal vector `required.inputs` are present. If any required column is
#' missing, the function stops and informs the user which columns are missing.
#'
#' @param x `data.frame` or `NULL`.  
#'   Input dataset. Must contain all required columns defined in
#'   `required.inputs`. If `NULL`, an interactive file chooser is launched.
#'
#' @param out.path `character` or `NULL`.  
#'   File path for the output `.txt` file. If `NULL`, a path is generated
#'   automatically.
#'   
#' @examples
#' \dontrun{
#' # Fully interactive use:
#' skeleton_EMOb()
#'
#' # Scripted use with a data frame:
#' skeleton_EMOb(
#'   x = example_df,
#'   out.path = "outputs/emob_skeleton.txt"
#' )
#' }
#' 
#' @importFrom janitor clean_names
#' @importFrom openxlsx2 read_xlsx
#' @importFrom rstudioapi selectFile showDialog
#' @importFrom dplyr select rename all_of any_of
#' @importFrom utils write.table
#'
#' @export

# usethis::use_package("utils")
# usethis::use_package("janitor")
# usethis::use_package("openxlsx2")
# usethis::use_package("rstudioapi")
# usethis::use_package("dplyr")

# TODO - Update to allow skeleton file creation using older/unformatted datasheets

skeleton_emob <- function(x = NULL, out.path = NULL) {
  
  # Set up ----

  # > Skeleton File Variables ----
  
  # Column renaming to fit Event Measure Formatting
  skeleton.names <- c(
    # EM Name = Old Name
    "OpCode"  = "sample", 
    "Depth"   = "depth" 
  )
  
  # Columns (and order) required in Event Measure 
  skeleton.cols <- c(
    
    # Columns 1-4 must be:
    "sample",
    "TapeReader", 
    "depth", 
    "Comment", 
    
    # Columns 5-12 can be anything else from metadata
    "date_time",
    "site", 
    "location", 
    "status", 
    "dbca_zone", 
    "dbca_sanctuary", 
    "latitude", 
    "longitude",
    
    # Columns 13-19 are as follows:
    "video_location",    # Location of video files 
    "CAL_L",             # Location of .CAM file for left camera 
    "CAL_R",             # Location of .CAM file for right camera
    "L_stream_length",   # Number of videos in LCam Stream
    "L_video_1",         # Filename for first LCAM file
    "R_stream_length",   # Number of videos in RCam Stream
    "R_video_1"          # Filename for first RCAM file
  )
  
  required.inputs <- c(skeleton.cols[1],skeleton.cols[3], skeleton.cols[5:12])
  
  # > Load data ----
  # uses inout dataframe or allows user to select a file
  
  if (!is.null(x)) {
    # set skeleton data if a dataframe is provided
    if (!is(x, 'data.frame')) {
      stop("data.frame expected from input x")
    }
    skeleton.data <- x
    
  } else {
    # Interactive select file if no dataframe is provided
    
    # select filte 
    fp <- rstudioapi::selectFile(
      caption = "Select labsheet",
      path =  get_dir("ilab_fish"),
      # path = normalizePath(grep("iLab_fish", 
      #                           list.dirs(Sys.getenv("OneDriveCommercial"), recursive = FALSE), value = TRUE)),
      filter = "Excel Files (*.xlsx)"  # filter to only show csv/xlsx
    )
    
    # Check to ensure file is loaded
    if (is.null(fp)) {
      stop("No file selected")
    }
    
    # Read in sheet two of file
    x <- openxlsx2::read_xlsx(fp, sheet = "ANALYSIS", detectDates = TRUE)%>%
      janitor::clean_names()#%>%
      # Reformat Time (takes excel decimal time and returns required format for checkEM)
      # dplyr::mutate(date_time = openxlsx::convertToDateTime(date_time))
    
    skeleton.data <- x
    
  }
  
  # > Check Columns ----
  # Error if missing any of the required columns
  if (any(!required.inputs %in% names(skeleton.data))) {
    stop(paste0("Expected columns missing from input x:\n",
                paste(required.inputs[which(!required.inputs %in% names(skeleton.data))], collapse = ", ")))
  }
  
  # > Format Skeleton ----
  # add additional columns (those not in required inputs)
  skeleton.data[setdiff(skeleton.cols, names(skeleton.data))] <- NA
  
  # Select required columns
  skeleton.data <- skeleton.data%>%
    dplyr::select(dplyr::all_of(skeleton.cols))%>%      # reorder columns
    dplyr::rename(dplyr::any_of(skeleton.names)) # rename columns to required format for EventMeasure
  
  # > Export skeleton -----

  # Set initial output directory 
  if (all(is.null(out.path), exists("fp"))) {
  out.path <- paste0(sub("_[^_]*$", "", fp), "_EMOb_skeleton.txt")
  
  } else if  (is.null(out.path)) {
    out.path <- file.path(grep("iLab_fish", 
                       list.dirs(Sys.getenv("OneDriveCommercial"), 
                                 recursive = FALSE),
                       value = TRUE),
              "_EMOb_skeleton.txt")}
  
  # Interactive pop-up for where to save file
  skeleton.path <- rstudioapi::selectFile(
    caption = "Save As",
    label = "Save",
    path = out.path, # Set initial path and name
    filter = "Text Files (*.txt)",     # export as csv unless otherwise specified
    existing = FALSE                   # Allow user to save a new file
  )
  
  # Export file/warn user if not possible
  if (is.null(skeleton.path)){ 
    
    # Warn user if no file path is set
    rstudioapi::showDialog(title = "WARNING:",
                           message = "No file path set for EMOb skeleton.\n Skipping file creation."
    )
    
  } else {
    
    # Update file extension if missing or wrong
    if (tools::file_ext(skeleton.path) != "txt") {
      skeleton.path <- paste0(tools::file_path_sans_ext(skeleton.path), ".txt")
    }
    
    # Export EMOB skeleton
    utils::write.table(skeleton.data, file = skeleton.path, 
                sep = "\t", na = "", row.names = FALSE, col.names = TRUE, quote = FALSE)
    
    # Print when successful
    print("EMOb skeleton created")
  }
  
}
