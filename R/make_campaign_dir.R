# ____________________________________________________________________
# Asset:   Fish
# Project: iLab Fish Data Management
# Task:    Create Campaign folders
# Data:    Campaign base directory
# Author:  Mike Taylor
# Date:    December 2025
# ____________________________________________________________________

# Check for the existence of a campaign directory and if missing create a new campaign directory

# Inputs:
# camp.dir - either NULL or a filepath to a campaign directory

# If is.null(camp.dir) the user is prompted to provide a Marine Park ID and set the appropriate date and method for the campaign

# If the file path in camp.dir does not exist the user is prompted to approve the creation of a new folder based on the template

# If the file path in camp.dir exists the folders in this directory are checked and the user is prompted to approve creation of any missing template folders.


#' Make Campaign Directory
#'
#' @description
#' Interactive creation of new campaign directory following standardised templates,
#' or addition of missing folders to existing campaign directory folders. If any of 
#' \code{marine.park, date, or method == NULL} interactive prompts allow the user 
#' to set these details.
#' 
#' @param marine.park character vector giving the marine park identifier. Defaults 
#' to \code{NULL} which prompts the user using a pop-up.
#' @param date character vector giving the campaigns year and month (e.g. 2023-01). 
#' Defaults to \code{NULL} which prompts the user using a pop-up. 
#' @param method character vector giving the sample collection method. Must be one 
#' of 'BRUV', 'DOV', 'ROV', or 'NULL'. Defaults to \code{NULL} which prompts the 
#' user using a pop-up.
#' @param base.dir character vector setting the root directory where folders are 
#' created. Defaults to ilab-fish and should only be changed for testing.
#' 
#' @examples
#' \dontrun{
#' make_campaign_dir() # Interactive
#' make_campaign_dir(marine.park = "RIMR", date = "2025-02", method = "BRUV") # Provide file path
#' }
#' 
#' @importFrom rstudioapi showPrompt showQuestion
#' 
#' @export

# TODO - add verbose option to run function without pop-ups

make_campaign_dir <- function(marine.park = NULL, date = NULL, method = NULL, base.dir = get_dir("ilab_fish")) {
  
  # > Check/Set Marine park ----
  if (is.null(marine.park)) {
    
    marine.park <- select.list(unique(parkID$parkID), preselect = NULL, multiple = FALSE,
                       title = "Select Marine Park ID:", graphics = TRUE)
    
    stopifnot("User aborted at marine park selection" = marine.park !="")
  }
  
  if (!marine.park %in% unique(parkID$parkID)){
    stop(
      sprintf("Invalid 'marine.park' detected.\n\n%s",
              paste("Marine Park Abbreviations:\n-", paste(unique(parkID$parkID), collapse = "\n- "))),
      call. = FALSE
    )
    
  }
  
  # Check/Set date ----
  if (is.null(date)) {
   
    # Add Date/Method
    date <- rstudioapi::showPrompt(
      "Set Campaign Year-Month",
      "format = YYYY-MM (e.g. 2026-01):",
      "YYYY-MM"
    )
    
    stopifnot("User aborted at date selection" = !is.null(date))
    
  }
  
  # parse date for checking
  check.date <- grepl("^(199\\d|20\\d{2}|2100)[-](0[1-9]|1[0-2])$", date, perl = TRUE)
  if (!check.date) {
    stop(sprintf("Invalid 'date' = %s, expected in the format Year-Month (e.g. 2025-01)", date),  call. = FALSE)
  }
  
  # Check/Set method
  if (is.null(method)) {
    
    method <- select.list(c("BRUV","DOV","ROV"), preselect = NULL, multiple = FALSE,
                               title = "Select sampling method:", graphics = TRUE)
    
    stopifnot("User aborted at method selection" = method !="")
  }
  
  stopifnot("Invalid 'method' detected, must be one of BRUV, DOV, ROV" = method %in%c("BRUV", "DOV", "ROV"))
    
  
  camp.dir <-  file.path(
    base.dir,
    marine.park,
    paste(date,method, sep ="_")
  )
  
    # Create campaign folder template
    folders <- file.path(
      camp.dir,
      c(
        "Calibration",
        "Calibration/Post-CAL",
        "Calibration/Pre-CAL",
        "Database Output",
        "Database Output/Cleaned Data",
        "Database Output/To Check",
        "EMObs",
        "Habitat Output",
        "Habitat Images"
      )
    )
  
  if (!dir.exists(camp.dir)) {
    # If the campaign directory does not exist ask the user if they want to create a new one
    
    # Ask user if you want to create a new template folder if it is missing
    add.campaign <- rstudioapi::showQuestion(
      "Create Campaign Folders:",
      paste0("Do you want to create a new campaign folder in the following location? \n\n ", camp.dir),
      ok = "Yes",
      cancel = "No")
    
    # Create new folder template
    if (add.campaign) {
      sapply(folders, dir.create, recursive = TRUE)
    }
    
  } else if (any(!dir.exists(folders))) {
    # If the campaign directory exists but there are folders missing from the template, ask the user if they want to add the missing folders.
    
    # Check if any folders are missing from the template  
    missing.folders <- folders[!file.exists(folders)]
    
    # Ask user if they want to add the missing folders
    add.missing.campaign <- rstudioapi::showQuestion(
      "Create Campaign Folders:",
      paste0("Do you want to add missing folders to ", camp.dir,"?\n\n",
             paste(gsub(camp.dir, "", missing.folders, fixed = TRUE), collapse = "\n")),
      ok = "Yes",
      cancel = "No")
    
    # Add missing folders
    if (add.missing.campaign) {
      sapply(missing.folders, dir.create, recursive = TRUE)
    }
  }
}
