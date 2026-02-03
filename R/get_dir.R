#' Get a OneDrive directory path
#'
#' @description
#' Validates and returns the full path to a first-level folder (and optional
#' subfolder) under a configurable root directory (defaults to OneDrive Buisness).
#' This allows DBCA personnel to create shareable scripts that can access OneDrive 
#' files synced to their PC. 
#'
#' @usage get_dir(
#' dir,
#' sub.folder = NULL,
#' root = Sys.getenv("OneDriveCommercial"),
#' exclude = c("Apps", "Attachments", "Desktop", "Documents", "Pictures",
#'             "OneNote Loop Files", "Microsoft Copilot Chat Files", "Microsoft Teams Chat Files")
#'             )
#' 
#' @param dir Character scalar. Name of a top-level folder under \code{root} (e.g.
#' "ilab-fish"). Only requires a partial match for a folder and errors if multiple 
#' matches are found.
#' @param sub.folder Optional character scalar. Subdirectory within \code{dir} (e.g.
#' "NCMP/2023-03-BRUV").
#' @param root Character scalar. Sets the root for \code{dir}. Defaults to
#'   \code{Sys.getenv("OneDriveCommercial")} which should be SharePoint for DBCA
#'   staff who sync sharepoint to their PC. You can override (e.g.,\code{Sys.getenv("OneDrive")}).
#'  @param exclude Character vector of folder names to exclude from the available
#'  set. Defaults to common system/personal folders.
#'
#' @return A length-1 character vector: the normalized absolute path.
#' 
#' @seealso \link[base]{list.dirs}, \link[base]{file.path}, \link[base]{Sys.getenv}
#'
#' @examples
#' \dontrun{
#' # Exact match to a OneDrive Business top-level folder:
#' get_dir("Projects")
#'
#' # With an existing subfolder:
#' get_dir("ilab_fish", sub.folder = "NCMP/2024-02_BRUV")
#'
#' # Use a different root (e.g., personal OneDrive):
#' get_dir("Photos", root = Sys.getenv("OneDrive"))
#' }
#' 
#' @export

get_dir <- function(dir, 
                    sub.folder = NULL, 
                    root = Sys.getenv("OneDriveCommercial"),
                    exclude = c("Apps", "Attachments", "Desktop",
                                "Documents", "Pictures", "OneNote Loop Files",
                                "Microsoft Copilot Chat Files", "Microsoft Teams Chat Files"
                    )
) {
  
  # Check dir format
  stopifnot("'dir' must be a single character string" = is.character(dir), length(dir) == 1L)
  
  # Check root
  stopifnot("'root' directory not found" = dir.exists(root))
  
  # Check subfolder
  if (!is.null(sub.folder)) {
    stopifnot("'sub.folder' must be a single character string" = is.character(sub.folder), length(sub.folder) == 1L)
  }
  
  # Get list of currently availbe dirs on users OneDrive
  available <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  
  # Exclude common system/personal folders if present
  if (!is.null(exclude) && length(exclude)) {
    available <- setdiff(available, exclude)
  }
  
  # Ignore case and find base directory
  available.lc <- tolower(available)
  dir.lc   <- tolower(dir)
  
  # Find directory allowing for partial matches
  pick <- NULL
  hits <- grepl(dir.lc, available.lc, fixed = TRUE)
  if (sum(hits) == 1L) {
    pick <- available[hits]
  } else if (sum(hits) > 1L) {
    
    # Error if multiple hits for dir
    stop(
      sprintf(
        "Partial match for dir = '%s' is ambiguous. Candidate folders:\n- %s",
        dir, paste(available[hits], collapse = "\n- ")
      ),
      call. = FALSE
    )
  }
  
  # Error if dir does not match available (and return the available folders)
  if (is.null(pick)) {
    msg <- if (length(available)) {
      paste("Available folders:\n- ", paste(available, collapse = "\n- "), sep = "")
    } else {
      "No top-level folders found under the OneDrive root."
    }
    stop(sprintf("Could not resolve 'dir' = '%s' under: %s\n\n%s", dir, root, msg), call. = FALSE)
  }
  
  # Create path 
  path <- file.path(root, pick)
  
  # Expand path if sub.folder provided
  if (!is.null(sub.folder)) {
    path <- file.path(path, sub.folder)
    
    if (!dir.exists(path)) {
      stop(sprintf("Path does not exist: %s", path), call. = FALSE)
    }
  }
  
  return(normalizePath(path))
}
