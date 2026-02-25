#' Validate Campaign Directories Against a Template
#'
#' @description
#' `check_structure()` validates iLab-fish campaign directories against a template
#' specification (created by the internal function `campaign_structure`) that 
#' describes the allowed directory and file layout.
#'
#' @param MP (Optional) acronym(s) of target marine park(s).
#' @param campaign (Optional) name of campaign(s) in the target marine park(s). 
#' Requires `MP` to be defined.
#' @param root A character path to the directory whose structure should be
#'   validated (Default = iLab::get_dir("ilab_fish")).
#
#' @return A list containing a summary data.frame detailing the matches per rule 
#' in the template specification, a data.frame of unexpected file paths found, and 
#' a data frame detailing all matches found.
#'
#' @examples
#' \dontrun{
#'
#' # Run the check on all of iLab-Fish
#' # check_structure()
#' 
#' # Run the check for specific campaign
#' checks <- check_structure(MP = "RIMR", campaign = "2023-03_BRUV")
#' # Print summary
#' checks$summary
#'
#' # Unexpected files
#' checks$unexpected
#' }
#'
#' @export

# TODO - remove reliance on tibble/purrr/stringr
# TODO - update roxygen to import functions/packages
# TODO - 
# TODO - add summary printout if verbose = T
# TODO - calculate min number of expected files
# TODO - allow method input to switch between spec file creation method as required (e.g., for !Essential_files or Database outputs)


# # install.packages(c("fs", "stringr", "purrr", "dplyr", "tibble"))
# library(fs)
# library(stringr)
# library(purrr)
# library(dplyr)
# library(tibble)
# 
# checks <- check_structure(MP = "RIMR", campaign = "2023-03_BRUV")
# 
# View(checks$summary)
# View(checks$matches)
# View(checks$unexpected)
# View(checks$details)

check_structure <- function(MP = NULL, campaign = NULL, root = iLab::get_dir("ilab_fish")){
  
  # check inputs ----
  
  # Check marine park matches parkID options
  if (!all(MP %in% iLab:::parkID$parkID)) {
    stop(sprintf("Invalid 'MP' (%s) detected.", paste(MP [!MP %in% iLab:::parkID$parkID], collapse = ", ")), call. = FALSE)
  }
  
  # Ensure a single MP is provided if targeting specific campaign
  if (is.null(MP) && !is.null(campaign)) {stop("To check structure of a specific campaign provide the associated marine park acronym.")} else if (length(MP)>1 && !is.null(campaign)) {stop("To check structure of a specific campaign provide a single associated marine park acronym.")}
  
  # Ensure CAMPAIGN IS LENGTH 1 (IF PROVIDED)
  if (!is.null(campaign) && length(campaign) != 1) {stop("length(campaign) != 1.")}
  
  # Ensure a single MP is provided 
  
  root <- fs::path_abs(root)
  
  # Check root dir exists
  if (!fs::dir_exists(root)) {stop(sprintf("'root (%s) is not a valid directory.", root), call. = FALSE)}
  
  # get paths ----
  
  # Walk the tree
  all_paths <- suppressWarnings(fs::dir_ls(root, recurse = TRUE, type = "any", fail = FALSE))
  
  # Also top-level entries (dir_ls on recurse sometimes excludes the root itself as an entry)
  # top_paths <- fs::dir_ls(root, recurse = FALSE, type = "any", fail = FALSE)
  # all_paths <- unique(c(top_paths, all_paths))
  
  # Remove habitat/external data/Essential Files folders
  # all_paths <- all_paths[!grepl("Habitat|External Data|!Essential_Files",all_paths)]
  all_paths <- all_paths[!grepl(file.path(root, "Habitat"),all_paths)]
  all_paths <- all_paths[!grepl(file.path(root, "External Data"),all_paths)]
  all_paths <- all_paths[!grepl(file.path(root, "!Essential_Files"),all_paths)]
  
  # Filter park/campaign ----
  
  # Filter to a specific marine park or parks
  if (!is.null(MP)) {
    all_paths <- all_paths[grepl(paste(MP, collapse = "|"),all_paths)]
  }
  
  # Filter to a specific campaign
  if (!is.null(campaign)) {
    all_paths <- all_paths[grepl(fs::path_join(c(MP,campaign)),all_paths)]
  }
  
  # Path errors -----
  
  # Isolate paths that are too long (i.e. >260 characters)
  error_paths <- all_paths[which(nchar(all_paths) >260)]
  
  # Return warning if any paths greater than 260 characters exist
  if (length(error_paths)) {
    if (verbose) {
      warning(
        sprintf("%s file paths >260 characters detected (these files cannot be accessed on most operating systems):\n%s",
                length(error_paths), paste(error_paths, collapse = "\n")), call. = FALSE)
    } else {
      warning(
        sprintf("%s file paths >260 characters detected (these files cannot be accessed on most operating systems). Re-run with verbose = TRUE to view path names.", length(error_paths)), call. = FALSE)
    }
    
    # Remove long file paths
    all_paths <- all_paths[which(nchar(all_paths) <261)]
  }
  
  # Create dataframe ----
  
  # Get tibble of all paths
  entries <- if (length(all_paths) == 0) {
    # Empty data frame if not paths remaining
    tibble(path = character(), type_actual = character(), rel_dir = character(), name = character())
  } else {
    tibble(
      # type (directory or file)
      type_actual = ifelse(fs::is_dir(all_paths), "dir", "file"),
      # directory/file name (without path)
      name = fs::path_file(all_paths),
      # file extension
      ext = ifelse(fs::is_dir(all_paths), NA, fs::path_ext(all_paths)),
      # path relative to root directory
      rel_path = fs::path_rel(all_paths, start = root),
      # Path without directory/filename
      rel_dir = {
        rp <- fs::path_rel(all_paths, start = root)
        parent <- fs::path_dir(rp)
        parent[parent == "."] <- ""
        parent
      },
      # full file path
      path = fs::path_abs(all_paths),
    ) %>%
      # Remove duplicate paths if present 
      distinct(path, .keep_all = TRUE)
  }
  
  # # Apply global ignores (by name only, not full path)
  # if (length(ignore_patterns)) {
  #   to_ignore <- reduce(ignore_patterns, \(acc, pat) acc | str_detect(entries$name, regex(pat)), .init = rep(FALSE, nrow(entries)))
  #   entries <- entries[!to_ignore, , drop = FALSE]
  # }
  
  
  # Helper: case-aware regex matching
  # re_match <- function(x, pattern, case_sensitive = TRUE) {
  #   str_detect(x, if (case_sensitive) pattern else regex(pattern, ignore_case = TRUE))
  # }
  
  # Template structure ----
  
  # Get folder structure (either generic or specific to the selected MP/campaign)
  spec <- campaign_structure(MP = MP, campaign = campaign)
  
  # Match paths ----
  
  # name matching (regex)
  re_name <- function(x, pattern, case_sensitive = TRUE) {
    stringr::str_detect(x, if (case_sensitive) pattern else stringr::regex(pattern, ignore_case = TRUE))
  }
  # directory matching (regex)
  re_dir <- function(x, pattern, case_sensitive = TRUE) {
    stringr::str_detect(x, if (case_sensitive) pattern else stringr::regex(pattern, ignore_case = TRUE))
  }
  
  # Get folder structure (either generic or specific to the selected MP/campaign)
  matches_by_spec <- purrr::pmap(
    .l = list(
      
      target = spec$target,
      type = spec$type,
      rel_dir_regex = spec$rel_dir_regex,             # <- folder regex
      name_regex = spec$name_regex,                   # <- file/dir name regex
      case_sensitive = spec$case_sensitive
    ),
    .f = function(target, type, rel_dir_regex, name_regex, case_sensitive) {
      subset <- entries %>%
        dplyr::filter(
          type_actual == type,
          re_dir(rel_dir, rel_dir_regex, case_sensitive)  # folder must match
        ) %>%
        dplyr::mutate(
          matches = re_name(name, name_regex, case_sensitive)       # AND name must match
        )%>%
        dplyr::mutate(target = target)
      
      if (!nrow(subset)) {
        # produce empty frame with a matches column
        subset <- tibble::tibble(
          type_actual = character(),
          name = character(),
          ext = character(),
          rel_path = character(),
          rel_dir = character(),
          matches = logical(),
          target = character()
        )
      }
      subset
    }
  )
  
  
  # Outputs ----
  
  # Summary per rule (min/max/required)
  summary <- map2_dfr(matches_by_spec, seq_len(nrow(spec)), function(df, i) {
    s <- spec[i, ]
    n <- sum(df$matches, na.rm = TRUE)
    
    status <- case_when(
      n >= s$min & n <= s$max ~ "OK",
      n < s$min & isTRUE(s$required)  ~ "MISSING",
      n < s$min & !isTRUE(s$required) ~ "MISSING_OPTIONAL",
      n > s$max                       ~ "EXCEEDS_MAX",
      TRUE                            ~ "CHECK"
    )
    
    tibble(
      spec_row = i,
      target = s$target,
      type = s$type,
      status = status,
      matches_count = n,
      required = s$required,
      min = s$min,
      max = s$max,
      case_sensitive = s$case_sensitive,
      rel_dir_regex = s$rel_dir_regex,
      name_regex = s$name_regex
    )
  })%>%
    dplyr::rename(target_row = spec_row)
  
  # Get paths explained by at least one rule
  explained_paths <- unique(unlist(lapply(matches_by_spec, function(df) df$path[df$matches])))
  explained_paths <- explained_paths[!is.na(explained_paths)]
  
  # Unexpected entries output
  unexpected <- entries %>%
    filter(!(path %in% explained_paths)) %>%
    select(type_actual, name, rel_dir, path)%>%
    dplyr::rename(type = type_actual, full_path = path)
  
  if(nrow(unexpected)>0) {
    warning(
      sprintf("%s non matching file paths found (see $unexpected for details)", nrow(unexpected)), call. = FALSE)
  }
  
  
  # Detailed Matches crosswalk (useful for debugging)
  matches <- map2_dfr(matches_by_spec, seq_len(nrow(spec)), function(df, i) {
    if (!nrow(df)) return(tibble())
    df %>%
      dplyr::mutate(spec_row = i, matched = matches) %>%
      dplyr::select(spec_row, target, type_actual, rel_dir, name, path, matched)
    
  }) %>% dplyr::arrange(spec_row, rel_dir, name)%>%
    # Only return matches (sometimes including FALSE is useful for debbuging)
    dplyr::filter(matched == TRUE)%>%
    # Select and rename required columns
    dplyr::select(type_actual, name, rel_dir, target, spec_row, rel_dir, path)%>%
    dplyr::rename(type = type_actual, target_matched = target, target_row = spec_row, full_path = path)
  
  # Output lists
  list(
    # root = root,
    summary = summary,
    unexpected = unexpected,
    matches = matches
  )
  
}


#'Build the default directory/file specification
#'
#' @description
#' Returns a data frame describing the expected structure. This function
#' centralizes defaults and makes specs testable/reusable.
#'
#' @param MP (Optional) acronym(s) of target marine park(s).
#' @param campaign (Optional) name of campaign(s) in the target marine park(s). 
#' Requires `MP` to be defined.
#' 
#' @return A data frame with the required columns for `check_structure()`.
#'   Required columns:
#'   \describe{
#'     \item{type}{`"file"` or `"dir"`.}
#'     \item{rel_dir_pattern}{Regex matching the **relative parent directory**.}
#'     \item{name_regex}{Regex matching the **basename** (file or folder name).}
#'     \item{required}{Logical, whether at least `min` matches are required.}
#'     \item{min}{Minimum number of matches (per rule).}
#'     \item{max}{Maximum allowed matches (use `Inf` for unlimited).}
#'     \item{case_sensitive}{Logical, case sensitivity for file/dir names.}
#'     \item{rel_dir_case_sensitive}{Logical, case sensitivity for folder matching.}

# TODO - add a way to extract the number of campaigns if providing a specific marine park
# TODO - match expected file naming structure for specific projects and/or files
# TODO - calculate min number of files when specific campaigns are provided....
# TODO - improve so that it looks for specific folders per park (e.g. within RIMR/2022-04_BRUV there should be one folder called "Data Outputs") at the moment we jest look for general matches rather than missing files?

campaign_structure <- function(MP = NULL, campaign = NULL) {
  
  # n.parks <- if(is.null(MP)) {length(unique(iLab:::parkID$parkID))} else {length(MP)}
  # n.campaigns <- if(is.null(campaign)) {Inf} else {length(campaign)}
  
  # Potential parks
  if(is.null(MP)) {
    # Generic
    parks <- paste(unique(iLab:::parkID$parkID), collapse = "|")
    camp.text <- paste(unique(iLab:::parkID$campaign_text), collapse = "|")
    n.parks <- length(unique(iLab:::parkID$parkID))
  } else {
    # User specificed
    parks <- paste(unique(MP), collapse = "|")
    camp.text <- paste(unique(iLab:::parkID$campaign_text[which(iLab:::parkID$parkID %in% MP)]), collapse = "|")
    n.parks <- length(unique(MP))
  }
  
  # campaign info (specified/generic)
  n.campaigns <- if (!is.null(campaign)) {1} else {Inf}
  campaign.helper <- if (!is.null(campaign)) {paste(campaign, collapse = "|")
    } else {"199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](BRUV|DOV|ROV|UVC"}
  
  
  # Function to quickly format regex (specific to current use)
  create_regex <- function(x, path = FALSE) {
    if (length(x)==1) {
      paste0("^(", x, ")$")
    } else {
      if (path) {
        # Create a file pathway
        paste0("^(", paste(x, collapse = ")/("), ")$")
      } else {
        # Create regex for multiple options
        paste0("^(", paste(x, collapse = "|"), ")$")
      }
    }
  }
  
  
  # Regex creation ----
  
  ## Parkid ----
  # park <- paste0("^(", if (!is.null(MP)) {paste(unique(MP), collapse = "|")} else {paste(unique(iLab:::parkID$parkID), collapse = "|")},")$")
  # park <- paste0("^(", parks,")$")
  park <- create_regex(parks)
  
  ## Campaign folders  ----
  # campaigns <- if (!is.null(campaign)) {paste0("^(", paste(campaign, collapse = "|"), ")$")} else {"^(199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](BRUV|DOV|ROV|UVC)$"}
  campaigns <- create_regex(campaign.helper)
  
  
  ## Park/Campaign filepath -----
  # (e.g. RIMR/2024-04_BRUV) 
  # park.campaign <- paste(sub("\\$","", park),sub("\\^","",campaigns), sep = "/")
  park.campaign <- create_regex(c(parks,campaign.helper), path = TRUE)
  
  ## Template campaign folders -----
  # Base level names
  campaign.folder.vector <- c("Calibration",
                              "Database Output",
                              "EMObs",
                              "Habitat Output",
                              "Habitat Images")
  
  # Regex
  # campaign.folders <- paste0("^(", paste(campaign.folder.vector,
  #                                        collapse = "|"),
  #                            ")$")
  campaign.folders <- create_regex(campaign.folder.vector)
  
  
  # Regex for park, campaign, and template folders (e.g. RIMR/2024-04_BRUV/Calibration) 
  # park.campaign.folders <- paste(sub("\\$","", park.campaign),sub("\\^","",campaign.folders), sep = "/")
  # stringr::str_view(c("RIMR/2024-1", "RIMR/2024-01", "RIMR/2024-01_BRUV",
  # "RIMR/2024-01_BRUV/Calibration", "RIMR/2024-01_BRUV/Database"),park.campaign.folders)
  
  ## Calibration folder/files ----
  park.campaign.cal <- create_regex(c(parks, campaign.helper, "Calibration"), path = TRUE)  
  
  #Pre/post-Cal folders
  campaign.cal <- "^(Pre-Cal|Post-Cal)$"
  
  # Pre/postcal folder paths 
  # campaign.cal.folders <- paste(sub("\\$","", park.campaign.folders),sub("\\^","",campaign.cal), sep = "/")
  campaign.cal.folders <- create_regex(c(parks,campaign.helper, "Calibration", "Pre-Cal|Post-Cal"), TRUE)
  
  ## Database folder/files ----
  
  park.campaign.data <- create_regex(c(parks, campaign.helper, "Database Output"), path = TRUE)  
  
  # Campaign data folders
  campaign.data <- "^(Cleaned Data|To Check)$"
  
  # Data folder paths
  # campaign.data.folders <- paste(sub("\\$","", park.campaign.folders),sub("\\^","",campaign.data), sep = "/")
  campaign.data.folders <- create_regex(c(parks,campaign.helper, "Database Output", "Cleaned Data|To Check"), TRUE)
  
  ## EMObs----
  
  # campaign.emob.folder <- paste(sub("\\$","", park.campaign), "EMObs$", sep = "/")
  campaign.emob.folder <- create_regex(c(parks,campaign.helper, "EMObs"), TRUE)
  
  ## Habitat Images ----
  # campaign.h.image.folder <- paste(sub("\\$","", park.campaign), "Habitat Images$", sep = "/")
  campaign.h.image.folder <- create_regex(c(parks,campaign.helper, "Habitat Images"), TRUE)
  
  
  ## Field/data/skeleton ----
  fd <- paste0("^(199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](", # YYYY-MM_
               parks,  # Park acronym
               ")[_](BRUV|DOV|ROV|UVC)[_]FIELDDATA.csv$") #_Method_FIELDATA.csv
  ld <- paste0("^(199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](",
               camp.text,
               ")[_](stereoBRUV|stereoDOV|stereoROV|UVC)[_]DATASHEET.xlsx$")  #_Method_DATASHEET.xlsx
  skeleton <- paste0("^(199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](",
                     parks,
                     ")[_](BRUV|DOV|ROV|UVC)[_]EMOb[_]skeleton.txt$") #_Method_skeleton.txt
  
  # Target structure table ---- 
  spec <- tibble::tribble(
    ~target,        ~type,  ~rel_dir_regex,          ~name_regex,                  ~required, ~min, ~max, ~case_sensitive,
    
    # Park folders
    "ParkID",       "dir",  "^()$",                  park,                         TRUE, 1, n.parks, TRUE,
    
    # Campaign folders inside parks
    "campaign",     "dir",  park,                    campaigns,                    TRUE, 1, n.campaigns, TRUE,
    
    # Campaign template folders
    "csubfolders",  "dir",  park.campaign,           campaign.folders,             TRUE, 1, length(campaign.folder.vector)*n.parks*n.campaigns, TRUE,
    
    # Campaign calibration folders
    "cal.folders",  "dir",  park.campaign.cal,       campaign.cal,                 TRUE, 1, n.parks*n.campaigns*2, TRUE,
    
    # Campaign calibration files
    "cal.files",    "file", campaign.cal.folders,    "\\.Cam$",                    TRUE, 1, Inf, TRUE,
    
    # Campaign Data folders
    "data.folders", "dir",  park.campaign.data,      campaign.data,                TRUE, 1, n.parks*n.campaigns*2, TRUE,
    
    # Campaign data files
    "data.files",   "file", campaign.data.folders,   "(\\.txt$|\\.csv$)",          TRUE, 1, Inf, TRUE,
    
    # Campaign Emobs files
    "emob.files",   "file", campaign.emob.folder,    "(\\.EMObs$|\\.EMObs_AUTO$)", TRUE, 1, Inf, TRUE,
    
    # Campaign Habitat data files
    # "habitat.data", "file", campaign.h.data.folder,  "\\.txt$",                  TRUE, 1, Inf, TRUE,
    
    # Campaign Habitat files
    "habitat.imgs", "file", campaign.h.image.folder, "\\.jpg$",                    TRUE, 1, Inf, TRUE,
    
    # EMOb skeleton
    "skeleton",     "file", park.campaign,           skeleton,                     FALSE, 1, n.parks*n.campaigns, TRUE,
    
    # Field datasheet
    "field.meta",   "file", park.campaign,           fd,                           FALSE, 1, n.parks*n.campaigns, TRUE,
    
    # Analysis datasheet 
    "analysis.meta","file", park.campaign,          ld,                            TRUE, 1, n.parks*n.campaigns, TRUE
    
  )
  
  if (!is.null(campaign)) {
    spec <- spec%>%
      filter(target != "ParkID")
  }
  
  return(spec)
}




