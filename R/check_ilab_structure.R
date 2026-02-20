# TODO - improve so that it looks for specific folders per park (e.g. within RIMR/2022-04_BRUV there should be one folder called "Data Outputs") at the moment we jest look for general matches rather than missing files

# Fields:
# - type: "dir" or "file"
# - rel_dir: relative parent directory where the rule applies ("" means top-level)
# - name_regex: regex for the basename (anchored with ^$)
# - required: TRUE/FALSE
# - min/max: how many matching entries must/can exist
# - case_sensitive: TRUE/FALSE

install.packages(c("fs", "stringr", "purrr", "dplyr", "tibble"))
library(fs)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)


# TODO - add a way to extract the number of campaigns if providing a specific marine park
# TODO - match expected file naming structure for specific projects and/or files
# TODO - test skeleton/field/analysis data matching

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
  campaign.helper <- if (!is.null(campaign)) {paste(campaign, collapse = "|")} else {"199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](BRUV|DOV|ROV|UVC"}
  
  
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
  fd <- paste0("^(199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](", parks, ")[_](BRUV|DOV|ROV|UVC)[_]FIELDATA.csv$")
  ld <- paste0("^(199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](", camp.text, ")[_](BRUV|DOV|ROV|UVC)[_]Datasheet.xlsx$")
  skeleton <- paste0("^(199[0-9]|2[0-9][0-9][0-9])[-](0[1-9]|1[0-2])[_](", parks, ")[_](BRUV|DOV|ROV|UVC)[_]EMOB[_]skeleton.txt$")
  # stringr::str_view(string = "2023-02_RIMR_BRUV_EMOB_Skeleton.txt", pattern = "[_]EMOB[_]skeleton.txt$")
  
  
  
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
    # "habitat.data", "file", campaign.h.data.folder,  "\\.txt$",                    TRUE, 1, Inf, TRUE,
    
    # Campaign Habitat files
    "habitat.imgs", "file", campaign.h.image.folder, "\\.jpg$",                    TRUE, 1, Inf, TRUE,
    
    # EMOb skeleton
    "skeleton",     "file", park.campaign,           skeleton,                               TRUE, 1, n.parks*n.campaigns, TRUE,
    
    # Field datasheet
    "field.meta",   "file", park.campaign,           fd,                                     TRUE, 1, n.parks*n.campaigns, TRUE,
    
    # Analysis datasheet 
    "analysis.meta","file", park.campaign,          ld,                                     TRUE, 1, n.parks*n.campaigns, TRUE
    
  )
  
  if (!is.null(campaign)) {
    spec <- spec%>%
      filter(target != "ParkID")
  }
  
  return(spec)
}


# TODO - add target summary of matches/summary 

check_structure <- function(MP = NULL, campaign = NULL, root = iLab::get_dir("ilab_fish")){#, spec) {
  
  # Check marine park matches parkID options
  if (!all(MP %in% iLab:::parkID$parkID)) {stop(sprintf("Invalid 'MP' (%s) detected.", paste(MP [!MP %in% iLab:::parkID$parkID], collapse = ", ")), call. = FALSE)}
  
  # Ensure a single MP is provided if targeting specific campaign
  if (is.null(MP) && !is.null(campaign)) {stop("To check structure of a specific campaign provide the associated marine park acronym.")} else if (length(MP)>1 && !is.null(campaign)) {stop("To check structure of a specific campaign provide a single associated marine park acronym.")}
  
  # Ensure CAMPAIGN IS LENGTH 1 (IF PROVIDED)
  if (!is.null(campaign) && length(campaign) != 1) {stop("length(campaign) != 1.")}
  
  # Ensure a single MP is provided 
  
  root <- fs::path_abs(root)
  
  # Check root dir exists
  if (!fs::dir_exists(root)) {stop(sprintf("'root (%s) is not a valid directory.", root), call. = FALSE)}
  
  # Walk the tree
  all_paths <- suppressWarnings(fs::dir_ls(root, recurse = TRUE, type = "any", fail = FALSE))
  
  # Also top-level entries (dir_ls on recurse sometimes excludes the root itself as an entry)
  # top_paths <- fs::dir_ls(root, recurse = FALSE, type = "any", fail = FALSE)
  # all_paths <- unique(c(top_paths, all_paths))
  
  # Remove habitat/external data/Essential Files folders
  all_paths <- all_paths[!grepl("Habitat|External Data|!Essential_Files",all_paths)]
  
  # Filter to a specific marine park or parks
  if (!is.null(MP)) {
    all_paths <- all_paths[grepl(paste(MP, collapse = "|"),all_paths)]
  }
  
  # Filter to a specific campaign
  if (!is.null(campaign)) {
    all_paths <- all_paths[grepl(fs::path_join(c(MP,campaign)),all_paths)]
  }
  
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
  # 
  # Get folder structure (either generic or specific to the selected MP/campaign)
  spec <- campaign_structure(MP = MP, campaign = campaign)
  # 
  # 
  # # For each spec row, mark matches in that folder
  # matches_by_spec <- pmap(
  #   .l = list(
  #     type = spec$type,
  #     rel_dir = spec$rel_dir,
  #     name_regex = spec$name_regex,
  #     case_sensitive = spec$case_sensitive
  #   ),
  #   .f = function(type, rel_dir, name_regex, case_sensitive) {
  #     subset <- entries %>%
  #       filter(type_actual == type, rel_dir == rel_dir)
  #     
  #     if (nrow(subset) == 0) {
  #       subset %>% mutate(matches = FALSE)
  #     } else {
  #       subset %>% mutate(matches = re_match(name, name_regex, case_sensitive))
  #     }
  #   }
  # )
  
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
      type = spec$type,
      rel_dir_regex = spec$rel_dir_regex,             # <- folder regex
      name_regex = spec$name_regex,                   # <- file/dir name regex
      case_sensitive = spec$case_sensitive
    ),
    .f = function(type, rel_dir_regex, name_regex, case_sensitive) {
      subset <- entries %>%
        dplyr::filter(
          type_actual == type,
          re_dir(rel_dir, rel_dir_regex, case_sensitive)  # folder must match
        ) %>%
        dplyr::mutate(
          matches = re_name(name, name_regex, case_sensitive)       # AND name must match
        )
      
      if (!nrow(subset)) {
        # produce empty frame with a matches column
        subset <- tibble::tibble(
          path = character(),
          type_actual = character(),
          rel_dir = character(),
          name = character(),
          matches = logical()
        )
      }
      subset
    }
  )
  
  
  # Summary per rule (min/max/required)
  summary <- map2_dfr(matches_by_spec, seq_len(nrow(spec)), function(df, i) {
    s <- spec[i, ]
    n <- sum(df$matches, na.rm = TRUE)
    
    status <- case_when(
      n >= s$min & n <= s$max ~ "OK",
      n < s$min & isTRUE(s$required)  ~ "MISSING",
      n < s$min & !isTRUE(s$required) ~ "BELOW_MIN_OPTIONAL",
      n > s$max                       ~ "EXCEEDS_MAX",
      TRUE                            ~ "CHECK"
    )
    
    tibble(
      spec_row = i,
      type = s$type,
      # rel_dir = s$rel_dir,
      rel_dir = s$rel_dir_regex,
      name_regex = s$name_regex,
      required = s$required,
      min = s$min,
      max = s$max,
      case_sensitive = s$case_sensitive,
      matches_count = n,
      status = status
    )
  })
  
  # Which actual entries are explained by at least one rule?
  explained_paths <- unique(unlist(lapply(matches_by_spec, function(df) df$path[df$matches])))
  explained_paths <- explained_paths[!is.na(explained_paths)]
  
  # Matching entries output
  matches <- entries %>%
    filter(path %in% explained_paths) %>%
    select(type_actual, rel_dir, name)
  
  # Unexpected entries output
  unexpected <- entries %>%
    filter(!(path %in% explained_paths)) %>%
    select(type_actual, rel_dir, name)
  
  # Detailed crosswalk (useful for debugging)
  details <- map2_dfr(matches_by_spec, seq_len(nrow(spec)), function(df, i) {
    if (!nrow(df)) return(tibble())
    df %>%
      mutate(spec_row = i, matched = matches) %>%
      select(spec_row, type_actual, rel_dir, name, path, matched)
  }) %>% arrange(spec_row, rel_dir, name)
  
  list(
    root = root,
    summary = summary,
    matches = matches,
    unexpected = unexpected,
    details = details
  )
}


checks <- check_structure(MP = "RIMR", campaign = "2023-02_BRUV")
# checks <- check_structure(root = iLab::get_dir("ilab_fish"), spec = generic)

View(checks$summary)
View(checks$matches)
View(checks$unexpected)
View(checks$details)


