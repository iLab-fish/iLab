#' Check for file differences between two hard-drives
#'
#' @description
#' `compare_hds()` checks for differences in files between two hard drives.
#' It looks for differing numbers of files, different file naming, different file sizes,
#' different modification dates, and runs preliminary checks for corrupt files.
#'
#' @param raw_hd Directory for the Raw Hard drive (original files)
#' @param bu_hd Directory for the Backup Hard drive (copied files)
#
#' @return A set of console messages highlighting errors/warnings.
#'
#' @examples
#' \dontrun{
#'
#' # Run the check on all of iLab-Fish
#' # compare_hds(raw_hd = "E:/", bu_hd = "D:/)
#' 
#' }
#' 
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom digest digest
#' @importFrom purrr map_chr
#'
#' @export
compare_hds <- function(raw_hd, bu_hd) {
  
  raw_files <- suppressWarnings(data.frame(full_name_raw = list.files(
    path = raw_hd,
    recursive = TRUE,
    full.names = TRUE
  )) %>%
    dplyr::filter(!stringr::str_detect(full_name_raw, "RECYCLE.BIN")) %>% # Removes deleted files
    dplyr::mutate(size_raw = file.info(full_name_raw)$size,
                  short_name_raw = basename(full_name_raw),
                  modified_raw = file.info(full_name_raw)$mtime))
  
  bu_files <- suppressWarnings(data.frame(full_name_bu = list.files(
    path = bu_hd,
    recursive = TRUE,
    full.names = TRUE
  )) %>%
    dplyr::filter(!stringr::str_detect(full_name_bu, "RECYCLE.BIN")) %>% # Removes deleted files
    dplyr::mutate(size_raw = file.info(full_name_bu)$size,
                  short_name_bu = basename(full_name_bu),
                  modified_bu = file.info(full_name_bu)$mtime))
  
  # Check the difference in the number of files between the two HDs
  num_files <- data.frame(raw_num = nrow(raw_files),
                          bu_num  = nrow(bu_files))
  
  if (num_files$raw_num > num_files$bu_num) {
    x <- num_files$raw_num - num_files$bu_num
    message(paste0("Raw HD has ", x, " files not found on the BU HD."))
    
    error = TRUE
  }
  
  if (num_files$raw_num < num_files$bu_num) {
    x <- num_files$bu_num - num_files$raw_num
    message(paste0("BU HD has ", x, " files not found on the Raw HD."))
    
    error = TRUE
  }
  
  # Match files based off the short name (file name ignoring folder structure)
  matches_raw <- full_join(raw_files, bu_files, by = c("short_name_raw" = "short_name_bu"))
  
  # # Match files based off the short name (file name ignoring folder structure)
  # matches_bu <- full_join(bu_files, raw_files, by = c("short_name_bu" = "short_name_raw"))
  
  
  if(any(is.na(matches_raw$full_name_bu))) {
    missing_files <- dplyr::filter(matches_raw, is.na(full_name_bu))
    message(paste0("The following files are not found on the BU HD:\n - ", paste(missing_files$short_name_raw, collapse = "\n - ")))

    
    error  = TRUE
  }
  
  if(any(is.na(matches_raw$full_name_raw))) {
    missing_files <- dplyr::filter(matches_raw, is.na(full_name_raw))
    message(paste0("The following files are not found on the Raw HD:\n - ", paste(missing_files$short_name_raw, collapse = "\n - ")))
    
    
    error  = TRUE
  }
  

  # Check size of files
  if(any(matches_raw$size_raw != matches_raw$size_bu, na.rm = T)) {
    mismatch <- dplyr::filter(matches_raw, size_raw != size_bu)
    message(paste0("The size of the following files does not match between HDs:\n - ", paste(mismatch$short_name_raw, collapse = "\n - ")))
    
    error = TRUE
  }
  
  # Check date/time modified between the HDs
  if(any(matches_raw$modified_raw != matches_raw$modified_bu, na.rm = T)) {
    mismatch_time <- dplyr::filter(matches_raw, modified_raw != modified_bu)
    message(paste0("The date/time modified of the following files does not match between HDs:\n - ", paste(mismatch_time$short_name_raw, collapse = "\n - ")))
    
    error = TRUE
  }
  
  # Run partial file hashing
  # Checks for start and end of file corruption
  
  hash_file_head <- function(file, n_bytes = 1e6, algo = "md5") {
    con <- file(file, open = "rb")
    on.exit(close(con), add = TRUE)
    
    raw_data <- readBin(con, what = "raw", n = n_bytes)
    digest::digest(raw_data, algo = algo)
  }
  
  hash_file_tail <- function(file, n_bytes = 1e6, algo = "md5") {
    size <- file.info(file)$size
    n_bytes <- min(n_bytes, size)
    
    con <- file(file, open = "rb")
    on.exit(close(con), add = TRUE)
    
    seek(con, where = size - n_bytes, origin = "start")
    raw_data <- readBin(con, what = "raw", n = n_bytes)
    
    digest::digest(raw_data, algo = algo)
  }
  
  partial_hash <- function(file, n_bytes = 1e6, algo = "md5") {
    
    if (is.na(file) || file == "") {
      return(NA_character_)
    }
    
    paste(
      hash_file_head(file, n_bytes, algo),
      hash_file_tail(file, n_bytes, algo),
      sep = ":"
    )
  }
  
  hashes <- suppressWarnings(matches_raw %>%
    dplyr::filter(size_raw == size_bu) %>%
    dplyr::mutate(hash_raw = purrr::map_chr(full_name_raw, partial_hash),
                  hash_bu  = purrr::map_chr(full_name_bu,  partial_hash))
    )
  
  if(any(hashes$hash_raw != hashes$hash_bu)) {
    hash_mismatch <- dplyr::filter(hashes, hash_raw != hash_bu)
    
    message("File size matches but hashing suggests one of the versions of the following files may be corrupt:\n - ", 
            paste(hash_mismatch$short_name_raw, collapase = "\n - "))
    
    error = TRUE
  }
  
  if(!error == TRUE) {
    message("No errors have been identified between the two versions of your hard drives.")
  }
  
  if(error == TRUE) {
    message("Errors have been identified between the two versions of your hard drives, please check thoroughly and re-run this function.")
  }

  }
