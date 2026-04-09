#' Detect and Convert Non-ASCII Characters in a Data Frame
#'
#' Scans a data frame for non-ASCII (i.e., special or extended) characters in
#' character or factor columns and attempts to convert them to valid ASCII or
#' UTF-8 representations.
#'
#' The function uses \code{stringi::stri_enc_isascii()} for robust detection of
#' non-ASCII characters and applies a conversion pipeline based on
#' \code{iconv()} followed by targeted substitution of commonly problematic
#' Windows-1252 control characters.
#'
#' If non-ASCII characters are found in a column named \code{"field_note"},
#' conversion is attempted automatically with a warning. If non-ASCII
#' characters are found in other columns, the user is prompted (via RStudio
#' dialog or console input) to either proceed with conversion or abort.
#'
#' After conversion, the data frame is re-checked and execution stops with an
#' error if any non-ASCII characters remain.
#'
#' @param df A data frame.
#'
#' @param from Character string giving the source encoding passed to
#'   \code{iconv()}. Defaults to \code{"latin1"}.
#'
#' @param to Character string giving the target encoding passed to
#'   \code{iconv()}. Defaults to \code{"UTF-8"}.
#'
#' @param sub Character string passed to the \code{sub} argument of
#'   \code{iconv()}, used when characters cannot be converted.
#'   Defaults to \code{"ascii"}.
#'
#' @param interactive Logical indicating whether the user should be prompted
#'   before converting non-ASCII characters in columns other than
#'   \code{"field_note"}. Defaults to \code{TRUE}.
#'
#' @details
#'
#' Non-ASCII characters are detected using \code{stringi::stri_enc_isascii()},
#'  which avoids UTF-8 conversion warnings and safely handles malformed 
#'  encodings. Factors are coerced to character before inspection.
#'  
#'  Conversion is performed by (1) encoding conversion using 
#'  \code{iconv(from, to, sub)} and (2) targeted replacement of common 
#'  Windows-1252 control characters.
#'
#' @return
#' A data frame with non-ASCII characters converted, or the original data
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'     field_note = c("didn\x92t record", NA),
#'     other = c("ok", "fine"),
#'     stringsAsFactors = FALSE)
#'     
#' out <- convert_non_ascii(df, interactive = TRUE)
#' }
#' 
#' @importFrom stringi stri_enc_isascii
#' @export

# usethis::use_package("stringi")

convert_non_ascii <- function(
    df,
    from = "latin1",
    to = "UTF-8",
    sub = "ascii", # maybe should be ""
    interactive = TRUE
) {
  
  if (!is.data.frame(df)) {
    stop("df must be a data frame", call. = FALSE)
  }

  # Helpers ----
  # detect non-ASCII safely
  has_non_ascii <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    is.character(x) && any(!stringi::stri_enc_isascii(x), na.rm = TRUE)
  }
  
  # conversion pipeline
  convert_text <- function(x) {
    x <- iconv(x, from = from, to = to, sub = sub)
    x <- chartr("\u0092\u0096", "\u0027\u002D", x)
    x
  }
  
  # Detection ----
  # Find columns with non-ASCII
  non_ascii_cols <- names(df)[vapply(df, has_non_ascii, logical(1))]
  
  # If nothing found return data frame
  if (length(non_ascii_cols) == 0) {
    return(df)
  }
  
  # Conversion (field notes) ----
  field_note_col <- "field_note"
  
  if (field_note_col %in% non_ascii_cols) {
    
    # message
    warning(
      sprintf(
        "Non-ASCII characters (i.e., special characters) found in '%s'. Attempting automatic conversion.",
        field_note_col
      ),
      call. = FALSE
    )
    
    # convert
    df[[field_note_col]] <- convert_text(df[[field_note_col]])
    
    # remove from non_ascii_cols
    non_ascii_cols <- setdiff(non_ascii_cols, field_note_col)
    
  } 
  
  # Conversion (other cols) -----
  if (length(non_ascii_cols) > 0) {
    
    # message
    msg <- paste(
      "Non-ASCII characters (i.e., special characters) found in the following column(s):",
      paste(non_ascii_cols, collapse = ", "),
      "\n\nAttempt conversion or abort (recommended)",
      sep = "\n"
    )
    
    # Interactive proceed/abort
    proceed <- TRUE
    
    if (rstudioapi::isAvailable()) {
      
      proceed <- rstudioapi::showQuestion(
        title = "Non-ASCII characters detected",
        message = msg,
        ok = "Continue",
        cancel = "Abort"
      )
    } else {
      ans <- readline(paste0(msg, "\nType 'y' to continue: "))
      proceed <- tolower(trimws(ans)) %in% c("y", "yes")
    }
    
    if (!isTRUE(proceed)) {
      stop("Process aborted by user due to non-ASCII characters.", call. = FALSE)
    }
    
    # convert
    for (col in non_ascii_cols) {
      df[[col]] <- convert_text(df[[col]])
    }
  }
  
  check_updated_for_ascii <- names(df)[vapply(df, has_non_ascii, logical(1))]
  
  if (length(check_updated_for_ascii)>0) {
    stop("non-ASCII characters detected after attempted conversion. Please remove non-ASCII characters from input dataframe/file.")
  }
  
  df
}
