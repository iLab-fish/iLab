#' Find columns containing any or all NAs
#'
#' @description
#' Return the names (or indices) of columns in a data frame/tibble where
#' either some values are missing or all values are missing. Both `NA` and 
#' `NaN`count as missing as detection uses [is.na()].
#' 
#' @param df A data.frame or tibble.
#' @param type One of "any" or "all".
#'   - `"any"`: columns with at least one missing value
#'   - `"all"`: columns where every value is missing
#' @param names Logical. If `TRUE` (default), return column names; if `FALSE`,
#'   return column indices.
#'
#' @return A character vector of column names or integer vector of column indices.
#' @examples
#' df <- data.frame(a = c(1, NA, 3),
#'                  b = c(NA, NA, NA),
#'                  c = c(1, 2, 3))
#' na_cols(df, type = "any")
#' na_cols(df, type = "all")
#' na_cols(df, type = "any", names = FALSE)
#'
#' # Edge cases
#' na_cols(data.frame(), type = "any")       # no columns -> character(0)
#' na_cols(df[0, ], type = "any")            # 0 rows -> character(0)
#' na_cols(df[0, ], type = "all")            # 0 rows -> character(0)
#'
#' @seealso [anyNA()], [is.na()], [colSums()]
#' @export


na_cols <- function(df, type = c("any", "all"), names = TRUE) {
  type <- match.arg(type)
  
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame (or tibble).", call. = FALSE)
  }
  
  # No columns
  if (ncol(df) == 0L) {
    return(if (names) character(0) else integer(0))
  }
  
  # No rows
  if (nrow(df) == 0L) {
    return(if (names) character(0) else integer(0))
  }
  
  na_counts <- colSums(is.na(df))
  
  idx <- if (type == "any") {
    which(na_counts > 0L)
  } else {
    which(na_counts == nrow(df))
  }
  
  if (names) names(df)[idx] else idx
}
