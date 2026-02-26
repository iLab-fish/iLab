#' Vectors defining all fish, sharks/rays, and other family names
#'
#' A list created from CSIRO's [CAAB database](https://www.cmar.csiro.au/data/caab) 
#' containing the family names associated with all fish (including sharks/rays), 
#' sharks/rays only, and other non-fish.
#'
#' @format ## `families`
#' A list with three items:
#' \describe{
#'   \item{fish}{All fish families}
#'   \item{sharks}{Shark and ray families}
#'   \item{year}{Non-fish families (i.e., other species of interest)}
#'   ...
#' }
#' @source <https://www.cmar.csiro.au/data/caab/create_caab_extract.cfm>
"families"