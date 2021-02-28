#' Convert WIOT table to long format
#'
#' Converts WIOT table(s) from the 2016 release of the World Input Output
#' Database into a long format.
#'
#' @param wiot A \code{data.frame} containing a WIOT for one or many years.
#' @return Returns one \code{tibble} with the WIOT tables converted to a long
#'   format
#' @seealso \code{\link{get_wiot}} for downloading WIOT tables
#' @example man/examples/wiod.R
#' @export wiot2long
#' @importFrom magrittr %>%
#'
wiot2long <- function(wiot) {
  # due to NSE notes in R CMD check
  IndustryCode <- IndustryDescription <- Year <- RNr <- Country <- TOT <- NULL
  destination_use <- use <- NULL

  wiot <- wiot %>%
    dplyr::select(-TOT) %>%
    tidyr::pivot_longer(cols = c(-IndustryCode, -IndustryDescription, -Year,
                                 -RNr, -Country),
                 names_to = "destination_use",
                 values_to = "flow") %>%
    tidyr::separate(destination_use, into = c("destination", "use"), 3) %>%
    dplyr::mutate(use = as.integer(use))
}
