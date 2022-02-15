#' Tidy data from the World Input Output Database
#'
#' Converts raw WIOT table(s) from the 2016 release of the World Input Output
#' Database, usually obtained by a call to io_load_wiot, into a long format
#' tibble, i.e. a 'tidy' data frame.
#'
#' @param wiot A \code{data.frame} containing a raw WIOT for one or many years.
#' @return Returns one \code{tibble} with the WIOT tables converted to a long
#'   format
#' @seealso \code{\link{io_load_wiot}} for (down)loading WIOT tables
#' @example man/examples/wiod.R
#' @export io_tidy_wiot
#' @importFrom magrittr %>%
#'
io_tidy_wiot <- function(wiot) {
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

  return(wiot)
}
