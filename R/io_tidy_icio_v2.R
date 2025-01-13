#' Tidy data from OECD's ICIO v2
#'
#' Function to turn a list of OECD ICIO components, usually obtained from a call
#' to \code{io_load_icio_v2()}, into a tibble, i.e. a 'tidy' data frame.
#'
#' @param icio list containing ICIO years from the 2023 release
#' @param quiet if TRUE will try to avoid printing messages
#' @return Returns one \code{tibble} combining all components from \code{icio}.
#' @references OECD. (2023) OECD Inter-Country Input-Output Tables,
#'   http://oe.cd/icio
#' @example man/examples/icio_v2.R
#' @importFrom magrittr %>%
#' @export io_tidy_icio_v2

io_tidy_icio_v2 <- function(icio, quiet = FALSE) {
  # due to NSE notes in R CMD check
  V1 <- destination <- origin <- NULL

  icio <- lapply(seq_along(icio), function(i) {
    if(!quiet) message("Tidying year ", names(icio)[i], ".")
    icio[[i]] %>%
    dplyr::rename(origin = V1) %>%
    tidyr::pivot_longer(
      cols = -"origin",
      names_to = "destination",
      values_to = "flow"
    ) %>%
    dplyr::filter(origin != "OUT", destination != "OUT") %>%
    tidyr::separate_wider_delim(
      cols = "origin",
      delim = "_",
      too_many = "merge",
      too_few = "align_end",
      names = c("origin", "sector")
    ) %>%
    tidyr::separate_wider_delim(
      cols = "destination",
      delim = "_",
      too_many = "merge",
      too_few = "align_end",
      names = c("destination", "use")
    ) %>%
    dplyr::mutate(
      origin = ifelse(is.na(origin), destination, origin)
    )}) %>%
    setNames(names(icio)) %>%
    dplyr::bind_rows(.id = "year")

  return(icio)
}
