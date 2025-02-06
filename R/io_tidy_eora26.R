#' Tidy data from the EORA26 MRIO
#'
#' Function to turn a list (one element for each year) of EORA MRIO components,
#' usually obtained from a call to \code{io_load_eora26()}, into a tibble, i.e.
#' a 'tidy' data frame.
#'
#' @param eora26 list containing EORA26 years with the components T, VA, FD and
#'   and element with the corresponding labels
#' @param quiet if TRUE will try to avoid printing messages
#' @return Returns one \code{tibble} combining all components and years from
#'  \code{eora26}.
#' @references Lenzen M, Kanemoto K; Moran D, and Geschke A (2012) Mapping the
#'   structure of the world economy. Environmental Science & Technology 46(15)
#'   pp 8374–8381.
#'
#'   Lenzen, M., Moran, D., Kanemoto, K., Geschke, A. (2013) Building Eora: A
#'   Global Multi-regional Input-Output Database at High Country and Sector
#'   Resolution. Economic Systems Research, 25:1, 20-49.
#' @example man/examples/eora26.R
#' @export io_tidy_eora26

io_tidy_eora26 <- function(eora26, quiet = FALSE) {
  # due to NSE notes in R CMD check
  origin <- destination <- sector <- year <- flow <- NULL

  labels <- eora26$labels
  eora26$labels <- NULL

  eora26 <- lapply(seq_along(eora26), function(year_n) {
    if(!quiet) message("Tidying year ", names(eora26)[year_n], ".")
    eora26_T <- tidyr::expand_grid(
        dplyr::rename(labels[["T"]], destination = origin, use = sector),
        labels[["T"]]
      ) |>
      dplyr::mutate(flow = as.vector(unlist(eora26[[year_n]]$`T`)))
    eora26_FD <- tidyr::expand_grid(labels[["T"]], labels[["FD"]]) |>
      dplyr::mutate(flow = as.vector(unlist(eora26[[year_n]]$FD)))
    eora26_VA <- tidyr::expand_grid(
        dplyr::rename(labels[["T"]], destination = origin, use = sector),
        labels[["VA"]]
      ) |>
      dplyr::mutate(
        flow = as.vector(unlist(eora26[[year_n]]$VA)),
        origin = destination
      )
    dplyr::bind_rows(
      eora26_T,
      eora26_VA,
      eora26_FD
    ) |>
      dplyr::mutate(year = names(eora26)[year_n]) |>
      dplyr::select(year, origin, sector, destination, use, flow)
  })
  return(eora26)
}
