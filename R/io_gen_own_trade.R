#' Generate small own trade values as needed
#'
#' Finds combinations of destination-sector-use in a standardized IO table that
#' are 0 where \code{destination == origin} but different from 0 for at least
#' one \code{destination != origin} and replaces them with either the smallest
#' value found for one of the \code{origin != destination} cases or
#' \code{max_replace}, whichever is smaller.
#'
#' @param iot a tibble with a standardized input output table in long format
#' with the columns \code{origin}, \code{sector}, \code{destination},
#' \code{use}, \code{flow}. See the package \code{iotr} for more details on this
#' format.
#' @param max_replace maximum value to replace zero own trade flows with.
#' @return Returns the data frame \code{iot} with respective zero own trade
#' flows removed.
#' @references  This manipulation of the original IO table is useful for
#' specific simulations that feature infinite trade barrieres. See for example
#' Eppinger, Peter and Felbermayr, Gabriel J. and Krebs, Oliver and
#' Kukharskyy, Bohdan, COVID-19 Shocking Global Value Chains (2020). CESifo
#' Working Paper No. 8572, https://ssrn.com/abstract=3702124
#' @export io_gen_own_trade
#' @importFrom magrittr %>%

io_gen_own_trade <- function(iot, max_replace) {

  # avid note in RMD check for predefined names in NSE
  origin <- destination <- sector <- use <- flow <- NULL

  iot %>%
    dplyr::group_by(destination, sector, use) %>%
    dplyr::mutate(flow = ifelse(flow[origin == destination] == 0 &
                                  any(flow > 0) & origin == destination,
                                min(max_replace, min(flow[flow != 0])),
                                flow)) %>%
    dplyr::ungroup()
}
