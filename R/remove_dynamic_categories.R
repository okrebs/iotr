#' Remove Dynamic Consumption Categories from IO Table
#'
#' Remove dynamic consumption categories, e.g. inventory or
#' gross-fixed-capital-formation from an input-output (IO) table by
#' recalculating the IO table based on its leontief inverse and implied new
#' demand.
#'
#' @details Static CGE trade models often can/do not capture dynamic components
#'   such as changes in inventory or gross-fixed-capital-formation. Therefore
#'   this function allows to recalculate the IO table following the suggestions
#'   in Costinot and Rodríguez-Clare (2014). Specifically, negative changes in
#'   dynamic components of final consumption are assumed to have been produced
#'   in the current period, and positive components are added to a different
#'   category that can be specified using the \code{category_to_scale} argument
#'   (usually this will be final consumption by households). To account for the
#'   intermediate goods that would have been necessary to produce the negative
#'   components in the current period the approach relies on a leontief inverse
#'   to calculate intermediate use based upon the implied new demand.
#'
#' @param iot An input-output table in long format with the columns,
#'   \code{origin}, \code{sector}, \code{destination}, \code{use} and
#'   \code{flow}
#' @param dynamic_categories a vector of dynamic categories in final use to be
#'   removed
#' @param category_to_scale the use category (as integer) to which to add the
#'   positive components of removed categories.
#' @return Returns an IO table in long format as a \code{tibble} with the
#'   columns \code{origin}, \code{sector}, \code{destination}, \code{use} and
#'   \code{flow}
#' @example man/examples/wiod.R
#' @export remove_dynamic_categories
#' @importFrom magrittr %>%

remove_dynamic_categories <- function(iot,
                                      dynamic_categories,
                                      category_to_scale) {
  # due to NSE notes in R CMD check
  origin <- destination <- sector <- use <- flow <- demand <- NULL

  n_locations <- iot$origin %>% unique() %>% length()
  n_sectors <- max(iot$sector)
  first_use_category <- n_sectors + 1
  n_use_categories <- max(iot$use)

  # Coefficient Matrix ---------------------------------------------------------

  # get matrix of only intermediate goods trade
  intermediate_matrix <-  iot %>%
    dplyr::filter(use < first_use_category) %>%
    dplyr::arrange(destination, use, origin, sector) %>%
    dplyr::pull(flow) %>%
    matrix(nrow = n_locations * n_sectors)

  # get a vector of total output (revenue) by country and sector
  output <- iot %>%
    dplyr::group_by(origin, sector) %>%
    dplyr::summarise(flow = sum(flow), .groups = "drop") %>%
    # make sure things are in the same order as for the matrix x
    dplyr::arrange(origin, sector) %>%
    dplyr::pull(flow)

  # coefficient matrix, divide by a small number to prevent infinity
  coefficient_matrix <- intermediate_matrix /
    rep(replace(output, output == 0, 0.000001),
        each = nrow(intermediate_matrix))

  # Leontief-inverse to calculate new x given the constructed new demand -------
  leontief_inverse <- diag(nrow(coefficient_matrix)) - coefficient_matrix

  # aggregate new demand vector for each country-sector output
  new_demand <- iot %>%
    dplyr::filter(use > n_sectors) %>%
    dplyr::mutate(flow = ifelse(use %in% dynamic_categories & flow < 0, 0, flow),
           use = ifelse(use %in% dynamic_categories, category_to_scale, use),
           use = first_use_category - 1 + as.integer(as.factor(use))) %>%
    dplyr::group_by(origin, sector, destination, use) %>%
    dplyr::summarise(flow = sum(flow), .groups = "drop")

  new_total_demand <- new_demand %>%
    dplyr::group_by(origin, sector) %>%
    dplyr::summarise(demand = sum(flow), .groups = "drop") %>%
    # make sure things are in the same order as in x
    dplyr::arrange(origin, sector) %>%
    dplyr::pull(demand)

  # update
  new_output <- solve(leontief_inverse, new_total_demand)
  new_intermediate_matrix <-  coefficient_matrix %*% diag(new_output)

  # bring everything back into "long" format -----------------------------------

  new_iot <- iot %>%
    dplyr::filter(use < first_use_category) %>%
    dplyr::arrange(destination, use, origin, sector) %>%
    dplyr::mutate(flow = as.vector(new_intermediate_matrix)) %>%
    dplyr::bind_rows(new_demand)

  return(new_iot)
}
