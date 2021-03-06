#' Remove Negative VAD from IO Table
#'
#' Remove negative imputed value added (VAD) from an input-output (IO) table by
#' scaling the total output and world wide use of the specific location-sector.
#'
#' @details In location-sectors where a given IO table reports intermediate use
#' larger than output value, i.e. where imputed VAD would be negative, this
#' function increases output until the imputed VAD matches the smallest share of
#' VAD in output found in any other location-sector in the IO table. To account
#' for the destination of the additional production the use category
#' \code{category_to_scale} is increased around the world to match the increased
#' production. If no negative value added is imputed the IO table is returned
#' unchanged.
#'
#' @param iot An input-output table in long format with the columns,
#'   \code{origin}, \code{sector}, \code{destination}, \code{use} and
#'   \code{flow}
#' @param category_to_scale the use category (as integer) which to scale if
#'   output has to be increased due to an imputed negative VAD
#' @return Returns an IO table in long format as a \code{tibble} with the
#'   columns \code{origin}, \code{sector}, \code{destination}, \code{use} and
#'   \code{flow}
#' @example man/examples/wiod.R
#' @export rm_negative_vad
#' @importFrom magrittr %>%

rm_negative_vad <- function(iot, category_to_scale) {

  # due to NSE notes in R CMD check
  origin <- destination <- sector <- use <- flow <- output <- vad_shr <- NULL
  intermediate_use <- imputed_vad <- output_diff <- new_output <- NULL

  location_sector_stats <- iot %>%
    dplyr::group_by(origin, sector) %>%
    dplyr::mutate(output = sum(flow)) %>%
    dplyr::group_by(destination, use) %>%
    dplyr::mutate(intermediate_use = sum(flow)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(origin == destination, sector == use) %>%
    dplyr::mutate(imputed_vad = output - intermediate_use) %>%
    dplyr::select(-destination, -use, -flow)

  vad_fix <- location_sector_stats %>%
    dplyr::mutate(vad_shr = imputed_vad / output,
           new_output = ifelse(imputed_vad > 0,
                               output,
                               intermediate_use /
                                 (1 - min(vad_shr[imputed_vad > 0]))),
           output_diff = new_output - output) %>%
    dplyr::filter(vad_shr < 0)

  if(nrow(vad_fix) == 0) {
    message("No negative VAD imputed. Returning the table unchanged.")
    return(iot)
  } else {
    message("Negative VAD imputed in \n",
            paste0(utils::capture.output(
                     dplyr::select(vad_fix, origin, sector))[-c(1, 3)],
                   collapse = "\n"),
            "\n Recalculating table.")

    changed_data <- iot %>%
      dplyr::inner_join(vad_fix, by = c("origin", "sector")) %>%
      dplyr::filter(use == category_to_scale) %>%
      dplyr::group_by(origin, sector) %>%
      dplyr::mutate(flow = flow * (sum(flow) + output_diff) / sum(flow)) %>%
      dplyr::ungroup() %>%
      dplyr::select(origin, sector, destination, use, flow)

    iot <- iot %>%
      dplyr::anti_join(changed_data,
                by = c("origin", "sector", "destination", "use")) %>%
      dplyr::bind_rows(changed_data)

    return(iot)
  }
}
