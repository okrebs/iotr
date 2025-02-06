#' @noRd
#'
#'
check_current_version_eora26 <- function() {
  eora26_website <- httr2::request("https://worldmrio.com/eora26/") |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  current_version <- regmatches(
    eora26_website, regexpr("v[0-9]{1,3}\\.[0-9]{1,3}", eora26_website)
  )
  return(current_version)
}
