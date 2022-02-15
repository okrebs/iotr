#' @noRd
#'
#'
check_icio_components <- function(components,
                                  check_type = "all") {

  if(!(check_type %in% c("all", "download"))) stop("Invalid 'check_type'")

  Rdata_components <- get_icio_components(set = "Rdata")
  all_components <- get_icio_components(set = check_type)
  if(check_type == "all") {
    all_components <- c("Rdata", all_components)
  }

  components <- unique(components)
  if (!all(components %in% all_components)) {
    invalid_components <- components[!(components %in% all_components)]
    stop("'", paste(invalid_components, collapse = "', '"),
         "' invalid choice(s) for 'components'")
  }

  if (check_type == "all") {
    if (any(components == "Rdata")) {
      components <-
        unique(c(components[components != "Rdata"], Rdata_components))
    }
  }

  return(components)
}
