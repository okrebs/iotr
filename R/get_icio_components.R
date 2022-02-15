#' @noRd
#'
get_icio_components <- function(set) {

  icio_Rdata <- c("CONS", "CVB", "FD", "GFCF", "GGFC", "GTR", "GTR_FNL",
                  "GTR_INT", "HFCE", "INVNT", "NONRES", "NPISH", "VA",
                  "VAexTAX", "X")
  icio_supplementary <- c("A", "B", "VB")
  icio_Z <- "Z"

  if(length(set) != 1) stop("Length of 'set' must be 1!")

  if(set == "all") {
    components <- c(icio_Rdata, icio_supplementary, icio_Z)
  } else if (set == "Rdata") {
    components <- c(icio_Rdata)
  } else if (set == "download") {
    components <- c("Rdata", icio_supplementary, icio_Z)
  } else {
    stop("Invalid 'set' passed!")
  }

  return(components)
}
