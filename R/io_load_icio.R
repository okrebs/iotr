#' Download IO tables from the OECD ICIO
#'
#' Function to download the set of inter country input output (icio) tables made
#' available by the OECD's 2021 release.
#'
#' @param cache_dir path to cache the ICIO data. If the full content of the ICIO
#'   data zip file is found under this location it will not be downloaded again.
#' @param years which year(s) to load. Must be a single value or vector of
#'   values between 1995 and 2018. Can be used to reduce memory needs and speed
#'   up loading.
#' @param components a character vector with one or several components of the
#'   ICIO tables to load. Options are "Rdata" which load all components from
#'   ICIO's 'Rdata' set, "Z" which loads the intermediate matrix Z, as well as
#'   "A", "B", and "VB" for the respective supplementary matrices. Additionally,
#'   accepted options are the individual parts of the 'Rdata' set ("CONS",
#'   "CVB", "FD", "GFCF", "GGFC", "GTR", "GTR_FNL", "GTR_INT", "HFCE", "INVNT",
#'   "NONRES", "NPISH", "VA", "VAexTAX", "X"). However, since all components
#'   from ICIO's 'Rdata' set are only available for download in a joint zip
#'   file. Selecting one of these components will download the entire set and
#'   place it in \code{cache_dir}, but only load the selected parts into R. See
#'   OECD's ICIO web page for details on these components.
#' @param quiet if TRUE will try to avoid printing messages
#' @return Returns one \code{data.frame} combining all years and requested
#'   components from the ICIOs from the OECD's 2021 release.
#' @references OECD (2021), OECD Inter-Country Input-Output Database,
#'   http://oe.cd/icio
#' @example man/examples/icio.R
#' @export io_load_icio

io_load_icio <- function(cache_dir = NULL,
                         years = 1995:2018,
                         components = c("Rdata", "Z"),
                         quiet = FALSE) {

  # avoid note in RMD check for predefined names of ICIO data
  ICIO2021econA <-ICIO2021econB <- ICIO2021econVB <-ICIO2021econZ <- NULL

  cache_dir <- check_cache_dir(cache_dir, quiet)

  if(!all(is.wholenumber(years)) | any(years < 1995) | any(years > 2018)) {
    stop("'years' out of range. Must be integers between 1995 and 2018.")
  }

  components <- check_icio_components(components, check_type = "all")
  # if one element from set "Rdata" is included the whole set must be downloaded
  dl_components <-
    unique(ifelse(
      components %in% get_icio_components("Rdata"),
      "Rdata",
      components
    ))

  dl_components <- NULL
  for(i in components) {
    file_name <- paste0("ICIO2021econ", i, ".Rdata")
    if (file.exists(file.path(cache_dir, file_name))) {
      if(!quiet) message("Found ", file_name, " in cache.")
    } else {
      if (!quiet) message(file_name, " not found in cache, marked for download.")
      dl_components <-
        c(dl_components, ifelse(i %in% get_icio_components("Rdata"), "Rdata", i))
    }
  }
  dl_components <- unique(dl_components)

  if(is.null(dl_components) & !quiet) {
    message("All requested data found in cache.")
  } else {
    for(i in dl_components) {
      if(!quiet) message("Downloading missing component ", i)
      cache_dir <-
        io_dl_icio(component = i, cache_dir = cache_dir, quiet = quiet)
    }
  }

  if(!quiet) message("Loading ICIO components.")
  icio <- list()
  for(i in components) {
    file_name <- paste0("ICIO2021econ", i, ".Rdata")
    tmp <- load_into(file.path(cache_dir, file_name))
    # in OECD ICIO first dimension is always years
    tmp <- abind::asub(tmp, as.character(years), 1, drop = FALSE)
    icio[[paste0(i)]] <- tmp
  }

  return(icio)
}
