#' Load IO tables from the WIOD
#'
#' Function to load the set of world input output tables (wiot) made
#' available by the World Input Output Database's (WIOD) 2016 release.
#'
#' @param cache_dir path to read the WIOD data from. If the necessary files are
#'   not found in this location, they will be downloaded using \code{io_dl_wiot}
#' @param years which year(s) to load. Must be a single value or vector of
#'   values between 2000 and 2014
#' @param quiet if TRUE will try to avoid printing messages
#' @return Returns one \code{data.frame} combining wiots from all requested
#' years from the WIOD's 2016 release
#' @references Timmer, M. P., Dietzenbacher, E., Los, B., Stehrer, R. and de
#' Vries, G. J. (2015), "An Illustrated User Guide to the World Input–Output
#' Database: the Case of Global Automotive Production", Review of International
#'  Economics., 23: 575–605, www.wiod.org
#' @example man/examples/wiod.R
#' @export io_load_wiot

io_load_wiot <- function(cache_dir = NULL,
                         years = 2000:2014,
                         quiet = FALSE) {

  # avid note in RMD check for predefined name of WIOD data
  wiot <- NULL

  cache_dir <- check_cache_dir(cache_dir, quiet)

  if(!all(is.wholenumber(years)) | any(years < 2000) | any(years > 2014)) {
    stop("'years' out of range. Must be integers between 2000 and 2014.")
  }

  wiot_files <- paste0("WIOT", years, "_October16_ROW.RData")
  if(all(file.exists(file.path(cache_dir, wiot_files)))) {
    if (!quiet) {
      message("All requested WIOT files found in cache. Reading cached data.")
    }
  } else {
    if(!quiet) {
      message("No or incomplete cache found. Downloading data.")
    }
    cache_dir <- io_dl_wiot(cache_dir = cache_dir, quiet = quiet)
  }

  if(!quiet) message("Loading and combining WIOT years.")
  wiot_all_years <- data.frame()
  for(current_file in wiot_files) {
    wiot <- load_into(file.path(cache_dir, current_file))
    wiot_all_years <- rbind(wiot_all_years, wiot)
  }

  # change some basic type choices made by WIOD
  wiot_all_years$Year <- as.integer(wiot_all_years$Year)
  wiot_all_years$RNr <- as.integer(wiot_all_years$RNr)

  return(wiot_all_years)
}
