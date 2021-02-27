#' @noRd
#'
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

#' Download IO tables from the WIOD
#'
#' Function to download the set of world input output tables (wiot) made
#' available by the World Input Output Database's (WIOD) 2016 release.
#'
#' @param cache_dir path to cache the WIOD data. If the full content of the WIOD
#'   data zip file is found under this location it will not be downloaded again.
#' @param years which year(s) to load. Must be a single value or vector of
#'   values between 2000 and 2014
#' @param url url to download the WIOD from. This should usually not be changed.
#' @return Returns one \code{data.frame} combining all wiots from the WIOD's
#' 2016 release
#' @references Timmer, M. P., Dietzenbacher, E., Los, B., Stehrer, R. and de
#' Vries, G. J. (2015), "An Illustrated User Guide to the World Input–Output
#' Database: the Case of Global Automotive Production", Review of International
#'  Economics., 23: 575–605, www.wiod.org
#' @example man/examples/wiod.R
#' @export get_wiot
#' @importFrom dplyr mutate

get_wiot <- function(cache_dir = NULL, years = 2000:2014,
                     url = paste0("http://www.wiod.org/protected3/",
                                  "data16/wiot_ROW/wiot_r_Nov16.zip")) {

  if(is.null(cache_dir)) {
    message("No cache directory given.",
            "Downloaded data will be discarded after reading.")
    cache_dir <- tempdir()
  } else if(!dir.exists(cache_dir)) {
    stop("Cache directory does not exist.")
  }

  file_sep <- .Platform$file.sep
  if(substr(cache_dir, nchar(cache_dir), nchar(cache_dir)) == file_sep) {
    cache_dir <- substr(cache_dir, 1, nchar(cache_dir) - 1)
  }

  if(!all(is.wholenumber(years)) | any(years < 2000) | any(years > 2015)) {
    stop("'years' out of range. Must be integers between 2000 and 2015.")
  }
  wiot_files <- paste0("WIOT", years, "_October16_ROW.RData")
  if(all(file.exists(file.path(cache_dir, wiot_files)))) {
    message("All requested WIOT files found in cache. Reading cached data.")
  } else {
    message("No or incomplete cache found. Downloading data.")

    tmp_file <- tempfile()
    download.file(url, tmp_file)

    message("Unzipping data.")
    unzip(tmp_file, exdir = cache_dir)
  }

  message("Loading and combining WIOT years.")
  wiot_all_years <- data.frame()
  for(current_file in wiot_files) {
    load(paste0(cache_dir, file_sep, current_file))
    wiot_all_years <- rbind(wiot_all_years, wiot)
  }

  # change some basic type choices made by WIOD
  wiot_all_years <- dplyr::mutate(wiot_all_years,
                                  Year = as.integer(Year),
                                  RNr = as.integer(RNr))

  return(wiot_all_years)
}
