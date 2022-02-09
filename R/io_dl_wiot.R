#' Download IO tables from the WIOD
#'
#' Function to download the set of world input output tables (wiot) made
#' available by the World Input Output Database's (WIOD) 2016 release.
#'
#' @param cache_dir path to cache the WIOD data. If the full content of the WIOD
#'   data zip file is found under this location it will not be downloaded again.
#' @param url url to download the WIOD from. This should usually not be changed.
#' @param quiet if TRUE will try to avoid printing messages
#' @return Path to the directory where data was saved, if given, this is
#' identical to \code{cache_dir}
#' @references Timmer, M. P., Dietzenbacher, E., Los, B., Stehrer, R. and de
#' Vries, G. J. (2015), "An Illustrated User Guide to the World Input–Output
#' Database: the Case of Global Automotive Production", Review of International
#'  Economics., 23: 575–605, www.wiod.org
#' @example man/examples/wiod.R
#' @export io_dl_wiot
#' @importFrom magrittr %>%

io_dl_wiot <- function(cache_dir = NULL,
                       url = paste0("https://dataverse.nl/api/access/datafile/199101"),
                       quiet = FALSE) {

  if (is.null(cache_dir)) {
    if (!quiet) {
      message(
        "No cache directory given. Data will be downloaded to a temporary",
        "directory and deleted with the end of the Rsession."
      )
    }
    cache_dir <- tempdir()
  } else if (!dir.exists(cache_dir)) {
    stop("Cache directory does not exist. Create it first.")
  }

  file_sep <- .Platform$file.sep
  if (substr(cache_dir, nchar(cache_dir), nchar(cache_dir)) == file_sep) {
    cache_dir <- substr(cache_dir, 1, nchar(cache_dir) - 1)
  }

  tmp_file <- tempfile()
  if(!quiet) message("Downloading data.")
  utils::download.file(url, tmp_file, quiet = quiet)

  if(!quiet) message("Unzipping data.")
  utils::unzip(tmp_file, exdir = cache_dir)

  return(cache_dir)
}
