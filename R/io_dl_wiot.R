#' Download IO tables from the WIOD
#'
#' Function to download the set of world input output tables (wiot) made
#' available by the World Input Output Database's (WIOD) 2016 release. Should
#' usually not be called directly but through \code{io_load_wiot()}.
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
#' @export io_dl_wiot

io_dl_wiot <- function(cache_dir = NULL,
                       url = paste0("https://dataverse.nl/api/access/datafile/199101"),
                       quiet = FALSE) {

  cache_dir <- check_cache_dir(cache_dir, quiet)

  tmp_file <- tempfile()
  if(!quiet) message("Downloading data.")
  utils::download.file(url, tmp_file, quiet = quiet)

  if(!quiet) message("Unzipping data.")
  utils::unzip(tmp_file, exdir = cache_dir)

  return(cache_dir)
}
