#' Download IO tables from the OECD ICIO
#'
#' Function to download the set of inter country input output (icio) tables made
#' available by the OECD's 2021 release.
#'
#' @param component a character with a single component of the ICIO tables to
#'   load. Options are 'Rdata', 'Z', 'A', 'B', 'VB' which refer to the
#'   respectively named files from OECD's ICIO website, where 'Z' is the
#'   intermediate flow matrix, 'Rdata' a set containing all remaining IO table
#'   components and 'A', 'B' and 'VB' are supplementary matrices. See the OECD's
#'   ICIO web page http://oe.cd/icio for details.
#' @param cache_dir path to cache the ICIO data. If the full content of the ICIO
#'   data zip file is found under this location it will not be downloaded again.
#' @param url list of urls to download the ICIO zip files from. This should
#'   usually not be changed.
#' @param quiet if TRUE will try to avoid printing messages
#' @return Path to the directory where data was saved, if given, this is
#'   identical to \code{cache_dir}
#' @references OECD (2021), OECD Inter-Country Input-Output Database,
#'   http://oe.cd/icio
#' @example man/examples/icio.R
#' @export io_dl_icio

io_dl_icio <- function(component,
                       cache_dir = NULL,
                       url = list(
                         "base_url" = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=",
                         "Rdata" = "6d8e884e-7983-4899-816d-6ca0a7d46ca4",
                         "A" = "757b2a01-f44e-43c5-a984-74ce4051a629",
                         "B" = "5da20b06-be2d-4bf1-be42-7fa130a9d23b",
                         "VB" = "e5f2e20f-5cb0-450a-b6fb-146a020fa625",
                         "Z" = "c44881c8-54e0-42b4-b8c0-444099592f2c"
                       ),
                       quiet = FALSE) {

  cache_dir <- check_cache_dir(cache_dir, quiet)
  component <- check_icio_components(component, check_type = "download")

  tmp_file <- tempfile()

  # OECD website can be slow and files are large, avoid timeout of download
  timeout_bak <- getOption("timeout")
  options("timeout" = 60 * 30)

  if(!quiet) message("Downloading data. This may take several minutes.")

  # avoid 90 character CRAN line length limit in \usage
  full_url <- paste0(url[["base_url"]], url[[component]])
  utils::download.file(full_url, tmp_file, quiet = quiet)
  options("timeout" = timeout_bak)

  if(!quiet) message("Unzipping data.")
  utils::unzip(tmp_file, exdir = cache_dir)

  return(cache_dir)
}
