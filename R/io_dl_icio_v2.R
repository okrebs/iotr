#' Download IO tables from the OECD ICIO v2
#'
#' Function to download the set of inter country input output (icio) tables made
#' available by the OECD's 2023 release. Should usually not be called directly
#' but through \code{io_load_icio()}.
#'
#' @param year_blk a single integer between 1 and 5, denoting which year block
#'   (1995-2000, 2001-2005, 2006-2010, 2011-2015, 2016-2020) to download.
#' @param cache_dir path to cache the ICIO data. If the full content of the ICIO
#'   data zip file is found under this location it will not be downloaded again.
#' @param url list of urls to download the ICIO zip files from. This should
#'   usually not be changed.
#' @param quiet if TRUE will try to avoid printing messages
#' @return Path to the directory where data was saved, if given, this is
#'   identical to \code{cache_dir}
#' @references OOECD. (2023) OECD Inter-Country Input-Output Tables,
#'   http://oe.cd/icio
#' @export io_dl_icio_v2

io_dl_icio_v2 <- function(year_blk,
                          cache_dir = NULL,
                          url = list(
                            "base_url" = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=",
                            "1995-2000" = "d26ad811-5b58-4f0c-a4e3-06a1469e475c",
                            "2001-2005" = "7cb93dae-e491-4cfd-ac67-889eb7016a4a",
                            "2006-2010" = "ea165bfb-3a85-4e0a-afee-6ba8e6c16052",
                            "2011-2015" = "1f791bc6-befb-45c5-8b34-668d08a1702a",
                            "2016-2020" = "d1ab2315-298c-4e93-9a81-c6f2273139fe"
                          ),
                          quiet = FALSE) {

  if(!all(is.wholenumber(year_blk)) | any(year_blk < 1) | any(year_blk > 5)) {
    stop("'year_blk' out of range. Must be integer between 1 and 5.")
  }

  cache_dir <- check_cache_dir(cache_dir, quiet)

  # OECD website can be slow, avoid timeout of download
  timeout_bak <- getOption("timeout")
  options("timeout" = 60 * 30)

  if(!quiet) message("Downloading data. This may take several minutes.")

  # avoid 90 character CRAN line length limit in \usage
  full_url <- paste0(url[["base_url"]], url[[year_blk + 1]])
  tmp_file <- tempfile()
  utils::download.file(full_url, tmp_file, quiet = quiet)
  options("timeout" = timeout_bak)

  if(!quiet) message("Unzipping data.")
  utils::unzip(tmp_file, exdir = cache_dir)

  return(cache_dir)
}
