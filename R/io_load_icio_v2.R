#' Download IO tables from the OECD ICIO v2
#'
#' Function to download the set of inter country input output (icio) tables made
#' available by the OECD's 2023 release.
#'
#' @param cache_dir path to cache the ICIO data. If the full content of the ICIO
#'   data zip file is found under this location it will not be downloaded again.
#' @param years which year(s) to load. Must be a single value or vector of
#'   values between 1995 and 2020. Can be used to reduce memory needs and speed
#'   up loading.
#' @param quiet if TRUE will try to avoid printing messages
#' @return Returns one \code{data.frame} combining all years and requested
#'   components from the ICIOs from the OECD's 2023 release.
#' @references OECD. (2023) OECD Inter-Country Input-Output Tables,
#'   http://oe.cd/icio
#' @example man/examples/icio_v2.R
#' @export io_load_icio_v2

io_load_icio_v2 <- function(cache_dir = NULL,
                            years = 1995:2020,
                            quiet = FALSE) {

  cache_dir <- check_cache_dir(cache_dir, quiet)

  if(!all(is.wholenumber(years)) | any(years < 1995) | any(years > 2020)) {
    stop("'years' out of range. Must be integers between 1995 and 2020.")
  }

  dl_year_blks <- NULL
  for(year in years) {
    file_name <- paste0(year, "_SML.csv")
    if (file.exists(file.path(cache_dir, file_name))) {
      if(!quiet) message("Found ", file_name, " in cache.")
    } else {
      if (!quiet) message(file_name, " not found in cache, marked for download.")
      year_blk <- which(year >= c(1995, 2001, 2006, 2011, 2016) &
                     year <= c(2000, 2005, 2010, 2015, 2020))
      dl_year_blks <- c(dl_year_blks, year_blk)
    }
  }
  dl_year_blks <- unique(dl_year_blks)

  if(is.null(dl_year_blks) & !quiet) {
    message("All requested data found in cache.")
  } else {
    for(i in dl_year_blks) {
      if(!quiet) message("Downloading missing years from year group ", i)
      cache_dir <-
        io_dl_icio_v2(year_blk = i, cache_dir = cache_dir, quiet = quiet)
    }
  }

  icio <- list()
  for(i in years) {
    if(!quiet) message("Reading ICIO file for year ", i)
    file_name <- paste0(i, "_SML.csv")
    tmp <- utils::read.csv(file.path(cache_dir, file_name))
    icio[[paste0(i)]] <- tmp
  }

  return(icio)
}
