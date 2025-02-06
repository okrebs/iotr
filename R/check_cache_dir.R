#' @noRd
#'
check_cache_dir <- function(cache_dir, quiet) {
  if (is.null(cache_dir)) {
    if (!quiet) {
      message(
        "No cache directory given. Data will be downloaded to a temporary ",
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

  return(cache_dir)
}
