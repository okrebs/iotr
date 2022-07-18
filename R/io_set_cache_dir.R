#' Set paths to cache large IO downloads
#'
#' @details Several `io_dl_*` and `io_load_*` functions ask for a cache
#' directory to cache several large IO data files in order to avoid repeated
#' downloads. This function is a helper to (re)set the folder paths where to
#' keep these files. The given path is stored in a config file (at the user
#' configs location obtained from package rappdirs). Manually call this funtion
#' with a new path to reset it.
#'
#' @param cache_dir If set to a character this will replace the current cache
#'   path stored in the iotr configs.
#' @param io_database For which database should the cache directory be set.
#'   Current options are 'wiod'  or 'icio'.
#' @return Returns path to the respective cache directory.
#' @export io_set_cache_dir
io_set_cache_dir <- function(cache_dir = NULL, io_database = NULL) {

  if (!interactive()) {
    stop("This function should only be called in interactive mode")
  }
  config_path <- rappdirs::user_config_dir("iotr")
  cache_config_path <- file.path(
    config_path,
    paste0(io_database, "_cache_path.txt")
  )

  if (file.exists(cache_config_path) & is.null(cache_dir)) {
    cache_dir <- readLines(cache_config_path)
    if (is.null(cache_dir)) stop("Cache config file found but was empty.")
  } else {
    if (!dir.exists(config_path)) {
      dir.create(config_path)
    }
    if(is.null(cache_dir)) {

      path_message <- paste0(
        "a path to cache downloaded",
        io_database,
        " raw data. Use 'io_set_cache_dir(new_path)' to reset it later."
      )
      if (requireNamespace("rstudioapi", quietly = TRUE) & rstudioapi::isAvailable()) {
        cache_dir <- rstudioapi::selectDirectory(
          caption = paste0("Pick ", path_message)
        )
      } else {
        cache_dir <- readline(
          prompt = paste0("Enter ", path_message)
        )
      }
    }
    writeLines(cache_dir, cache_config_path)
  }
  return(cache_dir)
}
