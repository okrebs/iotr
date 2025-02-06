#' Load IO tables from the EORA26 MRIO
#'
#' Function to load and, if not cached locally, download the set of
#' multiregional input output (MRIO) tables of the EORA. Note that this tool
#' simply automates the download process but you need to obtain an account from
#' https://worldmrio.com and agree to their usage and licensing terms to
#' download and use this data.
#'
#' @param years which year(s) to load. Must be a single value or vector of
#'   values between 1990 and the latest year available with your account.
#' @param cache_dir path to cache the Eora26 data. Downloads will overwrite
#'   files of the same name in this location.
#' @param email the email adress of the account
#' @param password the password of the account
#' @param version the EORA26 version to load. If NULL the most recent version in
#'   the cache will be used if any is available, otherwise, or if "current" the
#'   most recent version will be checked from the website and if necessary
#'   downloaded
#' @param quiet if TRUE will try to avoid printing messages
#' @return Returns a list combining for each requested year, the T, VA and FD
#'   components from the requested version of the EORA26 as well as as a list
#'   element with the corresponding labels.
#' @references Lenzen M, Kanemoto K; Moran D, and Geschke A (2012) Mapping the
#'   structure of the world economy. Environmental Science & Technology 46(15)
#'   pp 8374–8381.
#'
#'   Lenzen, M., Moran, D., Kanemoto, K., Geschke, A. (2013) Building Eora: A
#'   Global Multi-regional Input-Output Database at High Country and Sector
#'   Resolution. Economic Systems Research, 25:1, 20-49.
#' @example man/examples/eora26.R
#' @export io_load_eora26

io_load_eora26 <- function(years,
                           cache_dir = NULL,
                           email = NULL,
                           password = NULL,
                           version = NULL,
                           quiet = FALSE) {

  cache_dir <- check_cache_dir(cache_dir, quiet)

  if(!all(is.wholenumber(years)) | any(years < 1990)) {
    stop("'years' out of range. Must be integers between 1990 and the most ",
         "recent year available for your EORA account.")
  }

  if(is.null(version)) {
    specific_version_requested <- FALSE
    cached_versions <- regmatches(
      dir(cache_dir), regexpr("v[0-9]{1,3}\\.[0-9]{1,3}", dir(cache_dir))
    )
    if(length(cached_versions) == 0) {
      if(!quiet) message("No Eora26 versions found in cache.")
      version <- "current"
    } else {
      major_version <- regmatches(
        cached_versions, regexpr("(?<=v)[0-9]{1,3}", cached_versions, perl = TRUE)
      )
      minor_version <- regmatches(
        cached_versions, regexpr("(?<=\\.)[0-9]{1,3}", cached_versions, perl = TRUE)
      )
      max_major_version <- max(as.numeric(major_version))
      max_major_version_idx <- which.max(as.numeric(major_version))
      max_minor_version <- max(minor_version[max_major_version_idx])
      version <- paste0("v", max_major_version, ".", max_minor_version)
      if(!quiet) message("Most recent Eora26 version found in cache: ", version)
    }
  } else {
    specific_version_requested <- TRUE
  }

  if(version == "current") {
    current_version <- check_current_version_eora26()
    version <- current_version
    if(!quiet) message("Most recent Eora26 version found online: ", version)
  }

  dl_years <- NULL
  for(year in years) {
    file_names <- paste0(paste0("Eora26_", year, "_bp_"), c("VA", "T", "FD"), ".txt")
    if (!all(file.exists(file.path(cache_dir, version, file_names)))) {
      if (!quiet) message(
        paste(file_names[!file.exists(file.path(cache_dir, version, file_names))], collapse = ", "),
        " not found in cache."
      )
      dl_years <- c(dl_years, years)
    }
  }

  if(specific_version_requested) {
   if(version != current_version & !is.null(dl_years)) {
      stop("You asked for Eora26 version ", version, " but years ", dl_years,
           " for that version were not found in cache_dir. Choose another year ",
           "or version, or set version to 'current' to download all years you ",
           "asked for from the current online data, i.e. ", current_version ,".")
   }
  }

  if(is.null(dl_years) & !quiet) {
    message("All requested data found in cache.")
  } else {
    for(i in dl_years) {
      if(!quiet) message("Downloading years with missing files.")
      cache_dir <- io_dl_eora26(
        years = dl_years,
        cache_dir = cache_dir,
        email = email,
        password = password,
        quiet = quiet
      )
    }
  }

  label_files <- paste0("labels_", c("VA", "T", "FD"), ".txt")
  if (!all(file.exists(file.path(cache_dir, version, label_files)))) {
    message("Eora26 label files were not found in cache. Downloading...")
    major_version <- sprintf("%03d", as.numeric(regmatches(
      version, regexpr("(?<=v)[0-9]{1,3}", version, perl = TRUE)
    )))
    minor_version <- sprintf("%03d", as.numeric(regmatches(
      version, regexpr("(?<=\\.)[0-9]{1,3}", version, perl = TRUE)
    )))
    labels_url <- paste0("https://worldmrio.com/ComputationsM/Phase",
                         major_version, "/Loop", minor_version,
                         "/simplified/indices.zip")
    tmp_file <- tempfile()
    httr2::request(labels_url) |>
      httr2::req_perform() |>
      httr2::req_progress() |>
      httr2::req_perform(path = tmp_file)
    utils::unzip(tmp_file, exdir = file.path(cache_dir, version))
  }

  labels <- lapply(
    file.path(cache_dir, version, label_files),
    utils::read.delim,
    header = FALSE
  ) |>
    stats::setNames(sub("labels_(.*)\\.txt", "\\1", label_files))
  labels[["T"]] <- stats::setNames(
    labels[["T"]][,c(1,4)],
    c("origin", "sector")
  )
  labels[["FD"]] <- stats::setNames(
    labels[["FD"]][,c(1,4)],
    c("destination", "use")
  )
  labels[["VA"]] <- stats::setNames(labels[["VA"]][,2, drop = FALSE], "sector")


  eora26 <- list(
    labels = labels
  )
  for(year in years) {
    if(!quiet) message("Reading Eora26 files for year ", year)

    file_names <- sapply(c("T", "VA", "FD"), function(type) {
      paste0("Eora26_", year, "_bp_", type, ".txt")
    })
    eora26[[paste0(year)]] <- list(
      "T" = readr::read_delim(
        file.path(cache_dir, version, file_names["T"]),
        col_names = FALSE,
        col_types = "d"),
      "VA" = readr::read_delim(
        file.path(cache_dir, version, file_names["VA"]),
        col_names = FALSE,
        col_types = "d"),
      "FD" = readr::read_delim(
        file.path(cache_dir, version, file_names["FD"]),
        col_names = FALSE,
        col_types = "d")
    )
  }
  return(eora26)
}

