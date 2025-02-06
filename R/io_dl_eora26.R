#' Download IO tables from the Eora26 data set
#'
#' Function to download the multiregional input output (MRIO) tables of the
#' Eora26. Should usually not be called directly but through
#' \code{io_load_eora26()}. Note this tool simply automates the download process
#' but you need to obtain an account from https://worldmrio.com and agree to
#' their usage and licensing terms to download and use this data.
#'
#' @param years which year(s) to load. Must be a single value or vector of
#'   values between 1990 and the latest year available with your account.
#' @param cache_dir path to cache the Eora26 data. Downloads will overwrite
#'   files of the same name in this location.
#' @param email the email adress of the account
#' @param password the password of the account
#' @param quiet if TRUE will try to avoid printing messages
#' @return Path to the directory where data was saved, if given, this is
#'   identical to \code{cache_dir}
#' @references Lenzen M, Kanemoto K; Moran D, and Geschke A (2012) Mapping the
#'   structure of the world economy. Environmental Science & Technology 46(15)
#'   pp 8374–8381.
#'
#'   Lenzen, M., Moran, D., Kanemoto, K., Geschke, A. (2013) Building Eora: A
#'   Global Multi-regional Input-Output Database at High Country and Sector
#'   Resolution. Economic Systems Research, 25:1, 20-49.
#' @export io_dl_eora26

io_dl_eora26 <- function(years,
                         cache_dir = NULL,
                         email = NULL,
                         password = NULL,
                         quiet = FALSE) {

  cache_dir <- check_cache_dir(cache_dir, quiet)

  if(is.null(email) | is.null(password)) {
    stop("To use the Eora26 you must obtain an account at www.worldmrio.com, ",
         "agree to the licensing and usage terms, and provide you account's",
         "email adress and password. One of the two was not specified.")
  }

  cookie <- tempfile()
  headers = list(
    `User-Agent` = "iotr/0.0.0.9001 github.com/okrebs/iotr",
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    `Accept-Language` = "de,en-US;q=0.7,en;q=0.3",
    `Accept-Encoding` = "gzip, deflate, br, zstd",
    DNT = "1",
    `Sec-GPC` = "1",
    Connection = "keep-alive",
    Referer = "https://worldmrio.com/eora26/",
    `Upgrade-Insecure-Requests` = "1",
    `Sec-Fetch-Dest` = "document",
    `Sec-Fetch-Mode` = "navigate",
    `Sec-Fetch-Site` = "same-origin",
    `Sec-Fetch-User` = "?1",
    Priority = "u=0, i",
    TE = "trailers"
  )

  # successful login is stored in cookie
  cookie_path <- tempfile()
  # open login site once
  loginurl <- "https://worldmrio.com/login.jsp"
  resp <- httr2::request(loginurl) |>
    httr2::req_cookie_preserve(cookie_path) |>
    httr2::req_perform()
  # login to EORA homepage
  resp <- httr2::request("https://worldmrio.com/Register2") |>
    httr2::req_cookie_preserve(cookie_path) |>
    httr2::req_method("POST") |>
    httr2::req_url_query(submit = "login") |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_form(
      email = email,
      targetURL = "null",
      pass = password,
      submit = "login"
    ) |>
    httr2::req_perform()

  # EORA has regular updates that should be saved in different folders
  eora26_website <- httr2::request("https://worldmrio.com/eora26/") |>
    httr2::req_cookie_preserve(cookie_path) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  eora26_version <- regmatches(
    eora26_website, regexpr("v[0-9]{1,3}\\.[0-9]{1,3}", eora26_website)
  )
  if(!quiet) message("Using current EORA26 version ", eora26_version)

  if(!dir.exists(file.path(cache_dir, eora26_version))) {
    if(!quiet) {
      available_versions <- regmatches(
        dir(cache_dir), regexpr("v[0-9]{1,3}\\.[0-9]{1,3}", dir(cache_dir))
      )
      message("Could not find a directory with the current version number ",
              "under ", cache_dir, ". Existing version directories (if any): ",
              "\n", paste(available_versions, collapse = ", "),
              "\nCreating new directory.")
    }
    dir.create(file.path(cache_dir, eora26_version))
  }

  eora26_download_url <- paste0(
    "https://worldmrio.com",
    sub('.*<a href=\"(.*?)Eora26_2017_bp.*', "\\1", eora26_website)
  )

  # download data
  for(year in years) {
    url = paste0(eora26_download_url, "Eora26_", year, "_bp.zip")
    tmp_file <- tempfile()
    if(!quiet) message("Downloading data for year", year, ".")
    httr2::request(url) |>
      httr2::req_cookie_preserve(cookie_path) |>
      httr2::req_headers(!!!headers) |>
      httr2::req_progress() |>
      httr2::req_perform(path = tmp_file)

    files_to_unzip <- paste0(
      paste0("Eora26_", year, "_bp_"), c("VA", "T", "FD"), ".txt"
    )
    if(!quiet) message("Unzipping data for year ", year, ".")
    utils::unzip(
      tmp_file,
      files = files_to_unzip,
      exdir = file.path(cache_dir, eora26_version)
    )
  }

  # extract labels only once for all years
  label_files <- paste0("labels_", c("VA", "T", "FD"), ".txt")
  if(!all(file.exists(file.path(cache_dir, eora26_version, label_files)))) {
    utils::unzip(tmp_file, files = label_files, exdir = cache_dir)
  }

  return(cache_dir)
}
