#' Sector and Country/Location Correspondences
#'
#' A list that, for each IO data set in the packages, provides a sector and a
#' country/locations correspondence.
#'
#' @format A list with each element representing one of the data sets in the
#' package. Each element is itself a list containing
#' \describe{
#'   \item{wiot}{Sectors and Locations in the World Input Output Database}
#'   \itemize{
#'     \item{locations:}
#'     \describe{
#'       \item{location_id}{id for each location, with alphabetically ordered iso3}
#'       \item{wiot_location_id}{same as above but with ROW assigned the last id}
#'       \item{wiot_location_iso3}{country iso3 as provided by WIOD}
#'       \item{wiot_location}{full name as provided by WIOD}
#'     }
#'   }
#' }
"io_correspondences"
