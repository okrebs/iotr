#' @noRd
#'
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

#' @noRd
#'
load_into <- function(path) {
  load(path)
  get(ls()[ls() != "path"])
}
