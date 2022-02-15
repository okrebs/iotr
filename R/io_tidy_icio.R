#' Tidy data from OECD's ICIO
#'
#' Function to turn a list of OECD ICIO components, usually obtained from a call
#' to \code{io_load_icio()}, into a tibble, i.e. a 'tidy' data frame.
#'
#' @param icio list containing ICIO components
#' @param quiet if TRUE will try to avoid printing messages
#' @return Returns one \code{tibble} combining all components from \code{icio}.
#' @references OECD (2021), OECD Inter-Country Input-Output Database,
#'   http://oe.cd/icio
#' @example man/examples/icio.R
#' @export io_tidy_icio

io_tidy_icio <- function(icio, quiet = FALSE) {

  components <- names(icio)
  if(any(components != check_icio_components(names(icio), "all"))) {
    stop("'icio' list contains invalid elements.")
  }

  for(i in components) {
    if(!quiet) message("Tidying component ", i)
    # first dimension is always the year
    tmp <- NULL
    for(t in dimnames(icio[[i]])[[1]]) {
      # due to size/time each year done separately
      tmp_t <- abind::asub(icio[[i]], t, 1)
      n_dims <- length(dim(tmp_t))
      if(n_dims == 0) {
        tmp_t <- tibble::tibble(name = names(tmp_t), type = i, flow = tmp_t)
        new_cols <-
          dplyr::case_when(
            i %in% c("VA", "VAexTAX") ~ c("destination", "use", "sector"),
            i == "X" ~ c("origin", "sector", "use")
          )
        tmp_t <- dplyr
          tidyr::separate(tmp_t,
                          name,
                          into = new_cols[1:2],
                          sep = "_")
        tmp_t <- dplyr::rename(tmp_t, "{new_cols[3]}" := type)
      } else if(n_dims == 2) {
        tmp_t <- tibble::as_tibble(tmp_t, rownames = "row_id")
        tmp_t <-
          tidyr::pivot_longer(tmp_t, -row_id, names_to = "col_id", values_to = "flow")
        if(i != "CVB") {
          # tidyr::separate is much too slow for these very large tibbles
          tmp_t[["origin"]] <- substr(tmp_t$row_id, 1, 3)
          tmp_t[["sector"]] <- substr(tmp_t$row_id, 5, max(nchar(tmp_t$row_id)))
          tmp_t <- dplyr::select(tmp_t, -row_id)
        } else {
          tmp_t <- dplyr::rename(tmp_t, "origin" = row_id)
          tmp_t <- tibble::add_column(tmp_t, sector = i)
        }
        if(i %in% c("Z", "A", "B", "VB", "CVB")) {
          # large components take a bit so better output something
          if(!quiet & i != "CVB") message("...year ", t)
          tmp_t[["destination"]] <- substr(tmp_t$col_id, 1, 3)
          tmp_t[["use"]] <- substr(tmp_t$col_id, 5, max(nchar(tmp_t$col_id)))
          tmp_t <- dplyr::select(tmp_t, -col_id)
        } else {
          tmp_t <- dplyr::rename(tmp_t, "destination" = col_id)
          tmp_t <- tibble::add_column(tmp_t, use = i)
        }
      }
      tmp_t <- dplyr::mutate(tmp_t, year = t)
      tmp <- dplyr::bind_rows(tmp, tmp_t)
    }
    icio[[i]] <- tmp
  }
  rm(i, t, n_dims, tmp, tmp_t)
  icio <- dplyr::bind_rows(icio)
  icio <- dplyr::select(icio, year, origin, sector, destination, use, flow)

  return(icio)
}
