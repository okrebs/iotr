\dontrun{

  library(iotr)

  # set cache_dir for ICIO to avoid (very) long download times on every run !!!
  cache_dir <- NULL

  icio_v2_raw <- io_load_icio_v2(cache_dir, years = 2010:2012)

  icio_v2 <- io_tidy_icio_v2(icio_v2_raw)
}
