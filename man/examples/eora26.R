\dontrun{

  library(iotr)

  # set cache_dir for EORA to avoid (very) long download times on every run !!!
  cache_dir <- NULL

  eora26_raw <- io_load_icio_v2(cache_dir, years = 2016:2017)

  eora26 <- io_tidy_eora26(eora26_raw)
}
