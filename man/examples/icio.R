library(iotr)

# set cache_dir for ICIO to avoid (very) long download times on every run !!!
cache_dir <- NULL

icio_raw <- io_load_icio(cache_dir,
                         years = 2010:2012,
                         components = c("Z", "CONS", "GFCF", "INVNT", "NONRES"))

icio <- io_tidy_icio(icio_raw)
