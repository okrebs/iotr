#'\dontrun{

library(iotr)

# set cache_dir for WIOD to avoid long download times on every run !!!
cache_dir <- NULL

wiot_raw <- io_load_wiot(cache_dir, years = 2000:2014)

wiot_long <- io_tidy_wiot(wiot_raw)

# get wiot into standard long IO-table format
iot <- dplyr::filter(wiot_long, Country != "TOT", Year == "2014")
iot <-
  dplyr::select(iot,
                origin = Country,
                sector = RNr,
                destination,
                use,
                flow)

iot <- io_rm_negative_vad(iot, category_to_scale = 57)

iot <-
  io_rm_dynamics(iot,
                 dynamic_categories = c(60, 61),
                 category_to_scale = 57)

iot <- io_gen_own_trade(iot, max_replace = 1e-6)

#' }
