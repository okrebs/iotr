library(iotr)
library(dplyr)

wiot <- get_wiot(cache_dir = tempdir(), years = 2014)
wiot_long <- wiot2long(wiot)

# get wiot into standard long IO-table format
iot <- wiot_long %>%
  filter(Country != "TOT") %>%
  select(origin = Country, sector = RNr, destination, use, flow)

iot <- rm_negative_vad(iot, category_to_scale = 57)

iot <- rm_dynamics(iot, dynamic_categories = c(60, 61), category_to_scale = 57)

iot <- gen_own_trade(iot, max_replace = 1e-6)
