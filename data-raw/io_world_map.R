library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(s2)
library(sf)

sf_use_s2(FALSE)

io_world_map <-
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  st_crop(c(
    "xmin" = -170,
    "xmax" = 180,
    "ymin" = -56,
    "ymax" = 91
  )) %>%
  select(country = adm0_a3, geometry)

sf_use_s2( TRUE)

usethis::use_data(io_world_map, overwrite = TRUE, internal = TRUE)
