library(iotr)
library(dplyr)
library(tibble)

# WIOT

wiot <- io_load_wiot("/home/uxb/RawData/WIOD/", years = 2014)

wiot_sectors <-
  tibble(
    wiot_sector_id = wiot$IndustryCode,
    wiot_sector = wiot$IndustryDescription
  ) %>%
  distinct() %>%
  rowid_to_column("sector_id")

wiot_locations <-
  tibble(
    wiot_location_id = 1:44,
    wiot_location_iso3 = c(
      "AUS",
      "AUT",
      "BEL",
      "BGR",
      "BRA",
      "CAN",
      "CHE",
      "CHN",
      "CYP",
      "CZE",
      "DEU",
      "DNK",
      "ESP",
      "EST",
      "FIN",
      "FRA",
      "GBR",
      "GRC",
      "HRV",
      "HUN",
      "IDN",
      "IND",
      "IRL",
      "ITA",
      "JPN",
      "KOR",
      "LTU",
      "LUX",
      "LVA",
      "MEX",
      "MLT",
      "NLD",
      "NOR",
      "POL",
      "PRT",
      "ROU",
      "RUS",
      "SVK",
      "SVN",
      "SWE",
      "TUR",
      "TWN",
      "USA",
      "ROW"
    ),
    wiot_location = c(
      "Australia",
      "Austria",
      "Belgium",
      "Bulgaria",
      "Brazil",
      "Canada",
      "Switzerland",
      "China, People's Republic of",
      "Cyprus",
      "Czech Republic",
      "Germany",
      "Denmark",
      "Spain",
      "Estonia",
      "Finland",
      "France",
      "United Kingdom of Great Britain and Northern Ireland",
      "Greece",
      "Croatia",
      "Hungary",
      "Indonesia",
      "India",
      "Ireland",
      "Italy",
      "Japan",
      "Republic of Korea",
      "Lithuania",
      "Luxembourg",
      "Latvia",
      "Mexico",
      "Malta",
      "Netherlands",
      "Norway",
      "Poland",
      "Portugal",
      "Romania",
      "Russian Federation",
      "Slovakia",
      "Slovenia",
      "Sweden",
      "Turkey",
      "Taiwan",
      "United States",
      "Rest of World"
    ))

wiot_locations <- wiot_locations %>%
  arrange(wiot_location_iso3) %>%
  rowid_to_column("location_id")

wiot <- list(
  locations = wiot_locations,
  sectors = wiot_sectors
)

# ICIO - TBD

# Combine and Save
io_correspondences <-
  list(
    wiot = wiot
    #icio = icio
  )

usethis::use_data(io_correspondences, overwrite = TRUE)
