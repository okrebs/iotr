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

# ICIO

icio_sectors <- tibble(
  sector_id = 1:45,
  icio_sector_id = c(
    "D01T02",
    "D03",
    "D05T06",
    "D07T08",
    "D09",
    "D10T12",
    "D13T15",
    "D16",
    "D17T18",
    "D19",
    "D20",
    "D21",
    "D22",
    "D23",
    "D24",
    "D25",
    "D26",
    "D27",
    "D28",
    "D29",
    "D30",
    "D31T33",
    "D35",
    "D36T39",
    "D41T43",
    "D45T47",
    "D49",
    "D50",
    "D51",
    "D52",
    "D53",
    "D55T56",
    "D58T60",
    "D61",
    "D62T63",
    "D64T66",
    "D68",
    "D69T75",
    "D77T82",
    "D84",
    "D85",
    "D86T88",
    "D90T93",
    "D94T96",
    "D97T98"
  ),
  sector_name = c(
    "Agriculture, hunting, forestry",
    "Fishing and aquaculture",
    "Mining and quarrying, energy producing products",
    "Mining and quarrying, non-energy producing products",
    "Mining support service activities",
    "Food products, beverages and tobacco",
    "Textiles, textile products, leather and footwear",
    "Wood and products of wood and cork",
    "Paper products and printing",
    "Coke and refined petroleum products",
    "Chemical and chemical products",
    "Pharmaceuticals, medicinal chemical and botanical products",
    "Rubber and plastics products",
    "Other non-metallic mineral products",
    "Basic metals",
    "Fabricated metal products",
    "Computer, electronic and optical equipment",
    "Electrical equipment",
    "Machinery and equipment, nec ",
    "Motor vehicles, trailers and semi-trailers",
    "Other transport equipment",
    "Manufacturing nec; repair and installation of machinery and equipment",
    "Electricity, gas, steam and air conditioning supply",
    "Water supply; sewerage, waste management and remediation activities",
    "Construction",
    "Wholesale and retail trade; repair of motor vehicles",
    "Land transport and transport via pipelines",
    "Water transport",
    "Air transport",
    "Warehousing and support activities for transportation",
    "Postal and courier activities",
    "Accommodation and food service activities",
    "Publishing, audiovisual and broadcasting activities",
    "Telecommunications",
    "IT and other information services",
    "Financial and insurance activities",
    "Real estate activities",
    "Professional, scientific and technical activities",
    "Administrative and support services",
    "Public administration and defence; compulsory social security",
    "Education",
    "Human health and social work activities",
    "Arts, entertainment and recreation",
    "Other service activities",
    "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use"
  )
)

icio_locations <- tibble(
  location_id = 1:71,
  iso3 = c(
    "ARG",
    "AUS",
    "AUT",
    "BEL",
    "BGR",
    "BRA",
    "BRN",
    "CAN",
    "CHE",
    "CHL",
    "CHN",
    "CHN",
    "CHN",
    "COL",
    "CRI",
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
    "HKG",
    "HRV",
    "HUN",
    "IDN",
    "IND",
    "IRL",
    "ISL",
    "ISR",
    "ITA",
    "JPN",
    "KAZ",
    "KHM",
    "KOR",
    "LAO",
    "LTU",
    "LUX",
    "LVA",
    "MAR",
    "MEX",
    "MLT",
    "MMR",
    "MEX",
    "MEX",
    "MYS",
    "NLD",
    "NOR",
    "NZL",
    "PER",
    "PHL",
    "POL",
    "PRT",
    "ROU",
    "ROW",
    "RUS",
    "SAU",
    "SGP",
    "SVK",
    "SVN",
    "SWE",
    "THA",
    "TUN",
    "TUR",
    "TWN",
    "USA",
    "VNM",
    "ZAF"
  ),
  icio_iso3 = c(
    "ARG",
    "AUS",
    "AUT",
    "BEL",
    "BGR",
    "BRA",
    "BRN",
    "CAN",
    "CHE",
    "CHL",
    "CHN",
    "CN1",
    "CN2",
    "COL",
    "CRI",
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
    "HKG",
    "HRV",
    "HUN",
    "IDN",
    "IND",
    "IRL",
    "ISL",
    "ISR",
    "ITA",
    "JPN",
    "KAZ",
    "KHM",
    "KOR",
    "LAO",
    "LTU",
    "LUX",
    "LVA",
    "MAR",
    "MEX",
    "MLT",
    "MMR",
    "MX1",
    "MX2",
    "MYS",
    "NLD",
    "NOR",
    "NZL",
    "PER",
    "PHL",
    "POL",
    "PRT",
    "ROU",
    "ROW",
    "RUS",
    "SAU",
    "SGP",
    "SVK",
    "SVN",
    "SWE",
    "THA",
    "TUN",
    "TUR",
    "TWN",
    "USA",
    "VNM",
    "ZAF"
  ),
  icio_location = c(
    "Argentina",
    "Australia",
    "Austria",
    "Belgium",
    "Bulgaria",
    "Brazil",
    "Brunei Darussalam",
    "Canada",
    "Switzerland",
    "Chile",
    "China (People's Republic of)",
    "China - Activities excluding export processing",
    "China - Export processing activities",
    "Colombia",
    "Costa Rica",
    "Cyprus2",
    "Czech Republic - Czechia",
    "Germany",
    "Denmark",
    "Spain",
    "Estonia",
    "Finland",
    "France",
    "United Kingdom",
    "Greece",
    "Hong Kong, China",
    "Croatia",
    "Hungary",
    "Indonesia",
    "India",
    "Ireland",
    "Iceland",
    "Israel1",
    "Italy",
    "Japan",
    "Kazakhstan",
    "Cambodia",
    "Korea",
    "Lao People's Democratic Republic",
    "Lithuania",
    "Luxembourg",
    "Latvia",
    "Morocco",
    "Mexico",
    "Malta",
    "Myanmar",
    "Mexico - Activities excluding Global Manufacturing",
    "Mexico - Global Manufacturing activities",
    "Malaysia",
    "Netherlands",
    "Norway",
    "New Zealand",
    "Peru",
    "Philippines",
    "Poland",
    "Portugal",
    "Romania",
    "Rest of the World",
    "Russian Federation",
    "Saudi Arabia",
    "Singapore",
    "Slovak Republic",
    "Slovenia",
    "Sweden",
    "Thailand",
    "Tunisia",
    "Turkey",
    "Chinese Taipei",
    "United States",
    "Viet Nam",
    "South Africa"
  )
)

icio <- list(
  locations = icio_locations,
  sectors = icio_sectors
)

# Combine and Save
io_correspondences <-
  list(
    wiot = wiot,
    icio = icio
  )

usethis::use_data(io_correspondences, overwrite = TRUE)
