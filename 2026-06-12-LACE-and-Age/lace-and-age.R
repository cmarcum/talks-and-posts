library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(htmlwidgets)
library(tidycensus)
library(dplyr)

## Uncomment and add your your own Census API key from: https://api.census.gov/data/key_signup.html
#census_api_key("your_key", install = TRUE)

lace_data <- read_csv("https://www2.census.gov/programs-surveys/demo/datasets/lace/2023/LACE_23_County.csv")

lace_clean <- lace_data %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  select(GEOID, County_Name = NAME, NO_AC_PE)

acs_data <- get_acs(
  geography = "county",
  variables = "DP05_0024PE",
  year = 2022,
  survey = "acs5"
) %>%
  rename(B01001_ABOVE64_PCT = estimate) %>%
  select(GEOID, B01001_ABOVE64_PCT)

merged_data <- lace_clean %>%
  inner_join(acs_data, by = "GEOID")

options(tigris_use_cache = TRUE)
counties_geo <- counties(cb = TRUE, resolution = "20m", year = 2022)

map_data <- counties_geo %>%
  inner_join(merged_data, by = "GEOID")

map_data <- st_transform(map_data, crs = 4326)

pal_ac <- colorNumeric(palette = "inferno", domain = map_data$NO_AC_PE)

interactive_map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal_ac(NO_AC_PE),
    weight = 0.5,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "darkgray",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~sprintf(
      "%sHouseholds without AC: %g%% Population over 65: %g%%",
      County_Name, NO_AC_PE, B01001_ABOVE64_PCT
    ) %>% lapply(htmltools::HTML)
  ) %>%
  addLegend(
    pal = pal_ac,
    values = ~NO_AC_PE,
    opacity = 0.7,
    title = "No AC (%)",
    position = "bottomright"
  )

saveWidget(interactive_map, "lace_elderly_map.html", selfcontained = TRUE)
