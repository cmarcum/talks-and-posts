library(dplyr)
library(leaflet)
library(sf)
library(tigris)
library(readr)
library(htmltools)
library(htmlwidgets)

df <- read_csv("terminations.csv")
df <- df[grep("Convenience", df$Reason),]

state_summary <- df %>%
  group_by(State) %>%
  summarise(Total_Mod_Change = sum(`Mod $ Change`, na.rm = TRUE)) %>%
  filter(!is.na(State))

options(tigris_use_cache = TRUE)
us_states <- states(cb = TRUE, resolution = "20m")

map_data <- us_states %>%
  left_join(state_summary, by = c("STUSPS" = "State")) %>%
  # Filter to the contiguous US, Alaska, Hawaii, DC, and PR for a clean map extent
  filter(STUSPS %in% c(state.abb, "DC", "PR"))

pal <- colorNumeric(
  palette = "viridis",
  domain = map_data$Total_Mod_Change,
  na.color = "#e5e5e5"
)

labels <- sprintf(
  "<strong>%s</strong><br/>Total Mod $ Change: $%s",
  map_data$NAME, 
  formatC(map_data$Total_Mod_Change, format = "f", big.mark = ",", digits = 2)
) %>% lapply(htmltools::HTML)

interactive_map <- leaflet(data = map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = -96, lat = 37.8, zoom = 4) %>%     
  addPolygons(
    fillColor = ~pal(Total_Mod_Change),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.8,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#333333",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~Total_Mod_Change,
    opacity = 0.8,
    title = "Total $ Cancelled",
    position = "bottomright",
    na.label = "No Data"
  ) %>%
addLabelOnlyMarkers( #Feed the trolls
  lng = -90.0, lat = 25.0,
  label = htmltools::HTML("<span style='color:#006847;'>Gulf</span> <span style='color:#FFFFFF;'>of</span> <span style='color:#CE1126;'>Mexico</span>"),
  labelOptions = labelOptions(
    noHide = TRUE,
    textOnly = TRUE,
    direction = "center",
    style = list(
      "font-size" = "24px",
      "font-weight" = "bold",
      "font-style" = "italic",
      "text-shadow" = "2px 2px 4px rgba(0,0,0,0.85)" 
    )
  )
)

interactive_map

saveWidget(
  widget = interactive_map, 
  file = "2025spending.html", 
  selfcontained = TRUE
)
