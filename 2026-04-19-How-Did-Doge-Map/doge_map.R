library(dplyr)
library(leaflet)
library(sf)
library(tigris)
library(readr)
library(htmltools)
library(htmlwidgets)

df <- read_csv("terminations.csv")
df <- df[grep("Convenience", df$Reason),]

sa <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","AS","GU","MP","PR","VI")
val <- c(30988197598,8277938175,59618331140,17285963733,247027614139,31425993446,64815795387,6145304245,21707811382,145283094932,49648153964,8511361930,10710279372,67003947318,94826083044,16606118110,14409650329,92182866851,35227443923,9763583280,39978128542,49304996135,63513144063,116628959382,16710400999,47390427416,6902944270,14010021422,17434136207,7381922092,47966902935,26700513683,134634485853,61841144214,42762460670,66455684566,25182494625,30915586141,136390391356,7766734897,63105962745,5286453189,60465059837,153878093550,13700994714,4352872431,82417849986,53404166785,12579526279,52830576807,3035792776,312939706,971183884,257969807,20152934420,545992425)
spending_df <- data.frame(State = sa, Total_Spending = val, stringsAsFactors = FALSE)

state_summary <- df %>%
  group_by(State) %>%
  summarise(Total_Mod_Change = sum(`Mod $ Change`, na.rm = TRUE)) %>%
  filter(!is.na(State)) %>%
  left_join(spending_df, by = "State") %>%
  mutate(Normalized_Change = Total_Mod_Change / Total_Spending)

options(tigris_use_cache = TRUE)
us_states <- states(cb = TRUE, resolution = "20m")

map_data <- us_states %>%
  left_join(state_summary, by = c("STUSPS" = "State")) %>%
  filter(STUSPS %in% c(state.abb, "DC", "PR"))

pal_orig <- colorNumeric("viridis", domain = map_data$Total_Mod_Change, na.color = "#e5e5e5")

labels_orig <- sprintf(
  "<strong>%s</strong><br/>Total Mod $ Change: $%s",
  map_data$NAME, 
  formatC(map_data$Total_Mod_Change, format = "f", big.mark = ",", digits = 2)
) %>% lapply(htmltools::HTML)

map_original <- leaflet(data = map_data, width = "100%") %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -96, lat = 37.8, zoom = 4) %>%
  addPolygons(
    fillColor = ~pal_orig(Total_Mod_Change),
    weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.8,
    highlightOptions = highlightOptions(weight = 2, color = "#333333", dashArray = "", fillOpacity = 1, bringToFront = TRUE),
    label = labels_orig
  ) %>%
  addLegend(pal = pal_orig, values = ~Total_Mod_Change, title = "Total $ Cancelled", position = "bottomright") %>%
  addLabelOnlyMarkers( #feed the trolls
    lng = -90.0, lat = 25.0,
    label = htmltools::HTML("<span style='color:#006847;'>Gulf</span> <span style='color:#FFFFFF;'>of</span> <span style='color:#CE1126;'>Mexico</span>"),
    labelOptions = labelOptions(
      noHide = TRUE, textOnly = TRUE, direction = "center",
      style = list("font-size" = "24px", "font-weight" = "bold", "font-style" = "italic", "text-shadow" = "2px 2px 4px rgba(0,0,0,0.85)")
    )
  )

pal_norm <- colorNumeric("magma", domain = map_data$Normalized_Change, na.color = "#e5e5e5")

labels_norm <- sprintf(
  "<strong>%s</strong><br/>Normalized Change: %s%%<br/><em>(Cancelled: $%s)</em><br/><em>(Spending: $%s)</em>",
  map_data$NAME, 
  formatC(map_data$Normalized_Change * 100, format = "f", digits = 4),
  formatC(map_data$Total_Mod_Change, format = "f", big.mark = ",", digits = 0),
  formatC(map_data$Total_Spending, format = "f", big.mark = ",", digits = 0)
) %>% lapply(htmltools::HTML)

map_normalized <- leaflet(data = map_data, width = "100%") %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -96, lat = 37.8, zoom = 4) %>%
  addPolygons(
    fillColor = ~pal_norm(Normalized_Change),
    weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.8,
    highlightOptions = highlightOptions(weight = 2, color = "#333333", dashArray = "", fillOpacity = 1, bringToFront = TRUE),
    label = labels_norm
  ) %>%
  addLegend(
    pal = pal_norm, 
    values = ~Normalized_Change, 
    title = "Normalized Change<br>(Percent of Spending)", 
    position = "bottomright",
    labFormat = labelFormat(transform = function(x) x * 100, suffix = "%") 
  ) %>%
  addLabelOnlyMarkers(
    lng = -90.0, lat = 25.0,
    label = htmltools::HTML("<span style='color:#006847;'>Gulf</span> <span style='color:#FFFFFF;'>of</span> <span style='color:#CE1126;'>Mexico</span>"),
    labelOptions = labelOptions(
      noHide = TRUE, textOnly = TRUE, direction = "center",
      style = list("font-size" = "24px", "font-weight" = "bold", "font-style" = "italic", "text-shadow" = "2px 2px 4px rgba(0,0,0,0.85)")
    )
  )
