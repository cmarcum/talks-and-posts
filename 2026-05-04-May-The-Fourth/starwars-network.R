library(jsonlite)
library(visNetwork)
library(dplyr)

# The data come from Evelina Gabasova's github repository
url <- "https://raw.githubusercontent.com/evelinag/starwars-social-network/master/networks/starwars-full-interactions-allCharacters.json"
sw_data <- fromJSON(url)

nodes <- data.frame(
  id = 0:(nrow(sw_data$nodes) - 1),
  label = sw_data$nodes$name,
  value = sw_data$nodes$value,
  color = sw_data$nodes$colour,
  stringsAsFactors = FALSE
)

edges <- data.frame(
  from = sw_data$links$source,
  to = sw_data$links$target,
  width = sw_data$links$value / 5,
  stringsAsFactors = FALSE
)

# Just picking an arbitrary degree threshold for special
# character of certain nodes
degree_threshold <- 50

nodes <- nodes %>%
  mutate(
    shape = ifelse(value > degree_threshold, "icon", "dot"),
    icon.face = ifelse(value > degree_threshold, "FontAwesome", NA),
    # CRITICAL FIX: Supply the hex code as a plain string, without the "\u"
    icon.code = ifelse(value > degree_threshold, "f007", NA), 
    icon.color = ifelse(value > degree_threshold, color, NA),
    size = value / 3, 
    title = paste0("<p><b>", label, "</b><br>Interactions: ", value, "</p>")
  )
  
# Removing isolates
nodes <- nodes %>% filter(id %in% edges$from | id %in% edges$to)

starwars.net<-visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), 
             nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE, tooltipDelay = 100) %>%
  visPhysics(stabilization = FALSE) %>%
  visNodes(font = list(color = "#333333", size = 14, strokeWidth = 2, strokeColor = "white")) %>%
  addFontAwesome()

# Writing out for the blog  
visSave(starwars.net, file = "starwars.html", selfcontained = TRUE)
