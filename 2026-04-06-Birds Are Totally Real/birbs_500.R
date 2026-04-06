library(tidyverse)
library(lubridate)
library(leaflet)
library(patchwork)
library(htmlwidgets)
library(ggpattern)
library(geomtextpath)

# Okabe-Ito colorblind-friendly palette 
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Load the data
ebird_data <- read_csv("MyEBirdData.csv")

# Clean and format the dates AND fix the "X" counts
# There are some estimates of the X counts in the notes, but
# here I'm gonna say if any X then assume 1 bird
ebird_data_clean <- ebird_data %>%
  mutate(Date = ymd(Date),
         Month = month(Date, label = TRUE, abbr = TRUE),
         Count_Num = as.numeric(ifelse(Count == "X", 1, Count)))

# Frequency and Abundance (Birds I've met along the way)

# Licensed from nounproject: https://thenounproject.com/icon/5651704/
bf <- "blue-feather.png"
of <- "orange-feather.png"

total_checklists <- n_distinct(ebird_data_clean$`Submission ID`)

top_frequency <- ebird_data_clean %>%
  group_by(`Common Name`) %>%
  summarize(Appearances = n_distinct(`Submission ID`)) %>%
  mutate(Frequency = (Appearances / total_checklists) * 100) %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 10)

plot_freq <- ggplot(top_frequency, aes(x = reorder(`Common Name`, Frequency), y = Frequency)) +
  geom_col_pattern(
    pattern = "image",
    pattern_filename = bf,
    pattern_type = "squish",          
    fill = "transparent",             
    color = NA,                       
    width = 0.7                      
  ) + 
  coord_flip() + 
  labs(title = "Most Frequent",
       subtitle = "Percentage of checklists species was present",
       x = NULL, 
       y = "Frequency (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank() 
  )

top_abundance <- ebird_data_clean %>%
  group_by(`Common Name`) %>%
  summarize(Total_Individuals = sum(Count_Num, na.rm = TRUE)) %>%
  arrange(desc(Total_Individuals)) %>%
  slice_head(n = 10)

plot_abund <- ggplot(top_abundance, aes(x = reorder(`Common Name`, Total_Individuals), y = Total_Individuals)) +
  geom_col_pattern(
    pattern = "image",
    pattern_filename = of,
    pattern_type = "squish",
    fill = "transparent",
    color = NA,
    width = 0.7
  ) + 
  coord_flip() + 
  labs(title = "Most Abundant",
       subtitle = "Total number of individuals encountered",
       x = NULL, 
       y = "Total Individuals") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )

combined_plot <- plot_freq + plot_abund +
  plot_annotation(
    title = "Birds I met along the way: Frequency and Abundance",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  )

print(combined_plot)

#Lifers
lifers <- ebird_data_clean %>%
  group_by(`Common Name`) %>%
  summarize(First_Seen = min(Date)) %>%
  arrange(First_Seen) %>%
  mutate(Cumulative_Species = row_number())

y_40 <- as.Date("2021-12-10")
label_height <- max(lifers$Cumulative_Species) * 0.9

plot_3 <- ggplot(lifers, aes(x = First_Seen, y = Cumulative_Species)) +
  geom_step(color = okabe_ito[6], linewidth = 1.2) + 
  geom_vline(xintercept = y_40, linetype = "dashed", color = okabe_ito[5], linewidth = 0.8) +
  annotate("text", x = y_40, y = label_height, 
           label = "Year I turned 40", hjust = 1.1, fontface = "italic", color = okabe_ito[5], size = 4.5) +
  labs(title = "Species Accumulation Over Time",
       subtitle = "This is my cumulative life list",
       x = "Date",
       y = "Total Unique Species") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16))

print(plot_3)


# Birbs of the world
location_summary <- ebird_data_clean %>%
  group_by(Location, Latitude, Longitude) %>%
  summarize(Checklists = n_distinct(`Submission ID`),
            Species = n_distinct(`Common Name`),
            .groups = "drop")

bird_map <- leaflet(location_summary) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(
    lng = ~Longitude, 
    lat = ~Latitude,
    radius = ~ifelse(Checklists > 10, 8, 4), 
    color = okabe_ito[3], 
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste("<b>Location:</b>", Location, "<br>",
                   "<b>Checklists:</b>", Checklists, "<br>",
                   "<b>Total Species:</b>", Species)
  )

# Display the map widget
print(bird_map)

# Save the map as a standalone HTML file
saveWidget(bird_map, file = "bird_map.html", selfcontained = TRUE)
