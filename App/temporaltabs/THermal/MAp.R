library(exifr)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(pbapply)

Wd <- "/home/cowboybebop/Documents/ShareRemina/6_Thermal60m_DJIM300H20T/0_Images/W/"

image_files <- list.files(Wd, pattern = "\\.(JPG|jpeg|png)$", full.names = TRUE, ignore.case = TRUE)

# Function to extract GPS data from a single image
extract_gps_data <- function(image_path) {
  exif_data <- read_exif(image_path)
  gps_data <- exif_data %>% 
    select(SourceFile, GPSLatitude, GPSLongitude, GPSAltitude, AmbientTemperature, Reflection, RelativeHumidity) %>% 
    filter(!is.na(GPSLatitude) & !is.na(GPSLongitude))
  return(gps_data)
}

# Extract GPS data from all images
gps_data_list <- pblapply(image_files, extract_gps_data)
gps_data <- bind_rows(gps_data_list)

# Define a color palette based on altitude
palette <- colorNumeric(palette = brewer.pal(9, "YlOrRd"), domain = gps_data$GPSAltitude, na.color = "transparent")

# Create the base map layers
satellite <- providers$Esri.WorldImagery
dark <- providers$CartoDB.DarkMatter

# Create a Leaflet map with layers control
map <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(satellite, group = "Satellite") %>%
  addProviderTiles(dark, group = "Dark") %>%
  addScaleBar(position = "bottomleft") %>%
  addCircleMarkers(data = gps_data,
                   lat = ~GPSLatitude,
                   lng = ~GPSLongitude,
                   color = ~palette(GPSAltitude),
                   radius = 5,
                   stroke = FALSE, # No border
                   fillOpacity = 0.8,
                   popup = ~paste("Altitude:", GPSAltitude, "m")) %>%
  addLegend(pal = palette, 
            values = gps_data$GPSAltitude, 
            title = "Altitude (m)", 
            position = "topright") %>%
  addLayersControl(
    position = "topleft",
    baseGroups = c("OpenStreetMap", "Satellite", "Dark"),
    options = layersControlOptions(collapsed = TRUE) # Set collapsed to TRUE
  ) %>%
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE, position = "bottomright")

# Print the map
map
