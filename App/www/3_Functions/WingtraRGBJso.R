library(leaflet)
library(jsonlite)
library(sf)

setwd("/home/antonio/Laufwerk-M/04-Phil/Fernerkundung1/UAS/ProjectInformation/UniWald/MIssions/")

FLights <- list.files("/home/antonio/Laufwerk-M/04-Phil/Fernerkundung1/UAS/ProjectInformation/UniWald/MIssions/", full.names = T)

# Load the JSON data
json_data <- jsonlite::fromJSON(FLights[2])

# Area location
Area <- match("area", json_data$flightPlan$items$complexItemType)

# Extract coordinates from the JSON data
coordinates <- json_data$flightPlan$items$polygon[[Area]]
  
# Convert the list to a data frame
df <- data.frame("Lat" = coordinates[,1],"Lon" = coordinates[,2])

# Add the first point to close the polygon
df <- rbind(df, df[1, ])

# Combine Lon and Lat into a matrix
coords <- cbind(df$Lon, df$Lat)

# Create a polygon from the coordinates
polygon <- st_polygon(list(coords)) %>% st_sfc( crs = 4326)

# Create the leaflet map
leaflet() %>%
  setView(lng = lng[1], lat = lat[1], zoom = 13) %>%
  addTiles() %>%
  # Add markers with different colors
  addPolygons(data = polygon, fillColor = "blue", fillOpacity = 0.5)
