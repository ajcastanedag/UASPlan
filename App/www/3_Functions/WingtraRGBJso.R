setwd("/media/antonio/Sony/WingtraPilotProjects/Saaland_2_RGB Flight 01/DATA/")

library(leaflet)

# Load the JSON data
json_data <- jsonlite::fromJSON(list.files(getwd(),full.names = T))

# Extract coordinates from the JSON data
coordinates <- json_data$flights$geotag[[1]]$coordinate

# Convert the list to a data frame
df <- as.data.frame(do.call(rbind, coordinates))

lat <- as.numeric(df[,1])
lng <- as.numeric(df[,2])

json_data2 <- jsonlite::fromJSON("/media/antonio/Sony/WingtraPilotProjects/Saaland_3_RGB Flight 01/DATA/Saaland_3_RGB Flight 01.json")
# Extract coordinates from the JSON data
coordinates2 <- json_data2$flights$geotag[[1]]$coordinate

# Convert the list to a data frame
df2 <- as.data.frame(do.call(rbind, coordinates2))

lat2 <- as.numeric(df2[,1])
lng2 <- as.numeric(df2[,2])

json_data3 <- jsonlite::fromJSON("/media/antonio/Sony/WingtraPilotProjects/Saaland_3_RGB Flight 01_NEW512380/DATA/Saaland_3_RGB Flight 01_NEW512380.json")
# Extract coordinates from the JSON data
coordinates3 <- json_data3$flights$geotag[[1]]$coordinate

# Convert the list to a data frame
df3 <- as.data.frame(do.call(rbind, coordinates3))

lat3 <- as.numeric(df3[,1])
lng3 <- as.numeric(df3[,2])


# Create the leaflet map
leaflet() %>%
  setView(lng = lng[1], lat = lat[1], zoom = 13) %>%
  addTiles() %>%
  # Add markers with different colors
  addCircleMarkers(
    lng = lng2, lat = lat2,
    color = "blue",   # Change the color for the first set of markers
    radius = 6
  ) %>%
  addCircleMarkers(
    lng = lng3, lat = lat3,
    color = "red",    # Change the color for the second set of markers
    radius = 6
  )

