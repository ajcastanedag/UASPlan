library(exifr)
library(dplyr)
library(leaflet)


AvailableFiles <- list.files(path = "/media/antonio/MicaSense/",pattern = "_1.tif",recursive = T,full.names = T, )


DataFrame <- data.frame(
  Path = list.files(path = "/media/antonio/MicaSense/",pattern = "_1.tif",recursive = T,full.names = T, ),
  Lat = NA,
  Lon = NA
)


for(i in 1:length(DataFrame$Path)){
  print(i)
  Result <- GetCoordinates(DataFrame$Path[i])
  DataFrame$Lat[i] <- Result[1]
  DataFrame$Lon[i] <- Result[2]
}

# Create the leaflet map
leaflet() %>%
  setView(lng = DataFrame$Lon[1], lat = DataFrame$Lat[1], zoom = 13) %>%
  addTiles() %>%
  # Add markers with different colors
  addCircleMarkers(
    lng = DataFrame$Lon, lat = DataFrame$Lat,
    color = "blue",   # Change the color for the first set of markers
    radius = 6
  )

################################################################################33

# Function to convert degrees, minutes, and seconds to decimal degrees
convert_to_decimal <- function(gps_str) {
  parts <- unlist(strsplit(gps_str, "[^0-9.]+"))
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  decimal <- degrees + minutes/60 + seconds/3600
  return(decimal)
}


GetCoordinates <- function(filepath){
  
  # Run exiftool to extract GPS information and capture the output
  exif_output <- system2("exiftool", args = c("-gpsposition", filepath), stdout = TRUE)
  
  
  # Split the GPS Position into latitude and longitude components
  parts <- strsplit(exif_output, ",")[[1]]
  latitude_str <- trimws(parts[1])
  longitude_str <- trimws(parts[2])
  
  # Remove "GPS Position:" prefix
  latitude_str <- sub("GPS Position *: *", "", latitude_str)
  longitude_str <- sub("GPS Position *: *", "", longitude_str)
  
  # Extract latitude and longitude in decimal degrees
  latitude_decimal <- convert_to_decimal(latitude_str)
  longitude_decimal <- convert_to_decimal(longitude_str)
  
  return(c(latitude_decimal, longitude_decimal))
  
  
  
}




