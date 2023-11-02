library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(sf)
library(sp)
library(htmlwidgets)
#####################################################################################################################

kml_files <- list.files(path = "/home/antonio/Laufwerk-M/04-Phil/Fernerkundung1/UAS/WebShare/viewer/beta4/data/",
                        pattern = ".kml",
                        full.names = T)


kml_combined <-  st_read(kml_files[1]) %>% st_zm() %>% as("Spatial")

# for(i in 2:length(kml_files)){
#   kml_sf <-  st_read(kml_files[i]) %>% st_zm() %>% as("Spatial")
#   kml_combined <- rbind(kml_combined, kml_sf)
# }

kml_combined <-  st_read(kml_files) %>% st_zm() %>% as("Spatial")
kml_combined_sf <- st_as_sf(kml_combined)
kml_combined_sf <- kml_combined_sf[kml_combined_sf$Region == "UniversityForest",]

#####################################################################################################################
#####################################################################################################################
Path <- "/home/antonio/UASNAS/UniversityForest_AJCGPHD/2023_10_25_UniWaldM/"
PathIMG <- paste0(Path, "0_Flights/")
Batch <- Path2DF(PathIMG, "_1.tif")
AvailableDates <- format(as.POSIXct(Batch$DateTime, format = "%Y:%m:%d %H:%M:%S"), "%Y-%m-%d")
AvailableDates <- as.factor(AvailableDates)
date_groups <- split(Batch, AvailableDates)
#####################################################################################################################
# Create a new field Batch$Group and assign group names
for (i in seq_along(date_groups)) {
  date_groups[[i]]$Group <- paste0("Group", i)
}

# Combine the data frames back into a single data frame
Batch <- do.call(rbind, date_groups)

# Create a unique set of group names
unique_groups <- unique(Batch$Group)

# Create a color palette for the groups
colors <- colorFactor(
  palette = "Set1",  # You can choose a different palette
  domain = unique_groups
)

#######################################
# Convert to SF
Batch_sf <- st_as_sf(Batch, coords = c("Lon", "Lat"), crs = 4326)
Kml_Buff <- st_buffer(kml_combined_sf, dist =  20 )
intersections <- st_intersection(Batch_sf, Kml_Buff)
#######################################
# Create the leaflet map
Map <- leaflet() %>%
  setView(lng = 10.445, lat = 50.062, zoom = 15) %>%
  addTiles() %>%
  # Add Pols
  addPolygons(
    data = kml_combined,
    fillColor = "white", 
    fillOpacity = 0.2, 
    stroke = FALSE, 
    popup = ~Region) %>%
# Add markers with different colors
  addCircleMarkers(
    data = Batch,#[Batch$Group == "Group1",],
    lng = ~Lon, lat = ~Lat,
    color = ~colors(Group),  # Use the color palette for marker color
    radius = 0.5
  ) %>%
  addCircleMarkers(
    data = intersections,
    color = "red", 
    radius = 0.5
  ) 
  
Map

################################################################################
saveWidget(Map, file = paste0(Path,"/3_Media/Map.html"),selfcontained = T)

write.csv(intersections, file = paste0(Path,"/3_Media/Coverage/Images.csv"), row.names = FALSE)

# Create the ggplot2 plot
ggplot(Blue, aes(x = DateTime, y = ASL)) +
  geom_point() +  # Use geom_line to connect points with lines
  labs(x = "DateTime", y = "Altitude Above Sea Level (ASL)") + # Label axes
  theme_minimal()  # Use a minimal theme (you can customize the theme as needed)



################################################################################
################################################################################
################################################################################

Path2DF <- function(IMGPath, Pattern){
  
  DataFrame <- data.frame(
    Path = list.files(path = IMGPath, pattern = Pattern, recursive = T, full.names = T, ),
    Lat = NA,
    Lon = NA,
    ASL = NA,
    DateTime = NA
  )
  
  for(i in 1:length(DataFrame$Path)){
    print(i)
    Result <- GetExif(DataFrame$Path[i])
    DataFrame$Lat[i] <- as.numeric(Result[1])
    DataFrame$Lon[i] <- as.numeric(Result[2])
    DataFrame$ASL[i] <- as.numeric(Result[3])
    DataFrame$DateTime[i] <- Result[4]
  }
  
  return(DataFrame)
}

# Function to convert degrees, minutes, and seconds to decimal degrees
convert_to_decimal <- function(gps_str) {
  parts <- unlist(strsplit(gps_str, "[^0-9.]+"))
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  decimal <- degrees + minutes/60 + seconds/3600
  return(as.numeric(trimws((decimal))))
}

# Function to convert date Time to R format 
DateTime <- function(dt_str){
  DtString <- regmatches(dt_str, regexpr("\\d{4}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}", dt_str))
  return(DtString)
}

GetExif <- function(filepath){
  
  print(filepath)
  
  # Run exiftool to extract GPS information and capture the output
  exif_output <- system2("exiftool", args = c("-gpsposition","-gpsaltitude","-FileModifyDate", filepath), stdout = TRUE)
  
  # Split the GPS Position into latitude and longitude components
  parts <- strsplit(exif_output, ",")[[1]]
  latitude_str <- trimws(parts[1])
  longitude_str <- trimws(parts[2])
  
  Altitude <-  gsub("[^0-9.]", "", exif_output[[2]])
  
  DateTime_str <- DateTime(exif_output[[3]])
  
  # Remove "GPS Position:" prefix
  latitude_str <- sub("GPS Position *: *", "", latitude_str)
  longitude_str <- sub("GPS Position *: *", "", longitude_str)
  
  # Extract latitude and longitude in decimal degrees
  latitude_decimal <- convert_to_decimal(latitude_str)
  longitude_decimal <- convert_to_decimal(longitude_str)
  
  return(c(latitude_decimal, longitude_decimal,Altitude, DateTime_str))
  
}




