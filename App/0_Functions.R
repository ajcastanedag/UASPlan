####################### Folder Structure Functions ############################ ----
# This section contains all the functions that call the txt files that contain
# the folder structures for the aircraft - sensor combinations. The txt files are
# converted in .bat files and executed in the windows shell, finally the .bat file
# is erased.
################################################################################
### Create Folder structure based on root name, sensor combination and standard name
CreateFolder <- function(Root, TargetLoc, MissionName, SetUp, LogDat, Aoi_Pol){
  # Change directory to Target location
  setwd(TargetLoc)
  # Read TXT structure depending on configuration UAV-Sensor
  if(SetUp == "DJIM300Altum"){
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\DJIM300Altum.txt")))
  } else if(SetUp == "DJIM300MXDual"){
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\DJIM300MXDual.txt")))
  } else if(SetUp == "DJIM300H20T") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\DJIM300H20T.txt")))
  } else if(SetUp == "DJIM300L1") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\DJIM300L1.txt")))
  } else if(SetUp == "DJIM600Altum") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\DJIM600Altum.txt")))
  } else if(SetUp == "DJIM600MXDual") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\DJIM600MXDual.txt")))
  } else if(SetUp == "DJIM600LiAirV") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\DJIM600LiAirV.txt")))
  } else if(SetUp == "Phantom4RGB") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\Phantom4RGB.txt")))
  } else if(SetUp == "WingtraAltum") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\WingtraAltum.txt")))
  } else if(SetUp == "WingtraRX1RII") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\WingtraRX1RII.txt")))
  } else(print("ERROR"))
  
  # Modify the line that contains foldername= and add the dinamic values
  Structure[grep('foldername=', Structure)] <- paste0(Structure[grep('set foldername=', Structure)],MissionName)
  
  # Create Bat File with modified structure
  write.table(Structure, file = "Temporal.bat", sep="",
              row.names = FALSE, col.names = FALSE,  quote = FALSE)
  
  # Call system console, execute bat file and delete it
  shell.exec("Temporal.bat")
  Sys.sleep(1)
  file.remove("Temporal.bat")
  
  # Add log information to created text file "FlightLog.txt"
  setwd(paste0(TargetLoc,"\\",MissionName,"\\4_FlightFiles\\"))
  fileConn<-file("FlightLog.txt")
  writeLines(LogDat, fileConn)
  close(fileConn)
  
  # Save GPKG file with ,modified or imported polygon
  if(!is.null(Aoi_Pol)){
    st_write(Aoi_Pol,
             paste0(TargetLoc,"\\",MissionName,"\\4_FlightFiles\\AOI.gpkg"),
             delete_layer=TRUE
             )
    
  }
}
################################################################################

ModPolToSf <- function(Pol){
  
  Coor <- unlist(Pol$features[[1]]$geometry$coordinates)
  
  Longitude<-Coor[seq(1,length(Coor), 2)] 
  Latitude<-Coor[seq(2,length(Coor), 2)]
  
  Df <- data.frame(lon = Longitude,
                   lat = Latitude)
  
  sf <- sfheaders::sf_polygon(
    obj = Df
    , x = "lon"
    , y = "lat"
  )
  sf::st_crs( sf ) <- 4326
  
  return(sf)
  

}
################################################################################
# library(leaflet.extras)
# library(shiny)
# 
# ui <- leafletOutput("leafmap")
# 
# server <- function(input, output, session) {
#   output$leafmap <- renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%
#       addDrawToolbar(editOptions = editToolbarOptions())
#   })
#   
#   # Start of Drawing
#   observeEvent(input$leafmap_draw_start, {
#     print("Start of drawing")
#     print(input$leafmap_draw_start)
#   })
#   
#   # Stop of Drawing
#   observeEvent(input$leafmap_draw_stop, {
#     print("Stopped drawing")
#     print(input$leafmap_draw_stop)
#   })
#   
#   # New Feature
#   observeEvent(input$leafmap_draw_new_feature, {
#     print("New Feature")
#     print(input$leafmap_draw_new_feature)
#   })
#   
#   # Edited Features
#   observeEvent(input$leafmap_draw_edited_features, {
#     print("Edited Features")
#     print(input$leafmap_draw_edited_features)
#   })
#   
#   # Deleted features
#   observeEvent(input$leafmap_draw_deleted_features, {
#     print("Deleted Features")
#     print(input$leafmap_draw_deleted_features)
#   })
#   
#   # We also listen for draw_all_features which is called anytime
#   # features are created/edited/deleted from the map
#   observeEvent(input$leafmap_draw_all_features, {
#     print("All Features")
#     print(input$leafmap_draw_all_features)
#   })
# }
# 
# shinyApp(ui, server)

