####################### Folder Structure Functions ############################ ----
# This section contains all the functions that call the txt files that contain
# the folder structures for the aircraft - sensor combinations. The txt files are
# converted in .bat files and executed in the windows shell, finally the .bat file
# is erased.
################################################################################
### Create Folder structure based on root, name, setup and standard name
CreateFolder <- function(Root, TargetLoc, MissionName, SetUp, LogDat, Pol){
  # Change directory to Target location
  setwd(TargetLoc)
  # Read TXT structure depending on configuration UAV-Sensor                    ----
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
  #####
  # Modify the line that contains foldername= and add the dynamic values
  Structure[grep('foldername=', Structure)] <- paste0(Structure[grep('set foldername=', Structure)],MissionName)
  
  print(Structure)
  
  # Create Bat File with modified structure
  write.table(Structure, file = "Temporal.bat", sep="",
              row.names = FALSE, col.names = FALSE,  quote = FALSE)
  
  # Call system console, execute bat file and delete it
  shell.exec("Temporal.bat")
  Sys.sleep(1)
  file.remove("Temporal.bat")
  
  # # Add log information to created text file "FlightLog.txt"
  # setwd(paste0(TargetLoc,"\\",MissionName,"\\3_FlightFiles\\0_Log\\"))
  # 
  # # Fill basic information in Log File
  # MakeLog(Root, LogDat)
  # 
  # # Save GPKG file with ,modified or imported polygon
  # if(!is.null(Pol)){
  #   st_write(GeneratePol(Pol),
  #            paste0(TargetLoc,"\\",MissionName,"\\3_FlightFiles\\0_Log\\AOI.gpkg"),
  #            delete_layer=TRUE
  #            )
  #   
  # }
}
################################################################################
### Transform leaflet mods on AOI into new polygon (SF)
ModPolToSf <- function(Pol, New=F){
  
  if(New == F){
    # Un-list coordinates from new object
    Coor <- unlist(Pol$features[[1]]$geometry$coordinates)
  } else if(New == T){
    Coor <- unlist(Pol$geometry$coordinates)
  }
  
  
  # Create data frame to store pair of coordinates
  Df <- data.frame(lon = Coor[seq(1,length(Coor), 2)],
                   lat = Coor[seq(2,length(Coor), 2)])
  
  # Create SF object using sfheaders
  sf <- sfheaders::sf_polygon(
    obj = Df
    , x = "lon"
    , y = "lat"
  )
  
  # Assign coordinate system
  sf::st_crs( sf ) <- 4326
  
  return(sf)
}
################################################################################
# Transform geometries from DT into sf objects to export individually
GeneratePol <- function(GeomSF){
  # Aoi_Arr[[2]]
  SfObj <- st_geometry(GeomSF) %>% st_sf()
  
  # Assign coordinate system
  sf::st_crs( SfObj ) <- 4326
  
  return(SfObj)
}
################################################################################
#
MakeLog <- function(Root, LogDat){
  
  LogFile <- noquote(readLines(paste0(Root,"\\LogStructure\\FlightLog.txt")))
  
  LogFile[grep('* Project Location:', LogFile)] <- paste0('* Project Location: ',LogDat[1])
  LogFile[grep('* Mission Name:'    , LogFile)] <- paste0('* Mission Name:     ',LogDat[2])
  LogFile[grep('* Pilot:'           , LogFile)] <- paste0('* Pilot:            ',LogDat[3])
  LogFile[grep('* Copilot:'         , LogFile)] <- paste0('* Copilot:          ',LogDat[4])
  LogFile[grep('* Date of creation:', LogFile)] <- paste0('* Date of creation: ',as.character(Sys.Date()))
  LogFile[grep('* Date of Flight:'  , LogFile)] <- paste0('* Date of Flight:   ',LogDat[5])
  LogFile[grep('* Aircraft:'        , LogFile)] <- paste0('* Aircraft:         ',LogDat[6])
  LogFile[grep('* Sensor:'          , LogFile)] <- paste0('* Sensor:           ',LogDat[7])
  LogFile[grep('PlatformLogger'     , LogFile)+2] <- paste0("->",LogDat[8])
  
  # Update Log file 
  write.table(LogFile, file = paste0("FlightLog_",LogDat[2],".md"), sep="",
              row.names = FALSE, col.names = FALSE,  quote = FALSE)
  

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






