####################### Folder Structure Functions ############################ ----
# This section contains all the functions that call the txt files that contain
# the folder structures for the aircraft - sensor combinations. The txt files are
# converted in .bat files and executed in the windows shell, finally the .bat file
# is erased.
################################################################################----
# Read Flight TXT structure depending on configuration UAV-Sensor
GetSetup <- function(Root, SetUp){                                              
  
  # Set main path to read the folder structures 
  BaseLoc <- paste0(Root,"\\www\\0_FolderStructures\\")
  
  # Read Flight TXT structure depending on configuration UAV-Sensor   
  Structure <- noquote(readLines(paste0(BaseLoc,SetUp,".txt"),warn=FALSE))
  
  # Return the loaded txt file
  return(Structure)
}
################################################################################
### Create Folder structure based on root, name, setup and standard name
FillMetadata <- function(Root, TargetLoc, FlightsDF, MisName, IndexStart=1){
  
  for(i in 1:nrow(FlightsDF)){
    
    FlightPath <- paste0(TargetLoc,
                         "\\",
                         IndexStart+i,
                         "_",
                         FlightsDF$AirCraft[i],
                         FlightsDF$Sensor[i],
                         "\\3_FlightFiles\\0_Log\\")
    
    #print(FlightPath)
    
    # Read Log Structure 
    LogFile <- noquote(readLines(paste0(Root,"\\www\\5_LogStructure\\FlightLog.txt")))
    
    # Replace fields
    LogFile[grep('* Project Location:', LogFile)] <- paste0('* Project Location: ',FlightsDF$RootLoc[i])
    LogFile[grep('* Mission Name:'    , LogFile)] <- paste0('* Mission Name:     ',FlightsDF$MisName[i])
    LogFile[grep('* Pilot:'           , LogFile)] <- paste0('* Pilot:            ',FlightsDF$Pilot[i])
    LogFile[grep('* Copilot:'         , LogFile)] <- paste0('* Copilot:          ',FlightsDF$Copilot[i])
    LogFile[grep('* Date of creation:', LogFile)] <- paste0('* Date of creation: ',FlightsDF$DateC[i])
    LogFile[grep('* Date of Flight:'  , LogFile)] <- paste0('* Date of Flight:   ',FlightsDF$DateF)
    LogFile[grep('* Aircraft:'        , LogFile)] <- paste0('* Aircraft:         ',FlightsDF$AirCraft[i])
    LogFile[grep('* Sensor:'          , LogFile)] <- paste0('* Sensor:           ',FlightsDF$Sensor[i])
    LogFile[grep('PlatformLogger'     , LogFile)+2] <- paste0("->",FlightsDF$LogText[i])
     
    # Update Log file
    write.table(LogFile, file = paste0(FlightsDF$LogLoc[i],"FlightLog.md"), sep="",
                row.names = FALSE, col.names = FALSE,  quote = FALSE)
  #   
  # 
  #   # Save GPKG file with ,modified or imported polygon
  #   if(!is.na(FlightsDF$geometry)){
  #     st_write(GeneratePol(FlightsDF$geometry),
  #              paste0(FlightPath,"\\AOI.gpkg"),
  #              delete_layer=TRUE
  #              )
  # }
     
  }
}
################################################################################
### Create Mission folder structure based on root, name, setup and standard name
CreateFolder <- function(Root, TargetLoc, MainStructure, FlightsDF, MisName, IndexStart){
  
  # Change directory to Target location
  setwd(TargetLoc)
  
  # Create Bat File with modified structure
  write.table(MainStructure, file = "Temporal.bat", sep="",
              row.names = FALSE, col.names = FALSE,  quote = FALSE)
  
  # Call system console, execute bat file and delete it
  shell.exec("Temporal.bat")
  Sys.sleep(1)
  file.remove("Temporal.bat")
  
  FillMetadata(Root, TargetLoc, FlightsDF, MisName, IndexStart)
  
  setwd(Root)

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
