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


    # Save GPKG file with ,modified or imported polygon
    if(!is.na(FlightsDF$geometry)){
      st_write(GeneratePol(FlightsDF$geometry[i]),
               paste0(FlightsDF$LogLoc[i],"\\AOI.gpkg"),
               delete_layer=TRUE
               )
  }
     
  }
}
#################################################################################################################################################################### 
CreateFolder <- function(dir_path, selected_system, Name) {
  ############################################################################## DCG50
  if (selected_system == "LiBackpack") {
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create subdirectories 0_Base, 1_BagFiles, 2_Video, 3_Process, 4_Export
      for (i in 0:4) {
        sub_dir <- file.path(project_dir, paste0(i, "_", c("Base", "BagFiles", "Video", "Process", "Export")[i + 1]))
        dir.create(sub_dir)
        
        if (i == 2) {
          # Create subdirectories 1_Raw and 2_Stitched inside 2_Video
          dir.create(file.path(sub_dir, "1_Raw"))
          dir.create(file.path(sub_dir, "2_Stitched"))
        }
      }
      
      # Create a text file inside 4_Export
      text_file <- file.path(project_dir, "4_Export", "example.txt")
      file.create(text_file)
      
      return(TRUE)  # Success
    } else {
      return(FALSE)  # Folder structure already exists
    }
  }
  ############################################################################## All ALTUM or MX-Dual
  if (grepl("Altum",selected_system) || grepl("MXDual",selected_system) || grepl("3TAgisoft",selected_system) || grepl("3MAgisoft",selected_system)) {
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create subdirectories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_Images", "1_Agisoft", "2_Reports", "3_FlightFiles", "4_RawOutput")
      
      # Create subdirectories inside 3_FlightFiles
      if ("3_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_Log", "1_Plan", "2_Other")
      }
      
      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)
        
        # Create subdirectories inside 3_FlightFiles
        if (sub_dir_name == "3_FlightFiles") {
          for (flight_subdir_name in flight_files_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
        }
      }
      
      return(TRUE)  # Success
    } else {
      return(FALSE)  # Folder structure already exists
    }
  }
  ############################################################################# All RGB
  if (grepl("RGB",selected_system)) {
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create subdirectories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_Images", "1_Agisoft", "2_Reports", "3_FlightFiles", "4_RawOutput")
      
      # Create subdirectories inside 3_FlightFiles
      if ("3_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_Log", "1_Plan", "2_Other")
      }
      
      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)
        
        # Create subdirectories inside 3_FlightFiles
        if (sub_dir_name == "3_FlightFiles") {
          for (flight_subdir_name in flight_files_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
        }
      }
      
      return(TRUE)  # Success
    } else {
      return(FALSE)  # Folder structure already exists
    }
  }
  #############################################################################  LiAirV
  if (grepl("LiAirV",selected_system)) {
    
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create subdirectories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_BaseStation", "1_LiAirVData", "2_Reports", "3_FlightFiles", "4_RawOutput")
      
      # Create subdirectories inside 3_FlightFiles
      if ("3_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_Log", "1_Plan", "2_Other")
      }
      
      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)
        
        # Create subdirectories inside 3_FlightFiles
        if (sub_dir_name == "3_FlightFiles") {
          for (flight_subdir_name in flight_files_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
        }
      }
      
      return(TRUE)  # Success
    } else {
      return(FALSE)  # Folder structure already exists
    }
  }
  #############################################################################  L1
  if (grepl("L1",selected_system)) {
    
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create subdirectories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_TerraFiles", "1_TerraResults", "2_Reports", "3_FlightFiles", "4_RawOutput")
      
      # Create subdirectories inside 3_FlightFiles
      if ("3_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_Log", "1_Plan", "2_Other")
      }
      
      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)
        
        # Create subdirectories inside 3_FlightFiles
        if (sub_dir_name == "3_FlightFiles") {
          for (flight_subdir_name in flight_files_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
        }
      }
      
      return(TRUE)  # Success
    } else {
      return(FALSE)  # Folder structure already exists
    }
  }
  #############################################################################  H20T M3T
  if (grepl("H20T",selected_system) || grepl("3TTerra",selected_system) || grepl("3MTerra",selected_system)) {
    
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create subdirectories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_TerraFiles", "1_Images", "2_Agisoft", "3_Reports", "4_FlightFiles", "5_RawOutput")
      
      # Create subdirectories inside 3_FlightFiles
      if ("4_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_Log", "1_Plan", "2_Other")
      }
      
      # Create subdirectories inside 3_FlightFiles
      if ("1_Images" %in% subdirectories) {
        Images_subdirs <- c("Thermal", "Thermal_Cal", "RGB")
      }
      
      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)
        
        # Create subdirectories inside FlightFiles
        if (sub_dir_name == "4_FlightFiles") {
          for (flight_subdir_name in flight_files_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
        }
        
        # Create subdirectories inside 3_FlightFiles
        if (sub_dir_name == "1_Images") {
          for (Images_name in Images_subdirs) {
            image_sub_dir <- file.path(sub_dir, Images_name)
            dir.create(image_sub_dir)
          }
        }
      }
      
      return(TRUE)  # Success
    } else {
      return(FALSE)  # Folder structure already exists
    }
  }
  
}
#################################################################################################################################################################### 
# ### Create Mission folder structure based on root, name, setup and standard name
# CreateFolder <- function(Root, TargetLoc, MainStructure, FlightsDF, MisName, IndexStart){
#   
#   # Change directory to Target location
#   setwd(TargetLoc)
#   
#   # Create Bat File with modified structure
#   write.table(MainStructure, file = "Temporal.bat", sep="",
#               row.names = FALSE, col.names = FALSE,  quote = FALSE)
#   
#   # Call system console, execute bat file and delete it
#   shell.exec("Temporal.bat")
#   Sys.sleep(1)
#   file.remove("Temporal.bat")
#   
#   FillMetadata(Root, TargetLoc, FlightsDF, MisName, IndexStart)
#   
#   setwd(Root)
# 
# }
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
