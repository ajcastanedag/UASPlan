####################### Folder Structure Functions ############################ ----
# This section contains all the functions that call the txt files that contain
# the folder structures for the aircraft - sensor combinations. The txt files are
# converted in .bat files and executed in the windows shell, finally the .bat file
# is erased.
################################################################################ Create Mission function ----
CreateMission <- function(dir_path, Name) {
  
  # Create the main directory
  project_dir <- file.path(dir_path, Name)
  if (!dir.exists(project_dir)) {
    dir.create(project_dir)
    
    # Create subdirectories 
    for (i in 0:3) {
      sub_dir <- file.path(project_dir, paste0(i, "_", c("Flights", "Analysis", "Results", "Media")[i + 1]))
      dir.create(sub_dir)
      
      if (i == 2) {
        # Create subdirectories 1_Raw and 2_Stitched inside 2_Video
        create_qgis_project(paste0(sub_dir,"/"), Name)
        dir.create(file.path(sub_dir, "0_Raster"))
        dir.create(file.path(sub_dir, "1_Pointclouds"))
        dir.create(file.path(sub_dir, "2_Vectors"))
      }
    }
  }
  
}
################################################################################ Create Folder structures ---- 
CreateFolder <- function(dir_path, DataFrame) {
  
  #print(paste0(dir_path," - ",selected_system," - ",Name))
  selected_system <- paste0(DataFrame$AirCraft, DataFrame$Sensor)
  Name <- DataFrame$FlightName
  
  ############################################################################## LiBackpack
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
      text_file <- file.path(project_dir, "4_Export", "metadata.md")
      file.create(text_file)
      
      # Fill metadata on metadata.md
      FillMetadata(text_file, DataFrame)
      
      return(TRUE)  # Success
    } else {
      return(FALSE)  # Folder structure already exists
    }
  }
  ############################################################################## All ALTUM or MX-Dual with Agisoft
  if (grepl("Altum",selected_system) || grepl("AltumPT",selected_system) || grepl("MXDual",selected_system) || grepl("3TAgisoft",selected_system) || grepl("3MAgisoft",selected_system)) {
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create sub directories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_Images", "1_Agisoft", "2_Reports", "3_FlightFiles", "4_RawOutput")
      
      # Create subdirectories inside 3_FlightFiles
      if ("3_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_Log", "1_Plan", "2_Other")
      
      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)
        
        # Create subdirectories inside 3_FlightFiles
        if (sub_dir_name == "3_FlightFiles") {
          
          # Create a text file inside 4_Export
          text_file <- file.path(sub_dir, "metadata.md")
          file.create(text_file)
          
          # Fill metadata on metadata.md
          FillMetadata(text_file, DataFrame)
          
          for (flight_subdir_name in flight_files_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
        }

        # Create subdirectories inside 0_Images for Thermal
        if (sub_dir_name == "0_Images" && grepl("3TAgisoft",selected_system)) {
          
          Thermal_subdirs <- c("0_RGB", "1_Thermal", "3_ThermalCal")
          
          for (flight_subdir_name in Thermal_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
          
        }
        
        # Create subdirectories inside 0_Images for Thermal
        if (sub_dir_name == "0_Images" && grepl("3MAgisoft",selected_system)) {
          
          M3M_subdirs <- c("0_RGB", "1_MS")
          
          for (flight_subdir_name in M3M_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
          
        }
        
      }
        
      }
        
      return(TRUE)  # Success
    } else {
      return(FALSE)  # Folder structure already exists
    }
  }
  ############################################################################## All RGB
  if (grepl("RGB",selected_system)  || grepl("RX1RII",selected_system)  || grepl("D2M",selected_system) ) {
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
          
          # Create a text file inside 4_Export
          text_file <- file.path(sub_dir, "metadata.md")
          file.create(text_file)
          
          # Fill metadata on metadata.md
          FillMetadata(text_file, DataFrame)
          
          for (flight_subdir_name in flight_files_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
        }
        
        # Create subdirectories inside 0_Images for D2M
        if (sub_dir_name == "0_Images" && grepl("D2M",selected_system)) {
          
          D2M_subdirs <- c("W", "A", "S","D","X")
          
          for (flight_subdir_name in D2M_subdirs) {
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
  ############################################################################## LiAirV
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
          
          # Create a text file inside 4_Export
          text_file <- file.path(sub_dir, "metadata.md")
          file.create(text_file)
          
          # Fill metadata on metadata.md
          FillMetadata(text_file, DataFrame)
          
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
  ############################################################################## Qube240
  if (grepl("Qube240",selected_system)) {
    
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create subdirectories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_YSData", "1_Base", "2_Reports", "3_FlightFiles", "4_RawOutput")
      
      # Create subdirectories inside 3_FlightFiles
      if ("3_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_TrinityLog", "1_Plan", "2_Other")
      }
      
      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)
        
        # Create subdirectories inside 3_FlightFiles
        if (sub_dir_name == "3_FlightFiles") {
          
          # Create a text file inside 4_Export
          text_file <- file.path(sub_dir, "metadata.md")
          file.create(text_file)
          
          # Fill metadata on metadata.md
          FillMetadata(text_file, DataFrame)
          
          for (flight_subdir_name in flight_files_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
          }
        }
        
        # Create subdirectories inside 2_Bases for YS
        if (sub_dir_name == "1_Base") {
          
          YSBase_subdirs <- c("0_Raw", "1_Proccessed")
          
          for (flight_subdir_name in YSBase_subdirs) {
            flight_sub_dir <- file.path(sub_dir, flight_subdir_name)
            dir.create(flight_sub_dir)
            
            # Create subsubdirectories inside 0_Raw or 1_Proccessed for YS * This might change
            if (flight_subdir_name == "0_Raw" || flight_subdir_name == "1_Proccessed") {
              
              YSBase_subsubdirs <- c("EMLID", "QBASE")

              for (flight_subsubdir_name in YSBase_subsubdirs) {
                flight_subsub_dir <- file.path(flight_sub_dir, flight_subsubdir_name)
                dir.create(flight_subsub_dir)
              }
            }
          }
        }
        
        # Create subdirectories inside 4_RawOutput for YS
        if (sub_dir_name == "4_RawOutput") {
          
          YSBase_subdirs <- c("0_PointCloud", "2_Raster")
          
          for (flight_subdir_name in YSBase_subdirs) {
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
  ############################################################################## NanoHP
  if (grepl("NanoHP",selected_system)) {

    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)

      # Create subdirectories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_Images", "1_NHProjects", "2_Reports", "3_FlightFiles", "4_RawOutput")

      # Create subdirectories inside 3_FlightFiles
      if ("3_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_GNSS", "1_Plan", "2_Other")
      }

      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)

        # Create subdirectories inside 3_FlightFiles
        if (sub_dir_name == "3_FlightFiles") {

          # Create a text file inside 4_Export
          text_file <- file.path(sub_dir, "metadata.md")
          file.create(text_file)

          # Fill metadata on metadata.md
          FillMetadata(text_file, DataFrame)

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
          
          # Create a text file inside 4_Export
          text_file <- file.path(sub_dir, "metadata.md")
          file.create(text_file)
          
          # Fill metadata on metadata.md
          FillMetadata(text_file, DataFrame)
          
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
  ############################################################################## H20T M3T with TERRA
  if (grepl("H20T",selected_system) || grepl("3TTerra",selected_system) || grepl("3MTerra",selected_system)) {
    
    # Create the main directory
    project_dir <- file.path(dir_path, Name)
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
      
      # Create subdirectories 0_Images, 1_Agisoft, 2_Reports, 3_FlightFiles, 4_RawOutput
      subdirectories <- c("0_TerraFiles", "1_Images", "2_Agisoft", "3_Reports", "4_FlightFiles", "5_RawOutput")
      
      # Create subdirectories inside 4_FlightFiles
      if ("4_FlightFiles" %in% subdirectories) {
        flight_files_subdirs <- c("0_Log", "1_Plan", "2_Other")
      }
      
      # Create subdirectories inside 3_FlightFiles
      if ("1_Images" %in% subdirectories) {
        Images_subdirs <- c("0_RGB", "1_Thermal", "3_ThermalCal")
      }
      
      for (sub_dir_name in subdirectories) {
        sub_dir <- file.path(project_dir, sub_dir_name)
        dir.create(sub_dir)
        
        # Create subdirectories inside FlightFiles
        if (sub_dir_name == "4_FlightFiles") {
          
          # Create a text file inside 4_Export
          text_file <- file.path(sub_dir, "metadata.md")
          file.create(text_file)
          
          # Fill metadata on metadata.md
          FillMetadata(text_file, DataFrame)
          
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
################################################################################ Fill metadata flights ----
FillMetadata <- function(text_file_location, FlightsDF) {
  # Extracting required fields from the FlightsDF dataframe
  project_location <- as.character("Fix")
  mission_name <- as.character(FlightsDF$FlightName[1])
  pilot <- as.character(FlightsDF$Pilot[1])
  copilot <- as.character(FlightsDF$Copilot[1])
  date_of_flight <- as.character(FlightsDF$DateF[1])
  aircraft <- as.character(FlightsDF$AirCraft[1])
  sensor <- as.character(FlightsDF$Sensor[1])
  platform_logger <- as.character(FlightsDF$LogText[1])
  
  # Creating the Markdown content
  markdown_content <- paste0("# Metadata\n",
                             "- **Project Location:** ", project_location, "\n",
                             "- **Mission Name:** ", mission_name, "\n",
                             "- **Pilot:** ", pilot, "\n",
                             "- **Copilot:** ", copilot, "\n",
                             "- **Date of Flight:** ", date_of_flight, "\n",
                             "- **Aircraft:** ", aircraft, "\n",
                             "- **Sensor:** ", sensor, "\n",
                             "- **Platform Logger:** ", platform_logger, "\n")

  # Writing the Markdown content to the text file
  writeLines(markdown_content, con = text_file_location)
}
################################################################################ Create QGis Project ----
create_qgis_project <- function(path, name) {
  # Create a basic QGIS project XML structure
  qgis_project <- '<?xml version="1.0" encoding="UTF-8"?>\n
  <Project projection="" version="3.10.0-Madeira">\n
    <!-- Add your layers, styles, and project settings here -->\n
  </Project>'
  
  # Construct the full file path for .qgs and .qgz files
  qgs_file <- paste0(path, "/", name, ".qgs")

  # Write the QGIS project XML to a .qgs file
  writeLines(qgis_project, qgs_file)
  
  # Return the path to the created .qgz file
  #return(qgs_file)
}
################################################################################ Get SDK functionm
getSDK <- function(Path){
  
  download.file("https://dl.djicdn.com/downloads/dji_thermal_sdk/20221108/dji_thermal_sdk_v1.4_20220929.zip", dest="SDK.zip", mode="wb") 
  unzip("SDK.zip", exdir = Path)
  file.remove("SDK.zip") 
  
  return()
}
################################################################################
ThermalCal <- function(sdk_dir, emissivity, humidity,distance,in_dir,out_dir){
  
  # short version for running commands in terminal
  run <- function(x) shell(x, intern=FALSE, wait=TRUE)

  # List of Files
  in_files = list.files(in_dir, full.names = T, pattern = "_T")
  
  ### calibration/conversion procedure (...could be paralleled)
  for(i in 1:length(in_files)){
    
    #Update status of bar with each image
    updateProgressBar(id = "TCalProgBar", value = i, total = length(in_files))

    # calibration to celsius
    in_exif = exifr::read_exif(in_files[i])
    in_name = in_files[i]
    out_name = paste0(out_dir, substr(basename(in_files[i]), 0, nchar(basename(in_files[i]))-4), ".raw")
    run(paste0(sdk_dir,"//utility/bin/windows/release_x64/dji_irp.exe -s ", in_name, " -a measure -o ", out_name, " --measurefmt float32",
               " --emissivity ", emissivity, " --humidity ", humidity, " --distance ", distance))

    # from .raw (hex) to .tif (celsius in float)
    raw_data <- readBin(out_name, "double", size = 4, n = in_exif$ImageWidth*in_exif$ImageHeight)
    image_matrix <- matrix(raw_data, nrow = in_exif$ImageHeight, ncol = in_exif$ImageWidth, byrow = T)
    out_name_tif = paste0(substr(out_name, 0, nchar(out_name)-4), ".tif")
    write_tif(image_matrix, path = out_name_tif, overwrite = TRUE)

    # transfer metadata (exif)
    exiftool_call(paste0("-Model=", in_exif$Model[1]), out_name_tif)
    exiftool_call(paste0("-Make=", in_exif$Make[1]), out_name_tif)
    exiftool_call(paste0("-Orientation=", in_exif$Orientation[1]), out_name_tif)
    exiftool_call(paste0("-FocalLength=", in_exif$FocalLength[1]), out_name_tif)
    exiftool_call(paste0("-FocalLengthIn35mmFormat=", in_exif$FocalLengthIn35mmFormat[1]), out_name_tif)
    exiftool_call(paste0("-DigitalZoomRatio=", in_exif$DigitalZoomRatio[1]), out_name_tif)
    exiftool_call(paste0("-ApertureValue=", in_exif$ApertureValue[1]), out_name_tif)
    exiftool_call(paste0("-GPSAltitude=", in_exif$GPSAltitude[1]), out_name_tif)
    exiftool_call(paste0("-GPSLatitude=", in_exif$GPSLatitude[1]), out_name_tif)
    exiftool_call(paste0("-GPSLongitude=", in_exif$GPSLongitude[1]), out_name_tif)
  }
  
  ### remove temp files
  file.remove(list.files(out_dir, recursive = TRUE, full.names = T, pattern = "_original"))
  file.remove(list.files(out_dir, recursive = TRUE, full.names = T, pattern = "_T.raw"))
  
}
################################################################################ Graveyard ----

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

# Transform geometries from DT into sf objects to export individually
GeneratePol <- function(GeomSF){
  # Aoi_Arr[[2]]
  SfObj <- st_geometry(GeomSF) %>% st_sf()
  
  # Assign coordinate system
  sf::st_crs( SfObj ) <- 4326
  
  return(SfObj)
}


################################################################################ Button custom (temporal) ----
#' # Customised TRUE-FALSE switch button for Rshiny
#' # Only sing CSS3 code (No javascript)
#' #
#' # Sébastien Rochette
#' # http://statnmap.com/en/
#' # April 2016
#' #
#' # CSS3 code was found on https://proto.io/freebies/onoff/
#' # For CSS3 customisation, refer to this website.
#' 
#' #' A function to change the Original checkbox of rshiny
#' #' into a nice true/false or on/off switch button
#' #' No javascript involved. Only CSS code.
#' #' 
#' #' To be used with CSS script 'button.css' stored in a 'www' folder in your Shiny app folder
#' #' 
#' #' @param inputId The input slot that will be used to access the value.
#' #' @param label Display label for the control, or NULL for no label.
#' #' @param value Initial value (TRUE or FALSE).
#' #' @param col Color set of the switch button. Choose between "GB" (Grey-Blue) and "RG" (Red-Green)
#' #' @param type Text type of the button. Choose between "TF" (TRUE - FALSE), "OO" (ON - OFF) or leave empty for no text.
#' 
#' switchButton <- function(inputId, label, value=FALSE, col = "GB", type="TF") {
#'   
#'   # color class
#'   if (col != "RG" & col != "GB") {
#'     stop("Please choose a color between \"RG\" (Red-Green) 
#'       and \"GB\" (Grey-Blue).")
#'   }
#'   if (!type %in% c("OO", "TF", "YN")){
#'     warning("No known text type (\"OO\", \"TF\" or \"YN\") have been specified, 
#'      button will be empty of text") 
#'   }
#'   if(col == "RG"){colclass <- "RedGreen"}
#'   if(col == "GB"){colclass <- "GreyBlue"}
#'   if(type == "OO"){colclass <- paste(colclass,"OnOff")}
#'   if(type == "TF"){colclass <- paste(colclass,"TrueFalse")}
#'   if(type == "YN"){colclass <- paste(colclass,"YesNo")}
#'   
#'   # No javascript button - total CSS3
#'   # As there is no javascript, the "checked" value implies to
#'   # duplicate code for giving the possibility to choose default value
#'   
#'   if(value){
#'     tagList(
#'       tags$div(class = "form-group shiny-input-container",
#'                tags$div(class = colclass,
#'                         tags$label(label, class = "control-label"),
#'                         tags$div(class = "onoffswitch",
#'                                  tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
#'                                             id = inputId, checked = ""
#'                                  ),
#'                                  tags$label(class = "onoffswitch-label", `for` = inputId,
#'                                             tags$span(class = "onoffswitch-inner"),
#'                                             tags$span(class = "onoffswitch-switch")
#'                                  )
#'                         )
#'                )
#'       )
#'     )
#'   } else {
#'     tagList(
#'       tags$div(class = "form-group shiny-input-container",
#'                tags$div(class = colclass,
#'                         tags$label(label, class = "control-label"),
#'                         tags$div(class = "onoffswitch",
#'                                  tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
#'                                             id = inputId
#'                                  ),
#'                                  tags$label(class = "onoffswitch-label", `for` = inputId,
#'                                             tags$span(class = "onoffswitch-inner"),
#'                                             tags$span(class = "onoffswitch-switch")
#'                                  )
#'                         )
#'                )
#'       )
#'     ) 
#'   }
#' }
