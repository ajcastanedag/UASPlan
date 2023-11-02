#################################################################################################################################################################### 
createFolderStructure <- function(dir_path, selected_system, Name) {
  ############################################################################## DCG50
  if (selected_system == "DCG50") {
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
  if (grepl("Altum",selected_system) || grepl("MXDual",selected_system)) {
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
  if (grepl("H20T",selected_system)|| grepl("M3T",selected_system)) {
    
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