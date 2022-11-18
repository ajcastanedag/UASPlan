####################### Folder Structure Functions ############################ ----
# This section contains all the functions that call the txt files that contain
# the folder structures for the aircraft - sensor combinations. The txt files are
# converted in .bat files and executed in the windows shell, finally the .bat file
# is erased.
################################################################################
### Create Folder structure based on root name, sensor combination and standard name
CreateFolder <- function(Root, TargetLoc, MissionName, SetUp, LogDat){
  # Change directory to Target location
  setwd(TargetLoc)
  # Read TXT structure depending on configuration UAV-Sensor
  if(SetUp == "Phantom4RGB"){
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\PhantomRGB.txt")))
  } else if(SetUp == "WingtraAltum"){
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\WingtraAltum.txt")))
  } else if(SetUp == "WingtraRGB") {
    Structure <- noquote(readLines(paste0(Root,"\\FolderStructures\\WingtraAltum.txt")))
  }else(print("ERROR"))
  
  Structure[grep('foldername=', Structure)] <- paste0(Structure[grep('set foldername=', Structure)],MissionName)
  write.table(Structure, file = "Temporal.bat", sep="",
              row.names = FALSE, col.names = FALSE,  quote = FALSE)
  shell.exec("Temporal.bat")
  Sys.sleep(1)
  file.remove("Temporal.bat")
  
  setwd(paste0(TargetLoc,"\\",MissionName))
  fileConn<-file("FlightLog.txt")
  writeLines(LogDat, fileConn)
  close(fileConn)
  
}
################################################################################
