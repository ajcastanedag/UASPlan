####################### Folder Structure Functions ############################ ----
# This section contains all the functions that call the txt files that contain
# the folder structures for the aircraft - sensor combinations. The txt files are
# converted in .bat files and executed in the windows shell, finally the .bat file
# is erased.
################################################################################
### Phantom 4 
CreatePhantomRGB  <- function(WrkDir, MissionName){
  root <- "D:\\0_Document\\FolderStructures\\"
  Structure <- noquote(readLines(paste0(root,"PhantomRGB.txt"))) 
  Structure[7] <- paste0(Structure[7],"_",MissionName)
  write.table(Structure, file = "PhantomRGB.bat", sep="",
              row.names = FALSE, col.names = FALSE,  quote = FALSE)
  shell.exec("PhantomRGB.bat")
  Sys.sleep(0.5)
  file.remove("PhantomRGB.bat")
  print(paste0(MissionDir_Base,"\\",MissionName))
  
}
### Altum
CreateAltum  <- function(WrkDir, MissionName){
  root <- "D:\\0_Document\\FolderStructures\\"
  Structure <- noquote(readLines(paste0(root,"ALTUM.txt"))) 
  Structure[7] <- paste0(Structure[7],"_",MissionName)
  write.table(Structure, file = "ALTUM.bat", sep="",
              row.names = FALSE, col.names = FALSE,  quote = FALSE)
  shell.exec("ALTUM.bat")
  Sys.sleep(0.5)
  file.remove("ALTUM.bat")
}
############################################################################### ----  





############################################################################### ----  







################################################################################
