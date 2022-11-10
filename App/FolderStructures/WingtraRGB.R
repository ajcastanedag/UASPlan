library(filesstrings)
library(easycsv)
##############################################################
# Locate base folder 
MissionDir_Base <- easycsv::choose_dir()
setwd(MissionDir_Base)
##############################################################
CreateWintraRGB  <- function(WrkDir, MissionName){
  root <- "D:\\0_Document\\FolderStructures\\"
  Structure <- noquote(readLines(paste0(root,"WingtraRGB.txt"))) 
  Structure[7] <- paste0(Structure[7],"_",MissionName)
  write.table(Structure, file = "WingtraRGB.bat", sep="",
              row.names = FALSE, col.names = FALSE,  quote = FALSE)
  shell.exec("WingtraRGB.bat")
  Sys.sleep(0.5)
  file.remove("WingtraRGB.bat")
  print(paste0(MissionDir_Base,"\\",MissionName))
  
}
##############################################################

# Create standard directory
CreateWintraRGB(MissionDir_Base,"Mission2")

# Locate Wingtra Geotaged folder
MissionDir_Or <- easycsv::choose_dir()

# Read flight data 
LogFiles <- list.files(path=paste0(MissionDir_Or,"\\DATA"),
                       all.files=FALSE,
                       full.names=TRUE)

# Read flight data 
FlightFiles <- list.files(path=paste0(MissionDir_Or,"\\FLIGHT RECORD"),
                       all.files=FALSE,
                       full.names=TRUE)

# Read all image folders available
TagRep <- list.files(path=paste0(MissionDir_Or,"\\OUTPUT"),
                     all.files=FALSE,
                     full.names=TRUE)%>%
  grep(TagRep,pattern=".JPG",invert=TRUE,value=TRUE)

# Select final project folder *must be optimized
MissionDir <- easycsv::choose_dir()

# Move flight data to new structure
file.move(c(LogFiles,FlightFiles,TagRep), paste0(MissionDir,"\\4_FlightFiles"))

# Read all image folders available
Images_All <- list.files(path=paste0(MissionDir_Or,"\\OUTPUT"),
                         pattern = ".JPG",
                         all.files=FALSE,
                         full.names=TRUE)

# Move image files
file.move(Images_All, paste0(MissionDir,"\\0_Images"))





