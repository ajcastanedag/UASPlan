library(filesstrings)
library(easycsv)
source("D:\\0_Document\\FolderStructures\\Functions\\FolderStructures.R")
##############################################################
# Locate base folder 
MissionDir_Base <- easycsv::choose_dir()
setwd(MissionDir_Base)

# Choose mission name 
MissionName <- "Default"

# Create standard directory
CreatePhantomRGB(MissionDir_Base,MissionName)

# Locate Phantom images folder
MissionDir_Or <- easycsv::choose_dir()

# Read all image folders available
Images_All <- list.files(path=MissionDir_Or,
                         pattern = ".JPG",
                         all.files=FALSE,
                         full.names=TRUE)

# Move image files
file.move(Images_All, paste0(MissionDir,"\\0_Images"))





