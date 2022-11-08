library(filesstrings)
##############################################################
setwd("D:\\1_Projects\\UniWald\\2_Mission_202207\\1_FieldData\\Wingtra\\Altum\\")

##############################################################
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
##############################################################
# Create standard directory
CreateAltum(getwd(),"Mission1")

# Read and move flight files
MissionDir <- paste0(getwd(),"//150722_Altum_Mission1")

# Set original Micasense mission folder
OriginalFolder <- paste0(getwd(),"\\","0005SET")

# Read flight data 
LogFiles <- list.files(path=OriginalFolder,
                   pattern=".dat",
                   all.files=FALSE,
                   full.names=TRUE)

# Move flight data to new structure
file.move(LogFiles, paste0(MissionDir,"\\4_FlightFiles"))


# Read all image folders available
Images_All <- list.files(path=OriginalFolder,
                       all.files=FALSE,
                       full.names=TRUE)

# Move image files
for(i in 1:length(Images_All)){
  Images <- list.files(path=Images_All[i],all.files=FALSE,full.names=TRUE)
  file.move(Images, paste0(MissionDir,"\\0_Images"))
}




