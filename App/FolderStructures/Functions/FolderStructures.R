##############################################################
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