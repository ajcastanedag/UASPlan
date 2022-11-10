################################################################################
SetUpMaker <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    splitLayout(cellWidths = c("45%", "45%"),
                selectInput("AirCraft", "Aircraft",
                            c("",  "Phantom4", "DJI-M600", "DJI-M300", "Wingtra")),
                selectInput("Sensor", "Sensor",
                            c("",  "RGB", "Altum", "MX-Dual", "L1", "H20T")),
    ),
    verbatimTextOutput(ns("out"))
  )
}

################################################################################
ReturnWD <- function(){
  
  MainDir <- as.character(choose.dir())
  print(MainDir)
  #setwd(MainDir)

}
################################################################################
getRemoveButton <- function(n, idS = "", lab = "Pit") {
  if (stringr::str_length(idS) > 0) idS <- paste0(idS, "-")
  ret <- shinyInput(actionButton, n,
                    'button_', label = "Remove",
                    onclick = sprintf('Shiny.onInputChange(\"%sremove_button_%s\",  this.id)' ,idS, lab))
  return (ret)
}

shinyInput <- function(FUN, n, id, ses, ...) {
  as.character(FUN(paste0(id, n), ...))
}
################################################################################