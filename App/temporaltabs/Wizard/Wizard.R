#########################    UAS JMU PLATFORM    ############################### ----
# Do not close when setwd() fails.
# Add reset all fields button.
# Fill markdown files with information (from drive)
# 
################################################################################
##### Load libraries                                                            -----
pacman::p_load("shiny","shinyWidgets", "stringr","shinyjs", "shinythemes", "shinyFiles",
               "leaflet","leaflet.extras", "tidyverse", "rmarkdown", "shinyBS",
               "easycsv","sf","sfheaders","shinyalert","threejs")
################################################################################
Root <- paste0(getwd(),"/App/")
##### Add resource path                                                         ----- 
addResourcePath(prefix = 'media', directoryPath = paste0(Root,"/www"))
##### Include Functions file-> IF NOT SPECIFIED LIDAR COMPUTER FILE WILL BE USED----- 
source(paste0(Root,"/www/3_Functions/Base.R"))
##### Possible output locations general directory (E drive)                     ----- 
#TargetDrive() <- paste0("/home/antonio/Desktop/")
##### Set path to general style                                                 ----- 
Style <- paste0(Root,"/www/2_Style/UAS_Style_AJCG.css")
################################################################################
# https://github.com/statnmap/RshinyApps/tree/master/On-Off_SwitchButton
################################################################################



ui <- tagList(
  tags$head(
    # Include our custom CSS
    includeCSS(Style)
    #tags$link(rel = "stylesheet", type = "text/css", href = "/www/2_Style/UASstyle.css")
  ),
  useShinyjs(),
  navbarPage(title = div(img(src='media/4_Graphs/Logo.png',
                             style="margin-top: -10px; padding-right:10px; padding-bottom:10px",
                             height = 50)),
             windowTitle="JMU UAS Flight book",
             theme = shinytheme("slate"),
             ###################################################################
             # Create                                                           ----  
             tabPanel("Processing Wizzard",
                      # Include our custom CSS
                      tags$head(includeCSS(Style)),
                      icon = icon("hat-wizard"),
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     tags$div(title="HelpText.............",
                                              h4(strong("Select parameters"),
                                                 align = "center"),
                                     ),
                                     
                                     tags$div(title="HelpText.............",
                                              h5(strong("Basic Data"),
                                                 align = "left"),
                                     ),
                                     
                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         actionButton("WsI", "WSI", 
                                                      #style = 'margin-top:23px',
                                                      size ="lg",
                                                      width = "100%")
                                       ),
                                       div(
                                         actionButton("WsII", "WSII", 
                                                      #style = 'margin-top:23px',
                                                      size ="lg",
                                                      width = "100%")
                                       )
                                     ),
                                     
                                     br(),
                                     
                                     textInput("calibrate_dir", "Calibration pannel",
                                               value = ""),

                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         style = "margin-top: 2px;",
                                         selectizeInput(
                                           "name",
                                           label = "Mission",
                                           choices = list(""),
                                           selected = ".",
                                           options = list(dropdownParent = 'body')
                                         )
                                       ),
                                       div(
                                         style = "margin-top: 2px;",
                                         selectizeInput(
                                           "name",
                                           label = "Flight",
                                           choices = list(""),
                                           selected = ".",
                                           options = list(dropdownParent = 'body')
                                         )
                                       )
                                     ),
                                     
                                     tags$div(title="HelpText.............",
                                              h5(strong("Reconstruction parameters:"),
                                                 align = "left"),
                                     ),
                                     
                                     splitLayout(
                                       cellWidths = c("35%", "35%", "30%"),
                                       div(
                                         style = "margin-top: 2px;",
                                         selectizeInput(
                                           "acc",
                                           label = "Quality",
                                           choices = c("Ultra High","High","Medium"),
                                           selected = "High",
                                           options = list(dropdownParent = 'body')
                                         )
                                       ),
                                       div(
                                         style = "margin-top: 2px;",
                                         selectizeInput(
                                           "filter_mode",
                                           label = "Filtering",
                                           choices = list("No","Mild","Moderate","Agressive"),
                                           selected = "Mild",
                                           options = list(dropdownParent = 'body')
                                         )
                                       ),
                                       div(
                                         style = "margin-top: 2px;",
                                         textInput("crs2", label="EPSG:", value = "4326", width = NULL, placeholder = NULL)
                                       )
                                     ),
                                     
                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         switchButton(inputId = "reflectance_pan",
                                                      label = "Use calibration panel",
                                                      col = "GB",
                                                      value = TRUE)
                                         ),
                                       div(
                                         switchButton(inputId = "reflectance_dls",
                                                      label = "Use DLS",
                                                      col = "GB",
                                                      value = FALSE)
                                       )
                                     ),
                                     
                                     tags$div(title="HelpText.............",
                                              h5(strong("Aligh Photos"),
                                                 align = "left"),
                                     ),
                                     
                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         sliderInput("keypoint_limit", "Keypoint Limit (10e3):",
                                                     min = 10, max = 80,
                                                     value = 40 , step = 1,
                                                     ticks = F, width = "90%")
                                       ),
                                       div(sliderInput("tiepoint_limit", "Tiepoint Limit (10e3):",
                                                       min = 1, max = 20,
                                                       value = 4 , step = 1,
                                                       ticks = F, width = "90%")
                                       )
                                     ),
                                     
                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         switchButton(inputId = "guided_matching",
                                                      label = "Guided matching:",
                                                      col = "GB",
                                                      value = FALSE)
                                       ),
                                       div(
                                         switchButton(inputId = "adaptive_fitting",
                                                      label = "Adaptive fitting:",
                                                      col = "GB",
                                                      value = TRUE)
                                       )
                                     ),
                                     
                                     
                                     tags$div(title="HelpText.............",
                                              h5(strong("Orthomosaic Options"),
                                                 align = "left"),
                                     ),
                                     
                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         switchButton(inputId = "ghosting_filter",
                                                      label = "Ghosting filter:",
                                                      col = "GB",
                                                      value = FALSE)
                                       ),
                                       div(
                                         switchButton(inputId = "refine_seamlines",
                                                      label = "Refine seamlines:",
                                                      col = "GB",
                                                      value = TRUE)
                                       )
                                     ),
                                     
                                     
                                     
                                     
                                     br(),
                                     actionButton("addBtn", NULL, icon = icon("plus"),
                                                  #style = 'margin-top:23px',
                                                  size ="lg",
                                                  width = "100%")
                        ),
                        
                        mainPanel(tags$div(title="Main Data table: All values stored here will be used to generate folder structure. Please check and modify if needed by double click on the fields.",
                                           DT::dataTableOutput("Items")
                        ),
                        
                        tags$div(title="Click here when you have added all flights to the table.",
                                 actionButton("exportitems",
                                              "Please add items",
                                              style = 'margin-top:23px',
                                              width = "100%"),
                        ))
                      )),
             ###################################################################
             
  )
)


# 
# 
# 
# 
# # Define UI
# ui <- fluidPage(
#   titlePanel("Equipment Usage Registration"),
#   sidebarLayout(
#     sidebarPanel(
#       
#       splitLayout(
#         cellWidths = c("40%", "40%"),
#         div(
#           textInput("name", "Name:"),
#           textInput("email", "Email:")
#           )
#         ),
#       
#       
#       dateInput("date", "Date:", value = Sys.Date()),
#       textInput("id", "Object ID:"),
#       actionButton("addBtn", "Add")
#     ),
#     mainPanel(
#       dataTableOutput("table"),
#       actionButton("exportBtn", "Export to Excel")
#     )
#   )
# )

# Define server logic
server <- function(input, output) {
  # Initialize empty data frame to store registration info
  data <- reactiveVal(data.frame(Code = character(),
                                 Name = character(),
                                 Date = as.Date(character()),
                                 Object = character()))
  
  # Add registration info to the data frame
  observeEvent(input$addBtn, {
    newRow <- isolate(c(input$id, input$name, input$date, input$email))
    if (nchar(trimws(newRow[1])) > 0) {
      data(rbind(data(), as.data.frame(t(newRow))))
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please fill in all the fields.",
        easyClose = TRUE
      ))
    }
  })
  
  # Render the table
  output$table <- renderDataTable({
    data()
  })
  
  # Export data to Excel
  observeEvent(input$exportBtn, {
    if (nrow(data()) > 0) {
      exportFile <- tempfile(fileext = ".xlsx")
      write.xlsx(data(), exportFile)
      showModal(modalDialog(
        title = "Export Complete",
        paste("The data has been exported to", exportFile),
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        "No data available to export.",
        easyClose = TRUE
      ))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
