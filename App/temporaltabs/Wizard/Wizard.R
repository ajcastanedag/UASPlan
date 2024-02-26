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
                        sidebarPanel(width = 3,
                                     tags$div(title="HelpText.............",
                                              h4(strong("Select parameters"),
                                                 align = "center"),
                                     ),
                                     
                                     tags$div(title="HelpText.............",
                                              h5(strong("Basic Data"),
                                                 align = "left"),
                                     ),
                                     
                                     div(
                                       style = "margin-top: 2px;",
                                       selectizeInput(
                                         "ws",
                                         label = "Working station:",
                                         choices = list("", "WSI", "WSII"),
                                         selected = ".",
                                         options = list(dropdownParent = 'body')
                                       )
                                     ),
                                     
                                     textInput("calibrate_dir", "Calibration panel",
                                               value = "RP104.csv"),

                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         style = "margin-top: 2px;",
                                         selectizeInput(
                                           "mis_name",
                                           label = "Mission",
                                           choices = list("","Tester1"),
                                           selected = ".",
                                           options = list(dropdownParent = 'body')
                                         )
                                       ),
                                       div(
                                         style = "margin-top: 2px;",
                                         selectizeInput(
                                           "fl_name",
                                           label = "Flight",
                                           choices = list("","AltumMicasense"),
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
                                                      label = "Calibration panel",
                                                      col = "GB",
                                                      value = TRUE)
                                         ),
                                       div(
                                         switchButton(inputId = "reflectance_dls",
                                                      label = "DLS",
                                                      col = "GB",
                                                      value = FALSE)
                                       )
                                     ),
                                     
                                     tags$div(title="HelpText.............",
                                              h5(strong("Align Photos"),
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
                                           DT::dataTableOutput("process")
                        ),
                        
                        tags$div(title="Click here when you have added all flights to the table.",
                                 actionButton("exportitems",
                                              "Please add items",
                                              style = 'margin-top:23px',
                                              width = "110%"),
                        ))
                      )),
             ###################################################################
             
  )
)
################################################################################
# Define server logic
server <- function(input, output) {
  
  # Initialize empty data frame to store registration info
  data <- reactiveVal(data.frame(WS = character(),
                                 CPL = character(),
                                 MSS = character(),
                                 FL = character(),
                                 Qty = character(),
                                 Fl = character(),
                                 EPSG = character(),
                                 CP = logical(),
                                 DLS = logical(),
                                 KPL = integer(),
                                 TPL = integer(),
                                 GM = logical(),
                                 AF =  logical(),
                                 GF =  logical(),
                                 RS =  logical()))
  
  # Add registration info to the data frame
  observeEvent(input$addBtn, {
    newRow <- isolate(c(input$ws,input$calibrate_dir,input$mis_name,input$fl_name,
                        input$acc,input$filter_mode,input$crs2,input$reflectance_pan,
                        input$reflectance_dls,input$keypoint_limit,input$tiepoint_limit,
                        input$guided_matching,input$adaptive_fitting,input$ghosting_filter,
                        input$refine_seamlines))

    if (nchar(trimws(newRow[1])) > 0) {
      NewDataframe <-  as.data.frame(t(newRow))
      names(NewDataframe) <- names(data())
      data(rbind(data(), NewDataframe))
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please fill in all the fields.",
        easyClose = TRUE
      ))
    }
  })
  

  # Render table only  with initial options                                     
  output$process <- DT::renderDataTable(data(),
                                        editable = TRUE,
                                        options = list(dom = 't'))

  # # Export data to Excel
  # observeEvent(input$exportBtn, {
  #   if (nrow(data()) > 0) {
  #     exportFile <- tempfile(fileext = ".xlsx")
  #     write.xlsx(data(), exportFile)
  #     showModal(modalDialog(
  #       title = "Export Complete",
  #       paste("The data has been exported to", exportFile),
  #       easyClose = TRUE
  #     ))
  #   } else {
  #     showModal(modalDialog(
  #       title = "Error",
  #       "No data available to export.",
  #       easyClose = TRUE
  #     ))
  #   }
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
