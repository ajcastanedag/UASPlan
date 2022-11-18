#########################    UAS JMU PLATFORM    ############################### ----
# 
# 
# 
# 
################################################################################
##### Load libraries                                                            -----
pacman::p_load("shiny","shinyWidgets", "shinyjs", "shinythemes", "shinyFiles",
               "leaflet", "tidyverse", "rmarkdown", "shinyBS", "easycsv")
##### Set working directory (temporal for testing)                              ----- 
Root <- "\\\\132.187.202.41\\d$\\0_Document\\UASPlan\\App"
setwd(Root)
##### Add resource path                                                         ----- 
addResourcePath(prefix = 'pics', directoryPath = paste0(getwd(),"\\www"))
##### Include Functions file-> IF NOT SPECIFIED LIDAR COMPUTER FILE WILL BE USED----- 
source("D:/PhD/2_SideJobs/UASPlan/App/0_Functions.R")

##### Possible output locations general directory (E drive)                     ----- 
TargetDrive <- "\\\\132.187.202.41\\d$\\1_Projects"
################################################################################
#################################    UI   ###################################### ----
ui <- tagList(
  useShinyjs(),
  navbarPage(title = div(img(src='pics/Logo.png',
                             style="margin-top: -10px; padding-right:10px; padding-bottom:10px",
                             height = 50)),
             windowTitle="JMU UAS Flight book",
             theme = shinytheme("slate"),
             ###################################################################
             # INFO Tab                                                         ---- 
             # tabPanel("Info",
             #          tags$head(
             #            # Include our custom CSS
             #            includeCSS("UASstyle.css")
             #          ),
             #          icon = icon("circle-info"),
             # 
             #          fluidPage(
             #            titlePanel("Important information"),
             #            tags$hr(style="border-color: gray;"),
             #            fluidRow(
             #              column(3, align="center",
             #                     selectInput("AirCraftM", "Aircraft",
             #                                 c("","Phantom4", "DJI-M600", "DJI-M300", "Wingtra"))
             #                     ),
             #              column(3, align="center",
             #                     selectInput("SensorM", "Sensor",
             #                                 c("","RGB", "RX1R II", "Altum", "MX-Dual", "LiAir V","L1", "H20T"))
             #                     ),
             #              column(3, align="center",
             #                     selectInput("RTKstat", "RTK",
             #                                 c("NO", "YES"))
             #              ),
             #              column(3, align="center",
             #                     actionButton("rst", "Reset", style = 'margin-top:25px', width = "100%")
             # 
             #              )
             # 
             #            ),
             # 
             #            br(),
             #            tags$hr(style="border-color: gray;"),
             # 
             #            fluidRow(
             #              column(12, align="center",
             #                     uiOutput("MDdisplay"))
             #              ),
             # 
             #            br(),
             #            tags$hr(style="border-color: gray;"),
             # 
             #             fluidRow(
             #               column(3, div(style = "height:50px"),
             #                      actionButton("getPro", "Protocol", icon = icon("arrow-down")),
             #                      actionButton("getCheck", "Check List", icon = icon("arrow-down")),
             #                      offset = 9)
             # 
             #             ),
             #          ),
             #          ),
             ###################################################################
             # Create Flight Project                                            ----  
             tabPanel("Create Project",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")
                      ),
                      icon = icon("location"),
                      sidebarLayout(
                        sidebarPanel(width = 5,
                                     
                                     h4(strong("Flight Data"),
                                        align = "left"),
                                     
                                     uiOutput("rootLoc"),
                                     
                                     textInput("misnam",
                                               "Mision Name", ""),
                                     
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 textInput("pilot", "Pilot", ""),
                                                 textInput("copilot", "Co-Pilot", "")
                                                 
                                     ),
                                     
                                     dateInput("DoF",
                                               "Date:",
                                               value = as.character(Sys.Date()),
                                               daysofweekdisabled = c(0),
                                               format = "yyyy_dd_mm") ,
                                     h4(strong("Area of Interest"), align = "left"), 
                                     fileInput("AOI", NULL, accept = c(".txt",".TXT")),
                                     h4(strong("Flights"), align = "left"),
                                     splitLayout(cellWidths = c("44%", "44%", "12%"),
                                                 selectInput("AirCraft", "Aircraft",
                                                             c("", "Phantom4", "DJI-M600", "DJI-M300", "Wingtra")),
                                                 selectInput("Sensor", "Sensor",
                                                             c("","RGB", "Altum", "MX-Dual", "L1", "H20T")),
                                                 actionButton("add", NULL, icon = icon("plus"), style = 'margin-top:25px', width = "100%")
                                     ),
                                     h4(strong("Log Information:"), align = "left"),
                                     textAreaInput("LogInformation",
                                                   NULL,
                                                   value = "",
                                                   placeholder = "Add mission comments here...",
                                                   height = "150px"),
                                     
                                     tags$hr(style="border-color: gray;"),
                                     actionButton("crateStruct",
                                                  "Please fill fields",
                                                  icon = NULL,
                                                  width = "100%"),

                        ),
                        
                        mainPanel(leafletOutput("map"), width = 7,
                                  br(),
                                  DT::dataTableOutput("FlightsDF"))
                      )),
             ###################################################################
             # Load Project Tab                                                 ----  
             # tabPanel("Load Project",
             #           tags$head(
             #             # Include our custom CSS
             #             includeCSS("UASstyle.css")
             #           ),
             #           icon = icon("table"),
             # 
             # )
             ################################################################### 
  )
)
################################################################################
##############################    SERVER   ##################################### ----
server <- function(input, output, session) {
  
  #### Render elements                                                          ----
  # Load introduction information
  output$MDdisplay <- renderUI({includeMarkdown("./Protocols/Introduction.md")})
  
  # Get the folder options from remote folder (D)
  output$rootLoc <- renderUI({
    selectInput("rootLoc", "Project Location",
                choices = c("", list.dirs(path = TargetDrive,
                                          full.names = FALSE,
                                          recursive = FALSE)),
                selected = "")
  })
  
  # Render leaflet map with a reactive function base.map()                     
  output$map <- leaflet::renderLeaflet({
    base.map() 
  })
  
  # Create empty data frame for storing flights                              
  Data <- data.frame(Date=character(),
                     Aircraft=character(), 
                     Sensor=character(), 
                     Name=character(),
                     stringsAsFactors=FALSE) 
  
  # Render table only  with initial options
  output$FlightsDF <- DT::renderDataTable(addData(), editable = TRUE)
  
  addData <- eventReactive(input$add, {
    if(input$add>0){
      tmp <- data.frame(Date=input$DoF,
                         Aircraft=input$AirCraft,
                         Sensor=input$Sensor,
                         Name=input$misnam,
                         stringsAsFactors=FALSE)
      Data <<- rbind(Data, tmp)
    }
    Data
  }, ignoreNULL = FALSE)

  #### Observe Events ----
  observeEvent(input$AirCraft, {
    if(input$AirCraft == "Phantom4"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("","RGB"),
                        selected = "RGB")}
    else if (input$AirCraft == "DJI-M600"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("", "Altum", "MX-Dual", "LiAir V"))}
    else if (input$AirCraft == "DJI-M300"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("", "Altum", "MX-Dual","L1", "H20T"))}
    else if (input$AirCraft == "Wingtra"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("","RX1R II", "Altum"))}
    else updateSelectInput(session,
                           "Sensor",
                           choices=c("","RGB", "RX1R II", "Altum", "MX-Dual", "LiAir V","L1", "H20T"))
  }) 
  
  observeEvent(input$AirCraftM, {
    if(input$AirCraftM == "Phantom4"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("","RGB"),
                        selected = "RGB")}
    else if (input$AirCraftM == "DJI-M600"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("", "Altum", "MX-Dual", "LiAir V"))}
    else if (input$AirCraftM == "DJI-M300"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("", "Altum", "MX-Dual","L1", "H20T"))}
    else if (input$AirCraftM == "Wingtra"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("","RX1R II", "Altum"))}
    else updateSelectInput(session,
                           "SensorM",
                           choices=c("","RGB", "RX1R II", "Altum", "MX-Dual", "LiAir V","L1", "H20T"))
  }) 
  
  observeEvent(input$SensorM, {
    if(input$SensorM == ""){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/Introduction.md")})
    }
    else if(input$AirCraftM == "Phantom4" && input$SensorM == "RGB"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/Phantom4.md")})
    }
    else if(input$AirCraftM == "DJI-M600" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M600Altum.md")})
    }
    else if(input$AirCraftM == "DJI-M600" && input$SensorM == "MX-Dual"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M600MX.md")})
    }
    else if(input$AirCraftM == "DJI-M600" && input$SensorM == "LiAir V"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M600LiAir.md")})
    }
    else if(input$AirCraftM == "DJI-M300" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300Altum.md")})
    }
    else if(input$AirCraftM == "DJI-M300" && input$SensorM == "MX-Dual"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300MX.md")})
    }
    else if(input$AirCraftM == "DJI-M300" && input$SensorM == "L1"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300L1.md")})
    }
    else if(input$AirCraftM == "DJI-M300" && input$SensorM == "H20T"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300H20T.md")})
    }
    else if(input$AirCraftM == "DJI-M300" && input$SensorM == "H20T"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300H20T.md")})
    }
    else if(input$AirCraftM == "Wingtra" && input$SensorM == "RX1R II"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/WingtraSony.md")})
    }
    else if(input$AirCraftM == "Wingtra" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/WingtraAltum.md")})
    }
  })
  
  observeEvent(input$SensorM, {
    if(input$SensorM %in% c("LiAir V","L1")){
      updateSelectInput(session,
                        "RTKstat",
                        choices = "YES",
                        selected = "YES")} else (
      updateSelectInput(session,
                        "RTKstat",
                        choices = c("YES","NO"),
                        selected = "NO"))
  })
  
  observeEvent(input$rst,{
    updateSelectInput(session,
                      "SensorM",
                      selected = "")
    updateSelectInput(session,
                      "AirCraftM",
                      selected = "")
  })
  
  # Function to call the creation of the folder system !!!!
  observeEvent(input$crateStruct, {
    
    Target <- paste0(TargetDrive,"\\",input$rootLoc,"\\")
    SetUp <- paste0(input$AirCraft, input$Sensor)
    Mission <- paste0(input$DoF,"_",input$misnam,"_",SetUp)
    
    CreateFolder(Root, Target, Mission, SetUp, input$LogInformation)
    
    updateSelectInput(session,
                      "Sensor",
                      selected = "")
    updateSelectInput(session,
                      "AirCraft",
                      selected = "")
    
    updateTextAreaInput(session,
                        "LogInformation",
                        value = "")
    
  })
  
  observeEvent(ListenFields(),{
    TempVals <- c(input$rootLoc, input$misnam, input$pilot, input$copilot)
    if(all(TempVals != "")){
      updateActionButton(session, 
                         "crateStruct",
                         "Create project structure",
                         icon = icon("plus"))
    }
  })
  
  #### Reactive Funtions ----
  # Create base map (tiles + gray path) on a reactive function                  ----
  base.map <- reactive({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Cartographic',
                       options = providerTileOptions(opacity = 0.9)) %>%
      addScaleBar(position = "bottomleft",
                  scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE,
                                  updateWhenIdle = TRUE)) %>%
      fitBounds(9.96941167653942, 49.7836214950253, 9.983155389252369,49.789472652132595)
  })
  
  # React to the input information values                                       ---- 
  ListenFields <- reactive({
    list(input$rootLoc,
         input$misnam,
         input$pilot,
         input$copilot)
  })

}
################################################################################
###########################    EXECUTE APP   ################################### ----
shinyApp(ui,server)          
################################################################################