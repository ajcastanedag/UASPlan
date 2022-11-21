#########################    UAS JMU PLATFORM    ############################### ----
# 
# 
# 
# 
################################################################################
##### Load libraries                                                            -----
pacman::p_load("shiny","shinyWidgets", "shinyjs", "shinythemes", "shinyFiles",
               "leaflet","leaflet.extras", "tidyverse", "rmarkdown", "shinyBS",
               "easycsv","sf")
##### Set working directory (temporal for testing)                              ----- 
#Root <- "\\\\132.187.202.41\\d$\\0_Document\\UASPlan\\App"
Root<- "D:\\UASPlan\\App"
setwd(Root)
##### Add resource path                                                         ----- 
addResourcePath(prefix = 'pics', directoryPath = paste0(getwd(),"\\www"))
##### Include Functions file-> IF NOT SPECIFIED LIDAR COMPUTER FILE WILL BE USED----- 
source("D:/UASPlan/App/0_Functions.R")
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
             # # INFO Tab                                                         ---- 
             tabPanel("Info",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")
                      ),
                      icon = icon("circle-info"),

                      fluidPage(
                        titlePanel("Important information"),
                        tags$hr(style="border-color: gray;"),
                        fluidRow(
                          column(3, align="center",
                                 selectInput("AirCraftM", "Aircraft",
                                             c("","Phantom4", "DJIM600", "DJIM300", "Wingtra"))
                                 ),
                          column(3, align="center",
                                 selectInput("SensorM", "Sensor",
                                             c("","RGB", "RX1RII", "Altum", "MXDual", "LiAirV","L1", "H20T"))
                                 ),
                          column(3, align="center",
                                 selectInput("RTKstat", "RTK",
                                             c("NO", "YES"))
                          ),
                          column(3, align="center",
                                 actionButton("rst", "Reset", style = 'margin-top:25px', width = "100%")

                          )

                        ),

                        br(),
                        tags$hr(style="border-color: gray;"),

                        fluidRow(
                          column(12, align="center",
                                 uiOutput("MDdisplay"))
                          ),

                        br(),
                        tags$hr(style="border-color: gray;"),

                         fluidRow(
                           column(3, div(style = "height:50px"),
                                  actionButton("getPro", "Protocol", icon = icon("arrow-down")),
                                  actionButton("getCheck", "Check List", icon = icon("arrow-down")),
                                  offset = 9)

                         ),
                      ),
                      ),
             ###################################################################
             # Create Flight Project                                            ----  
             tabPanel("Create Project",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css"),
                        
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
                                     fileInput("AOI", NULL, accept = c(".gpkg")),
                                     h4(strong("Flights"), align = "left"),
                                     splitLayout(cellWidths = c("44%", "44%", "12%"),
                                                 selectInput("AirCraft", "Aircraft",
                                                             c("", "Phantom4", "DJIM600", "DJIM300", "Wingtra")),
                                                 selectInput("Sensor", "Sensor",
                                                             c("","RGB", "Altum", "MXDual", "L1", "H20T")),
                                                 actionButton("add", NULL, icon = icon("plus"), style = 'margin-top:21px', width = "100%")
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
             #Load Project Tab                                                 ----
             tabPanel("Load Project",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")
                      ),
                      icon = icon("table"),
                      
             ),
             ###################################################################
             # Processing wizard                                                 ----
             tabPanel("Processing Wizzard",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")
                      ),
                      icon = icon("hat-wizard"),
                      
             )
             ###################################################################
             
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
  
  # Create empty SP object to store loaded AOI
  Aoi_Pol <- NULL
  
  # Render table only  with initial options
  output$FlightsDF <- DT::renderDataTable(addData(), editable = TRUE)
  
  # Include the filled data into the table and reset fields
  addData <- eventReactive(input$add, {
    if(input$add>0 &&
       input$AirCraft != "" &&
       input$Sensor != "" &&
       input$misnam != "" &&
       input$pilot != "" &&
       input$copilot != ""){

      tmp <- data.frame(Date=input$DoF,
                         Aircraft=input$AirCraft,
                         Sensor=input$Sensor,
                         Name=input$misnam,
                         stringsAsFactors=FALSE)
      Data <<- rbind(Data, tmp)
    }
    updateSelectInput(session,
                      "Sensor",
                      selected = "")
    updateSelectInput(session,
                      "AirCraft",
                      selected = "")
    Data
  }, ignoreNULL = FALSE)

  #### Observe Events ----
  observeEvent(input$AirCraft, {
    if(input$AirCraft == "Phantom4"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("","RGB"),
                        selected = "RGB")}
    else if (input$AirCraft == "DJIM600"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("", "Altum", "MXDual", "LiAirV"))}
    else if (input$AirCraft == "DJIM300"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("", "Altum", "MXDual","L1", "H20T"))}
    else if (input$AirCraft == "Wingtra"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("","RX1RII", "Altum"))}
    else updateSelectInput(session,
                           "Sensor",
                           choices=c("","RGB", "RX1RII", "Altum", "MXDual", "LiAirV","L1", "H20T"))
  }) 
  
  observeEvent(input$AirCraftM, {
    if(input$AirCraftM == "Phantom4"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("","RGB"),
                        selected = "RGB")}
    else if (input$AirCraftM == "DJIM600"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("", "Altum", "MXDual", "LiAirV"))}
    else if (input$AirCraftM == "DJIM300"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("", "Altum", "MXDual","L1", "H20T"))}
    else if (input$AirCraftM == "Wingtra"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("","RX1RII", "Altum"))}
    else updateSelectInput(session,
                           "SensorM",
                           choices=c("","RGB", "RX1RII", "Altum", "MXDual", "LiAir V","L1", "H20T"))
  }) 
  
  observeEvent(input$SensorM, {
    if(input$SensorM == ""){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/Introduction.md")})
    } else if(input$AirCraftM == "Phantom4" && input$SensorM == "RGB"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/Phantom4RGB.md")})
    }
    else if(input$AirCraftM == "DJIM600" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M600Altum.md")})
    }
    else if(input$AirCraftM == "DJIM600" && input$SensorM == "MXDual"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M600MX.md")})
    }
    else if(input$AirCraftM == "DJIM600" && input$SensorM == "LiAirV"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M600LiAir.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300Altum.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "MXDual"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300MX.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "L1"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300L1.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "H20T"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300H20T.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "H20T"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/M300H20T.md")})
    }
    else if(input$AirCraftM == "Wingtra" && input$SensorM == "RX1RII"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/WingtraSony.md")})
    }
    else if(input$AirCraftM == "Wingtra" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/WingtraAltum.md")})
    }
  })
  
  observeEvent(input$SensorM, {
    if(input$SensorM %in% c("LiAirV","L1")){
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

    if(nrow(Data)>0){

      for(i in 1:nrow(Data)){
        SetUp <- paste0(Data[i,"Aircraft"], Data[i,"Sensor"])
        Mission <- paste0(Data[i,"Date"],"_",Data[i,"Name"],"_",SetUp)
        
        CreateFolder(Root, Target, Mission, SetUp, input$LogInformation, Aoi_Pol)
        
        print(Aoi_Pol)
      }
    }
    
    
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
  
  #### Reactive Functions ----
  # Create base map (tiles + gray path) on a reactive function                  ----
  base.map <- reactive({
    RenderedMap <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Cartographic',
                       options = providerTileOptions(opacity = 0.9)) %>%
      addScaleBar(position = "bottomleft",
                  scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE,
                                  updateWhenIdle = TRUE)) %>%
      fitBounds(9.96941167653942, 49.7836214950253, 9.983155389252369,49.789472652132595) 
    
    if(!is.null(input$AOI)){
      Aoi_Pol <<- st_read(input$AOI$datapath) %>% st_transform(4326)

      RenderedMap <- RenderedMap %>% addPolygons(data = Aoi_Pol,
                                                 color = "green",
                                                 group = "Imported") %>%
        fitBounds(st_bbox(Aoi_Pol)[[1]],
                  st_bbox(Aoi_Pol)[[2]],
                  st_bbox(Aoi_Pol)[[3]],
                  st_bbox(Aoi_Pol)[[4]]) %>%
        addLayersControl(position = "topright",
                         overlayGroups = "Imported") %>%
        addDrawToolbar(
          targetGroup = "Imported",
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) 
      
    }
    
    return(RenderedMap)
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
#######################    EXECUTE THE APP   ################################### ----
shinyApp(ui,server)          
################################################################################