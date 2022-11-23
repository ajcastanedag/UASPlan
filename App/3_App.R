#########################    UAS JMU PLATFORM    ############################### ----
# Do not close when setwd() fails.
# Add reset all fields button.
# Fill markdown files with information (from drive)
# 
################################################################################
##### Load libraries                                                            -----
pacman::p_load("shiny","shinyWidgets", "shinyjs", "shinythemes", "shinyFiles",
               "leaflet","leaflet.extras", "tidyverse", "rmarkdown", "shinyBS",
               "easycsv","sf","sfheaders","shinyalert")
##### Set working directory (temporal for testing)                              ----- 
#Root <- "\\\\132.187.202.41\\c$\\UASPlan\\App"                                  # From remote location 
#Root<- "D:\\UASPlan\\App"                                                       # From office Aj 
Root <- "D:\\PhD_Main\\UASPlan\\App"                                            # From home Aj 
#Root <- "C:\\UASPlan\\App"                                                      # From LidarPc
setwd(Root)
##### Add resource path                                                         ----- 
addResourcePath(prefix = 'pics', directoryPath = paste0(getwd(),"\\www"))
##### Include Functions file-> IF NOT SPECIFIED LIDAR COMPUTER FILE WILL BE USED----- 
source(paste0(getwd(),"\\0_Functions.R"))
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
             # # INFO Tab                                                       ---- 
             tabPanel("Info",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")),
                      icon = icon("circle-info"),
                      # Start fluid Page
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
                        includeCSS("UASstyle.css"),),
                      icon = icon("location"),
                      sidebarLayout(
                        sidebarPanel(width = 5,
                                     
                                     h4(strong("Flight Data"),
                                        align = "left"),
                                     
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 uiOutput("rootLoc"),
                                                 textInput("misnam","Mision Name", "")),
                                     splitLayout(cellWidths = c("35%", "35%","30%"),
                                                 textInput("pilot", "Pilot", ""),
                                                 textInput("copilot", "Co-Pilot", ""),
                                                 dateInput("DoF",
                                                           "Date:",
                                                           value = as.character(Sys.Date()),
                                                           daysofweekdisabled = c(0),
                                                           format = "yyyy_dd_mm")),
                                     fileInput("AOI", "Area of Interest", accept = c(".gpkg")),
                                     h4(strong("Flights"), align = "left"),
                                     splitLayout(cellWidths = c("44%", "44%", "12%"),
                                                 selectizeInput("AirCraft", "Aircraft",
                                                             c("", "Phantom4", "DJIM600", "DJIM300", "Wingtra"),
                                                             options = list(dropdownParent = 'body')),
                                                 selectizeInput("Sensor", "Sensor",
                                                             c("","RGB", "Altum", "MXDual", "L1", "H20T"),
                                                             options = list(dropdownParent = 'body')),
                                                 actionButton("add", NULL, icon = icon("plus"),
                                                              style = 'margin-top:23px',
                                                              size ="lg",
                                                              width = "100%"),),
                                     h4(strong("Log Information:"), align = "left"),
                                     textAreaInput("LogInformation",
                                                   NULL,
                                                   value = "",
                                                   placeholder = "Add mission comments here...",
                                                   height = "125px"),
                                     tags$hr(style="border-color: gray;"),
                                     actionButton("crateStruct",
                                                  "Please fill fields",
                                                  icon = NULL,
                                                  width = "100%"),),
                        
                        mainPanel(leafletOutput("map", height = "60vh"), width = 7,
                                  br(),
                                  
                                  DT::dataTableOutput("FlightsDF"))
                      )),
             ###################################################################
             #Load Project Tab                                                  ----
             tabPanel("Load Project",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")
                      ),
                      icon = icon("table"),        
             ),
             ###################################################################
             # Processing wizard                                                ----
             tabPanel("Processing Wizzard",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")
                      ),
                      icon = icon("hat-wizard"),)
             ###################################################################
             
             ################################################################### 
  )
)
################################################################################
##############################    SERVER   ##################################### ----
server <- function(input, output, session) {
  #### Create objects                                                           ----
  # Create empty SP object to store loaded AOI
  Aoi_Pol <<- NULL
  
  # Create empty SP array to store all modified AOI
  Aoi_Arr <<- NULL
  
  # Create empty data frame for storing flights                                               
  Data <- data.frame(Date=character(),
                     Aircraft=character(), 
                     Sensor=character(), 
                     Name=character(),
                     stringsAsFactors=FALSE)
  
  #### Render elements                                                          ----
  # Load introduction information
  output$MDdisplay <- renderUI({includeMarkdown("./Protocols/Introduction.md")})
  
  # Get the folder options from remote folder (D)
  output$rootLoc <- renderUI({
    selectizeInput("rootLoc", "Project Location",
                choices = c("", list.dirs(path = TargetDrive,
                                          full.names = FALSE,
                                          recursive = FALSE)),
                selected = "",
                options = list(dropdownParent = 'body'))
  })
  
  # Render leaflet map with a reactive function base.map()                     
  output$map <- leaflet::renderLeaflet({
    base.map() 
  })
  
  # Render table only  with initial options
  output$FlightsDF <- DT::renderDataTable(addData(),
                                          editable = TRUE,
                                          options = list(dom = 't'))
  
  #### Reactive events                                                          ----
  # Include the filled data into the table and reset fields
  addData <- eventReactive(input$add, {
    if(input$add>0 &&
       input$AirCraft != "" &&
       input$Sensor != "" &&
       input$misnam != "" &&
       input$pilot != "" &&
       input$copilot != ""){
      
      serial_no <- 1 + nrow(Data)
        
      tmp <- data.frame(Date=input$DoF,
                         Aircraft=input$AirCraft,
                         Sensor=input$Sensor,
                         Name=input$misnam,
                         stringsAsFactors=FALSE)
      
      Data <<- rbind(Data, tmp)
      
      Aoi_Arr <<- rbind(Aoi_Arr, Aoi_Pol$geometry)
      
    } else if (!is.null(input$FlightsDF_rows_selected)) {
      
      Data <<- Data[-as.numeric(input$FlightsDF_rows_selected),]
      Aoi_Arr <<- Aoi_Arr[-as.numeric(input$FlightsDF_rows_selected),]
    }
    
    updateSelectInput(session,
                      "Sensor",
                      selected = "")
    updateSelectInput(session,
                      "AirCraft",
                      selected = "")
    Data
  }, ignoreNULL = FALSE)

  #### Observe Events                                                           ----
  # Update Sensor options depending on selected Aircraft
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
  
  # Update SensorM options depending on selected Aircraft
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
  
  # Render markdown depending on selected set up and suggest RTK 
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
  
  # Button to clean SetUp opctions
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
        
        LogInfo <- c(input$rootLoc,
                     input$misnam,
                     input$pilot,
                     input$copilot,
                     Data[i,"Date"],
                     #input$AOI,
                     Data[i,"Aircraft"],
                     Data[i,"Sensor"],
                     input$LogInformation)
        
        CreateFolder(Root, Target, Mission, SetUp, LogInfo, Aoi_Arr[[i]])
        
        updateSelectInput(session,
                          "Sensor",
                          selected = "")
        updateSelectInput(session,
                          "AirCraft",
                          selected = "")
        
        updateTextAreaInput(session,
                            "LogInformation",
                            value = "")
      }
    } else(shinyalert("Error!", "No flights added to the table!.", type = "error"))
    
    
    
  })
  
  # Function to change the status of the crateStruct button to "ready"
  observeEvent(ListenFields(),{
    TempVals <- c(input$rootLoc, input$misnam, input$pilot, input$copilot)
    if(all(TempVals != "")){
      updateActionButton(session, 
                         "crateStruct",
                         "Create project structure",
                         icon = icon("plus"))
    }
  })
  
  # use SelectedCel status to change +/- sybol on add button 
  observeEvent(SelectedCel(),{
    if(SelectedCel()){
      updateActionButton(session,"add",icon = icon("minus"))
      shinyjs::disable("AirCraft")
      shinyjs::disable("Sensor")
      
    } else{
      updateActionButton(session,"add",icon = icon("plus"))
      shinyjs::enable("AirCraft")
      shinyjs::enable("Sensor")}
  })
  
  # Edited Features
  observeEvent(input$map_draw_edited_features, {
    Aoi_Pol <<- ModPolToSf(input$map_draw_edited_features)
  })
  
  # Created Features
  observeEvent(input$map_draw_new_feature, {
    Aoi_Pol <<- ModPolToSf(input$map_draw_new_feature, T)
  })
  
  #### Reactive Functions                                                       ----
  # Create base map (tiles + gray path) on a reactive function                  
  base.map <- reactive({
    RenderedMap <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Cartographic',
                       options = providerTileOptions(opacity = 1)) %>%
      addProviderTiles(providers$Stamen.Toner, group = 'Cartographic',
                       options = providerTileOptions(opacity = 0.3)) %>%
      addScaleBar(position = "bottomleft",
                  scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE,
                                  updateWhenIdle = TRUE)) %>%
      fitBounds(9.96941167653942, 49.7836214950253, 9.983155389252369,49.789472652132595) %>%
      addLayersControl(position = "topright",
                       overlayGroups = "Imported") %>%
      addDrawToolbar(targetGroup = "Imported",
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) 
    
    if(!is.null(input$AOI)){
      Aoi_Pol <<- st_read(input$AOI$datapath, quiet = TRUE) %>% st_transform(4326)

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
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) #%>%
        #addStyleEditor()
      
    }
    
    return(RenderedMap)
  })
  
  # React to the input information values                                       
  ListenFields <- reactive({
    list(input$rootLoc,
         input$misnam,
         input$pilot,
         input$copilot)
  })
  
  # React to selection of cell in Table 
  SelectedCel <- reactive({!is.null(input$FlightsDF_rows_selected)})  

}
################################################################################
#######################    EXECUTE THE APP   ################################### ----
# {library(shiny)
#   vwr = dialogViewer('modellvergleiche-irt-with-brms', width = 1600, height = 1200)
#   runGadget(shinyAppDir(appDir = 'D:\\UASPlan\\App\\'), viewer = vwr)
# } # Run app in presized window
shinyApp(ui,server)          
################################################################################