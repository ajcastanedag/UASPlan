#########################    UAS JMU PLATFORM    ############################### ----
# Do not close when setwd() fails.
# Add reset all fields button.
# Fill markdown files with information (from drive)
# 
################################################################################
##### Load libraries                                                            -----
pacman::p_load("shiny","shinyWidgets", "stringr","shinyjs", "shinythemes", "shinyFiles",
               "leaflet","leaflet.extras", "tidyverse", "rmarkdown", "shinyBS",
               "easycsv","sf","sfheaders","shinyalert","threejs", "exifr","shinybusy")
################################################################################
Root <- paste0(getwd(),"/App/")
##### Add resource path                                                         ----- 
addResourcePath(prefix = 'media', directoryPath = paste0(Root,"/www"))
##### Include Functions file-> IF NOT SPECIFIED LIDAR COMPUTER FILE WILL BE USED----- 
source(paste0(Root,"/www/3_Functions/Base.R"))
##### Set path to general style                                                 ----- 
Style <- paste0(Root,"/www/2_Style/UAS_Style_AJCG.css")
################################################################################
options(shiny.port = 5555)
################################################################################
#################################    UI   ###################################### ----
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
             # # Info Tab                                                         ----
             # tabPanel("Info",
             #          tags$head(
             #            # Include our custom CSS
             #            includeCSS(Style)
             #          ),
             #          icon = icon("circle-info"),
             #          mainPanel(
             #            #htmlOutput("inc")
             #          )),
             ###################################################################
             # Create                                                           ----  
             tabPanel("Create",
                      # Include our custom CSS
                      tags$head(includeCSS(Style)),
                      icon = icon("location"),
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     #__________________________________________               
                                     tags$div(title="In this tab, you will find all the necessary fields to create a new mission structure or add flights to existing missions. The  target drive is set up in the lidar computer's D drive.",
                                              h4(strong("Creator assistant"),
                                                 align = "center"),
                                     ),
                                     #__________________________________________ 
                                     tags$div(title="In this section, you must fill all fields containing a star and add them to the table using the + button. The app will return an error if the fields are filled partially.",
                                              h5(strong("Target drive"),
                                                 align = "left"),
                                     ),
                                     #__________________________________________ 
                                     splitLayout(
                                       cellWidths = c("40%", "60%"),
                                       div(
                                         selectizeInput(
                                           "TargLocFix",
                                           label = "Select Working Station*",
                                           choices = list("", "WSI", "WSII", "AJCG", "Other"),
                                           selected = ".",
                                           options = list(dropdownParent = 'body')
                                         )
                                       ),
                                       div(
                                         style = "margin-top: 5px;", # Adjust margin as needed
                                         textAreaInput(
                                           "misnTargLocTxt",
                                           "",
                                           resize = "none",
                                           value = "",
                                           height = "38px"
                                         )
                                       )
                                     ),
                                     #__________________________________________ 
                                     tags$div(title="This folder is assigned to you in the email, do not use another project location, as this might lead to confusion. If your folder does not appear, please get in touch with the UAS team immediately.",
                                              uiOutput("rootLoc")
                                     ),
                                     #__________________________________________ 
                                     splitLayout(cellWidths = c("50%","50%"),
                                                 
                                                 tags$div(title="Select which type of data you want to create. For example, you can choose between creating a whole mission or adding flights to existing missions.",
                                                          selectizeInput("TypeMF",
                                                                       label = "Select type*",
                                                                       choices= list("Mission", "Flights"), 
                                                                       selected = "Mission",
                                                                       options = list(dropdownParent = 'body'))
                                                 ),
                                                 tags$div(title="This field will be the name used to create a single folder to store all the flights you make in the following section. Use words familiar to you or associated with features of the flight. Avoid special characters, numbers, dates and UAV or sensor names since those will be assigned automatically.",
                                                          textAreaInput("misnam",
                                                                        "Mission Name*",
                                                                        resize = "none",
                                                                        value = "",
                                                                        height = "38px"),
                                                          selectizeInput("ProjLoc", "Select mission*",
                                                                         c("","NO project available"),
                                                                         options = list(dropdownParent = 'body')),
                                                 )
                                                 ),
                                     #__________________________________________ 
                                     tags$div(title="In this section, you must fill all fields containing a star and add them to the table using the + button. The app will return an error if the fields are filled partially.",
                                              h5(strong("Flight Data"),
                                                 align = "left"),
                                     ),
                                     #__________________________________________ 
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 textInput("pilot", "Pilot*", ""),
                                                 textInput("copilot", "Co-Pilot*", "")),
                                     #__________________________________________ 
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 tags$div(title="This name will be used on the folder name as guidance to understand the flight's purpose. Avoid using any names that contain numbers, dates or special characters. Also, avoid including the sensor's or UAV's name since those will be automatically included. (f.e. 1_FlightName_Phantom4RGB).",
                                                          textInput("flightNam", "Flight label*", "")
                                                          ),
                                                 tags$div(title="In this field, you can select the date of each flight.",
                                                          dateInput("DoF",
                                                                    "Date*",
                                                                    autoclose = T,
                                                                    value = as.character(Sys.Date()),
                                                                    daysofweekdisabled = c(0),
                                                                    format = "yyyy_mm_dd")
                                                          ),
                                     ),
                                     #__________________________________________ 
                                     h5(strong("Equipment used"), align = "left"),
                                     #__________________________________________ 
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 uiOutput("AirCraft"),
                                                 uiOutput("Sensor")
                                                 ),
                                     #__________________________________________ 
                                     h5(strong("Log Information"), align = "left"),
                                     textAreaInput("LogInformation",
                                                   NULL,
                                                   resize = "none",
                                                   value = "",
                                                   placeholder = "Add mission comments here...",
                                                   height = "125px"),
                                     #__________________________________________ 
                                     tags$hr(style="border-color: gray;"),
                                     #__________________________________________ 
                                     actionButton("add", NULL, icon = icon("plus"),
                                                  #style = 'margin-top:23px',
                                                  size ="lg",
                                                  width = "100%")
                                     ),
                        #_______________________________________________________
                        mainPanel(tags$div(title="Main Data table: All values stored here will be used to generate folder structure. Please check and modify if needed by double click on the fields.",
                                  DT::dataTableOutput("Flights")
                                  ),
                                  
                                  tags$div(title="Click here when you have added all flights to the table.",
                                  actionButton("crateStruct",
                                               "Please add flights",
                                               style = 'margin-top:23px',
                                               width = "100%"),
                                  ))
                      )),
             ###################################################################
             # Thermal Calibration                                              ----
             tabPanel("ThermCal",
                      tags$head(
                        # Include our custom CSS
                        includeCSS(Style)
                      ),
                      icon = icon("temperature-full"),
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     #__________________________________________  
                                     tags$div(title="In this tab, you will find all the necessary fields to calibrate DJI thermal images",
                                              h4(strong("Thermal calibration"),
                                                 align = "center"),
                                     ),
                                     #__________________________________________  
                                     tags$div(title="In this section, you must fill all fields containing a star and add them to the table using the + button. The app will return an error if the fields are filled partially.",
                                              h5(strong("DJI-SDK"), align = "left"),
                                     ),
                                     #__________________________________________  
                                     textInput("sdk_dir", "Select SDK Directory", paste0(Root,"/www/1_SDK")),
                                     #__________________________________________  
                                     actionButton("sdk_download", "Get ThermalSDK", icon = icon("download"),
                                          size ="lg",
                                          width = "100%"),
                                     #__________________________________________  
                                     tags$div(title="In this section, you must fill all fields containing a star and add them to the table using the + button. The app will return an error if the fields are filled partially.",
                                              h5(strong("Select thermal images allocation"), align = "left"),
                                     ),
                                     #__________________________________________  
                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         uiOutput("ThermalProtLoc")
                                       ),
                                       div(
                                         uiOutput("ThermalFlightLoc")
                                       )
                                     ),
                                     #__________________________________________  
                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         disabled(textInput("in_Thdir", "Input dir", NULL))
                                       ),
                                       div(
                                         disabled(textInput("out_Thdir", "Output dir", NULL))
                                       )
                                     ),
                                     #__________________________________________  
                                     actionButton("makemapthermalCal", "Make map", icon = icon("map"),
                                                  size ="lg",
                                                  width = "100%"),
                                     #__________________________________________  
                                     tags$div(title="In this section, you must fill all fields containing a star and add them to the table using the + button. The app will return an error if the fields are filled partially.",
                                              h5(strong("Set parameters"), align = "left"),
                                     ),
                                     #__________________________________________  
                                     sliderInput("emissivity", "Emissivity", 0.1, 1, 1, step = 0.01),
                                     #__________________________________________  
                                     sliderInput("humidity", "Humidity", 20, 100, 70, step = 5),
                                     #__________________________________________  
                                     sliderInput("distance", "Distance", 1, 25, 25, step = 1),
                                     #__________________________________________  
                                     tags$hr(style="border-color: gray;"),
                                     #__________________________________________  
                                     actionButton("startTCal_button", NULL, icon = icon("play"),
                                                  size ="lg",
                                                  width = "100%"),
                                     #__________________________________________  
                                     br(),
                                     #__________________________________________  
                                     progressBar(id = "TCalProgBar", value = 0, title = " ", display_pct = T)
                                       
                        ),
                        #_______________________________________________________
                        mainPanel(width = 8,
                                  uiOutput("ThermalMain") )
             )),
             ###################################################################
             # # Load Project Tab                                                 ----
             # tabPanel("Load Project",
             #          tags$head(
             #            # Include our custom CSS
             #            includeCSS(Style)
             #          ),
             #          icon = icon("table"),
             #          sidebarLayout(
             #            sidebarPanel(width = 5),
             #            mainPanel(width = 7)
             # )),
             ###################################################################
             # # Mission Planner                                                  ----
             # tabPanel("Mission Planner",
             #          tags$head(
             #            # Include our custom CSS
             #            includeCSS(Style)
             #          ),
             #          icon = icon("ruler-combined"),
             #          sidebarLayout(
             #            sidebarPanel(width = 5),
             #            mainPanel(width = 7)
             #          )),
             ###################################################################
             # # Processing wizard                                                ----
             # tabPanel("Processing Wizzard",
             #          tags$head(
             #            # Include our custom CSS
             #            includeCSS(Style)
             #          ),
             #          icon = icon("hat-wizard")),
             ###################################################################
             # # Processing wizard                                                ----
             # tabPanel("ThermCal",
             #          tags$head(
             #            # Include our custom CSS
             #            includeCSS(Style)
             #          ),
             #          icon = icon("temperature-full"))
             ################################################################### 
  )
)
################################################################################
##############################    SERVER   ##################################### ----
server <- function(input, output, session) {
  ###################################################################           ---

  ###################################################################           ---
  # Info Tab 
  #### Create objects                                                           ----
  getPage <- function() {
    return(includeHTML(paste0(Root,'www/0_IntroPage/instructions.html')))
  }
  output$inc<-renderUI({getPage()})
  ###################################################################           
  # Create Tab                                                                  ----
  #### Create objects                                                           ----
  # Create empty SP object to store loaded AOI
  Aoi_Pol <<- NULL
  
  # Create Main data frame to store all information
  FlightsDF <<- data.frame(Path=character(),
                           FlightName=character(),
                           Pilot=character(),
                           Copilot=character(),
                           DateF=character(),
                           AirCraft=character(),
                           Sensor=character(),
                           LogText=character(),
                           LogLoc=character(),
                           geometry=character(),
                           stringsAsFactors=FALSE)
  
  
 
  # Fixed or "Definition" dataframe for getting options
  SetUpDF <<- data.frame(UAV = c("Phantom4","DJIM600","DJIM300","Wingtra","Mavic","TrinityF90pro","LiBackpack"),
                         Sensors = c(paste0(".","-RGB"),
                                     paste0(".","-Altum","-MXDual","-LiAirV","-NanoHP"),
                                     paste0(".","-Altum","-MXDual","-L1","-H20T"),
                                     paste0(".","-RX1RII","-Altum"),
                                     paste0(".","-3TAgisoft","-3TTerra","-3MAgisoft","-3MTerra"),
                                     paste0(".","-AltumPT","-D2M","-Qube240"),
                                     paste0(".")
                                     ),
                         stringsAsFactors=FALSE
                         ) 

  #### Render elements                                                          ----
  output$AirCraft <- renderUI({
    selectizeInput("AirCraft", "Aircraft",
                   c("",SetUpDF$UAV),
                   options = list(dropdownParent = 'body'))
  })
  
  output$Sensor <- renderUI({
    selectizeInput("Sensor",
                   "Sensor",
                   c(SetUpDF$Sensors %>% str_replace(pattern = ".", replacement = "") %>% str_split(pattern = "-",simplify = T) %>% as.vector() %>% unique()),
                   options = list(dropdownParent = 'body'))
  })
  
  # Get the folder options from TargetDrive
  output$rootLoc <- renderUI({
    selectizeInput("rootLoc", "Project Location*",
                   choices = c("", list.dirs(path = TargetDrive(),
                                             full.names = FALSE,
                                             recursive = FALSE)),
                   selected = "",
                   options = list(dropdownParent = 'body'))
  })

  # Render table only  with initial options                                     
  output$Flights <- DT::renderDataTable(addData(),
                                        editable = TRUE,
                                        options = list(dom = 't'))
  
  # State the table as a proxy
  DTproxy <- DT::dataTableProxy('Flights')
  
  #### Reactive events                                                          ----
  # Include the filled data into the table and reset fields
  addData <- eventReactive(input$add, {
    if(input$add>0 &&
       input$flightNam != "" &&
       input$AirCraft != "" &&
       (input$Sensor != "" || input$AirCraft %in% c("LiBackpack"))&&
       input$pilot != "" &&
       input$copilot != ""){
      
      # Assign NA if polygon is empty to keep tmp df seize equal to FlightsDF
      if(is.null(Aoi_Pol$geometry)){
        Aoi_Pol$geometry <- NA
      }
      
      # Create temporal data frame 
      tmp <- data.frame( Path = paste0(TargetDrive(), input$rootLoc) ,
                         FlightName=paste0(gsub("-","",input$DoF),"_",input$flightNam,"_",input$AirCraft, input$Sensor),  #input$flightNam,  #      #------------ change to paste0(ALl name)
                         Pilot=input$pilot,
                         Copilot=input$copilot,
                         DateF=input$DoF,
                         AirCraft=input$AirCraft,
                         Sensor=input$Sensor,
                         LogText=input$LogInformation,
                         geometry=Aoi_Pol$geometry,
                         stringsAsFactors=FALSE)
      
      # Add row from tmp data frame to FlightsDF
      FlightsDF <<- rbind(FlightsDF, tmp)

    } else if (!is.null(input$Flights_rows_selected)) {
      
      # Delete selected row
      FlightsDF <<- FlightsDF[-as.numeric(input$Flights_rows_selected),]
      
    } else if(input$add>0 && (
                 input$flightNam == "" ||
                 input$AirCraft == "" ||
                 input$Sensor == "" ||
                 input$pilot == "" ||
                 input$copilot == "")){shinyalert("Error", "Fill the fields to add a flight!", type = "error")}
    
    # Update Sensor and Aircraft fields
    updateSelectInput(session,
                      "Sensor",
                      selected = "")
    updateSelectInput(session,
                      "AirCraft",
                      selected = "")
    updateTextInput(session,
                    "flightNam",
                    value = "")
    
    if(nrow(FlightsDF) > 0){
      updateActionButton(session, 
                         "crateStruct",
                         "Create folder structure")
    } else{
        updateActionButton(session, 
                           "crateStruct",
                           "Please add flights")
    }
    
    # Return
    FlightsDF[,c("FlightName","Pilot","Copilot","DateF","AirCraft","Sensor")]
    
  }, ignoreNULL = FALSE)
  
  # Reactive target drive value
  TargetDrive <- reactive({input$misnTargLocTxt})
  
  #### Observe Events                                                           ----
  # Update Drive Location depending on selected working station
  observeEvent(input$TargLocFix, {                                                 
  
    if(input$TargLocFix == ""){
      shinyjs::disable("misnTargLocTxt")
      updateTextAreaInput(session, "misnTargLocTxt",
                          value = "No drive selected")
    } else if(input$TargLocFix == "WSI"){
      shinyjs::disable("misnTargLocTxt")
      updateTextAreaInput(session, "misnTargLocTxt",
                          value = "D:/1_Projects/")
    } else if(input$TargLocFix == "AJCG"){
      shinyjs::disable("misnTargLocTxt")
      updateTextAreaInput(session, "misnTargLocTxt",
                          value = "/home/antonio/Desktop/")
    } else if(input$TargLocFix == "WSII"){
      shinyjs::disable("misnTargLocTxt")
      updateTextAreaInput(session, "misnTargLocTxt",
                          value = "B:/1_Projects/")
      
    } else{
      shinyjs::enable("misnTargLocTxt")
      updateTextAreaInput(session, "misnTargLocTxt",
                          value = "")
    }
    
  }) 
  
  # Evaluate if mission already exists
  observeEvent(input$misnam,{
    
    # Create temporal mission full path
    NewMisLoc <- file.path(paste0(TargetDrive(),input$rootLoc), input$misnam)
    
    # Evaluate if mission exists
    if(dir.exists(NewMisLoc) && input$misnam != ""){
      shinyalert("Error", "Project already exists. \"Select type\" will be changed to Flights. If you want to store a mission with the same name,
                 edit the date first and then change the \"Select type\" field back to mission.", type = "error")
      
      reset("misnam")
      
      updateSelectizeInput(session,"TypeMF", selected = "Flights")
      
    }
  })
  
  # Update Sensor options depending on selected Aircraft
  observeEvent(input$AirCraft, {
    
    shinyjs::enable("Sensor")
    
    updateSelectizeInput(session,
                      "Sensor",
                      choices=SetUpDF$Sensors %>% str_replace(pattern = ".", replacement = "") %>% str_split(pattern = "-",simplify = T) %>% as.vector() %>% unique(),
                      selected = "")
    
    NewChoices <- SetUpDF[SetUpDF$UAV == input$AirCraft,"Sensors"] %>% str_replace(pattern = ".", replacement = "") %>% str_split(pattern = "-",simplify = T)
    
    updateSelectizeInput(session,
                      "Sensor",
                      choices=NewChoices)
    
    if (input$AirCraft %in% c("LiBackpack","")){
      updateSelectizeInput(session,
                        "Sensor",
                        choices=c(""),
                        selected = "")
      shinyjs::disable("Sensor")
    }
    
  }) 
  
  # Get the folders from selected project and update table                      
  observeEvent(input$rootLoc,{
    if(input$rootLoc != ""){
      
      AvailableProjects <- list.dirs(path = paste0(TargetDrive(),"/",input$rootLoc),
                                     full.names = FALSE,
                                     recursive = FALSE)
      
      updateSelectInput(session,
                        "ProjLoc",
                        choices=c("", AvailableProjects),
                        selected = "")
      
      Dir <- paste0(TargetDrive(),"/",
                    input$rootLoc,"/",
                    input$ProjLoc, "/0_Flights/")
    }
  })
  
  # Enable or disable Mission selector based on type of creation             
  observeEvent(input$TypeMF,{
    if(input$TypeMF == "Mission"){
      shinyjs::hideElement("ProjLoc")
      shinyjs::showElement("misnam")
      #updateSelectInput(session,
      #                  "rootLoc",
      #                  selected = "")
    } else if(input$TypeMF == "Flights"){
      shinyjs::hideElement("misnam")
      shinyjs::showElement("ProjLoc")
      #updateSelectInput(session,
      #                  "rootLoc",
      #                  selected = "")
    }
  })
  
  # Function to call the creation of the folder system !!!!
  observeEvent(input$crateStruct, {
    
    # Check if table has length greater than one and mission is selected        
    if(nrow(FlightsDF)>0 && input$TypeMF == "Mission"){
      
      # Create Mission Structure 
      CreateMission(paste0(TargetDrive(),input$rootLoc), input$misnam)
      
      # Loop through the rows to create each folder structure
      for(i in 1:nrow(FlightsDF)){
      
        # Modify final Path
        FlightFinalPath <- paste0(FlightsDF[i,"Path"],"/",input$misnam,"/0_Flights")
        
        # Call function to create structure
        CreateFolder(FlightFinalPath, FlightsDF[i,])
      }
      
      # Modal dialog to check if structure was created
      showModal(modalDialog(
        title = "Folder structure created",
        "Please check if the folder structure was successfully created!",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      ))
      
    }
    
    # Check if table has length greater than one and Flights is selected       --
    else if(nrow(FlightsDF)>0 && input$TypeMF == "Flights"){
      
      # Loop through the rows to create each folder structure
      for(i in 1:nrow(FlightsDF)){
        
        # Create Final name for each flight
        FLightFinalName <- paste0(gsub("-","",FlightsDF[i,"DateF"]),"_",FlightsDF$FlightName[i],"_",FlightsDF[i,"AirCraft"], FlightsDF[i,"Sensor"]) 
        
        # Modify final Path
        FlightFinalPath <- paste0(FlightsDF[i,"Path"],"/",input$ProjLoc,"/0_Flights")
        
        # Call function to create structure
        CreateFolder(FlightFinalPath, FlightsDF[i,])
        
      }
      
      # Modal dialog to check if structure was created
      showModal(modalDialog(
        title = "Folder structure created",
        "Please check if the folder structure was successfully created!",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      ))
      
      }
    
    # Display error
    else (shinyalert("Error!", "No flights added to the table!.", type = "error"))
  })
  
  # When OK button of modal is pressed -> clean UI elements
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    removeModal()
    print("It Worked!")
    
    # Clean UI elements
    updateSelectInput(session,
                      "Sensor",
                      selected = "")
    updateSelectInput(session,
                      "AirCraft",
                      selected = "")
    updateTextInput(session,
                    "flightNam",
                    value = "")
    
    # Render table only  with initial options
    DT::selectRows(DTproxy, seq(1:length(row.names(FlightsDF))), ignore.selectable = FALSE)
    
    # Clear data frame
    FlightsDF <<- FlightsDF[0, ]
    
    # Call add button to delete the fields 
    delay(500, click("add"))

  })
  
  # use SelectedCel status to change +/- symbol on add button 
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
  
  # # Edited Features
  # observeEvent(input$map_draw_edited_features, {
  #   Aoi_Pol <<- ModPolToSf(input$map_draw_edited_features)
  # })
  
  # # Created Features
  # observeEvent(input$map_draw_new_feature, {
  #   Aoi_Pol <<- ModPolToSf(input$map_draw_new_feature, T)
  # })
  # 
  # # Deleted Features
  # observeEvent(input$map_draw_deleted_features, {
  #   Aoi_Pol <<- NULL
  # })
  
  #### Reactive Functions                                                       ----
  # # Create base map (tiles + gray path) on a reactive function (Base Map Create)                
  # base.map <- reactive({
  #   RenderedMap <- leaflet() %>%
  #     addProviderTiles(providers$Esri.WorldImagery, group = 'Cartographic',
  #                      options = providerTileOptions(opacity = 1)) %>%
  #     addProviderTiles(providers$Stamen.Toner, group = 'Cartographic',
  #                      options = providerTileOptions(opacity = 0.3)) %>%
  #     addScaleBar(position = "bottomleft",
  #                 scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE,
  #                                 updateWhenIdle = TRUE)) %>%
  #     fitBounds(9.96941167653942, 49.7836214950253, 9.983155389252369,49.789472652132595) %>%
  #     addLayersControl(position = "topright",
  #                      overlayGroups = "Imported") %>%
  #     addDrawToolbar(targetGroup = "Imported",
  #                    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) 
  #   
  #   if(!is.null(input$AOI)){
  #     Aoi_Pol <<- st_read(input$AOI$datapath, quiet = TRUE) %>% st_transform(4326)
  # 
  #     RenderedMap <- RenderedMap %>% addPolygons(data = Aoi_Pol,
  #                                                color = "green",
  #                                                group = "Imported") %>%
  #       fitBounds(st_bbox(Aoi_Pol)[[1]],
  #                 st_bbox(Aoi_Pol)[[2]],
  #                 st_bbox(Aoi_Pol)[[3]],
  #                 st_bbox(Aoi_Pol)[[4]]) %>%
  #       addLayersControl(position = "topright",
  #                        overlayGroups = "Imported") %>%
  #       addDrawToolbar(
  #         targetGroup = "Imported",
  #         editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) #%>%
  #       #addStyleEditor()
  #     
  #   }
  #   return(RenderedMap)
  # })
  
  # React to selection of cell in Table 
  SelectedCel <- reactive({!is.null(input$Flights_rows_selected)})  
  
  ###################################################################           
  # Thermal Tab                                                                 
  #### Render objects                                                           ----
  # Render ThermalProtLoc element
  output$ThermalProtLoc <- renderUI({
    selectizeInput("ThermalProtLoc", "Select Project:",
                   choices = c("", list.dirs(path = TargetDrive(),
                                             full.names = FALSE,
                                             recursive = FALSE)),
                   selected = "",
                   options = list(dropdownParent = 'body'))
  })
  # Render ThermalFlightLoc element
  output$ThermalFlightLoc <- renderUI({
    selectizeInput("ThermalFlightLoc", "Select Flight:",
                   choices = c("", list.dirs(path = TargetDrive(),
                                             full.names = FALSE,
                                             recursive = FALSE)),
                   selected = "",
                   options = list(dropdownParent = 'body'))
  })
  # Render intro HTML
  output$ThermalMain <- renderUI({
    includeHTML(paste0(Root,"/www/0_IntroPages/ThermalTab.html"))
  })

  #### Observe Events                                                           ----                                                           
  # Get thermal SDK
  observeEvent(input$sdk_download, {
    
    # show waiting GIF
    show_modal_gif(
      src = paste0("media/4_Graphs/GIF/dude4.gif"),
      width = "450px",
      height = "450px",
      modal_size = "m",
      text = div(br(),"Getting the SDK from DJI...", style="font-size:160%")
    )
    
    # Call download function
    getSDK(input$sdk_dir)
    
    # delete waiting GIF
    remove_modal_gif()
    
    # Check for SDK
    if (length(list.files(input$sdk_dir, full.names = F, recursive = T)) > 0) {
      shinyalert(
        title = "SDK downloaded!",
        text = " ",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showConfirmButton = FALSE,
        showCancelButton = FALSE,
        type = "success",
        timer = 1500,
        imageUrl = "",
        animation = TRUE
      )
    } else{shinyjs::alert("Something went wrong!", title = "Error")}
    
  })
  
  # Start calibration function
  observeEvent(input$startTCal_button, {
    
    # Get system OS
    OS <- Sys.info()["sysname"] %>% as.character() %>% tolower()
    
    # Create a list of available files in directory
    files <- list.files(input$sdk_dir, full.names = TRUE, recursive = TRUE)
    
    # Look for the bdji_irp executable 
    sdkPath <- files %>%
      grep("\\bdji_irp\\b", ., ignore.case = TRUE, value = TRUE) %>%
      grep(OS, ., ignore.case = TRUE, value = TRUE) %>%
      grep("release_x64", ., ignore.case = TRUE, value = TRUE)
    
    # Check for SDK
    if (!identical(sdkPath, character(0))) {
      
      # Check if all the rest of the fields are filled    * Needs to be checked!
      if (is.null(input$in_Thdir) || is.null(input$out_Thdir)) {
        shinyjs::alert("Please fill in all required fields.", title = "Error")
      } else {
        
        # Get the input values
        emissivity <- input$emissivity
        humidity <- input$humidity
        distance <- input$distance
        in_Thdir <- input$in_Thdir
        out_Thdir <- input$out_Thdir
        
        # Reset progress bar
        updateProgressBar(id = "TCalProgBar", value = 0)
        
        # show waiting GIF
        show_modal_gif(
          src = paste0("media/4_Graphs/GIF/dude1.gif"),
          width = "860px",
          height = "500px",
          modal_size = "l"
        )
        
        # Perform data processing by calling the ThermlCal function
        ThermalCal(OS, sdkPath, emissivity, humidity, distance, in_Thdir, out_Thdir)
        
        # delete waiting GIF
        remove_modal_gif()
        
        # Reset progress bar
        updateProgressBar(id = "TCalProgBar", value = 0)
      }
  
    } else {
      shinyalert("SDK Error", "SDK not fount, please download!", type = "error")
    }
    
  })
  
  # Render leafletmap of image locations
  observeEvent(input$makemapthermalCal, {
    output$ThermalMain <- renderUI({
      
    })
  })
  ###################################################################           
  # Load Project Tab                                                            ----
  ###################################################################           
  # Mission Planner                                                             ----
  ###################################################################           
  # Processing wizard                                                           ----
  ###################################################################           
  
}
################################################################################
#######################    EXECUTE THE APP   ################################### ----
# 
shinyApp(ui,server)          
################################################################################ñ






