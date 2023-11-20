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
addResourcePath(prefix = 'media', directoryPath = paste0(Root,"/www/"))
##### Include Functions file-> IF NOT SPECIFIED LIDAR COMPUTER FILE WILL BE USED----- 
source(paste0(Root,"/www/3_Functions/Base.R"))
##### Possible output locations general directory (E drive)                     ----- 
TargetDrive <- paste0("/home/antonio/Documents/Samples/")
##### Set path to general style                                                 ----- 
Style <- paste0(Root,"/www/2_Style/UAS_Style_AJCG.css")
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
             # Create                                                           ----  
             tabPanel("Create",
                      # Include our custom CSS
                      tags$head(includeCSS(Style)),
                      icon = icon("location"),
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     tags$div(title="In this tab, you will find all the necessary fields to create a new mission structure or add flights to existing missions. The  target drive is set up in the lidar computer's D drive.",
                                              h4(strong("Creator assistant"),
                                                 align = "center"),
                                     ),
                                     
                                     splitLayout(cellWidths = c("20%","40%","40%"),
                                                 
                                                 tags$div(title="Select which type of data you want to create. For example, you can choose between creating a whole mission or adding flights to existing missions.",
                                                          selectizeInput("TypeMF",
                                                                       label = "Select type*",
                                                                       choices= list("Mission", "Flights"), 
                                                                       selected = "Mission",
                                                                       options = list(dropdownParent = 'body'))
                                                 ),
                                                 tags$div(title="This folder is assigned to you in the email, do not use another project location, as this might lead to confusion. If your folder does not appear, please get in touch with the UAS team immediately.",
                                                          uiOutput("rootLoc")
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
                                     tags$div(title="In this section, you must fill all fields containing a star and add them to the table using the + button. The app will return an error if the fields are filled partially.",
                                              h5(strong("Flight Data"),
                                                 align = "left"),
                                     ),
                                     
                                     splitLayout(cellWidths = c("35%", "35%","30%"),
                                                 textInput("pilot", "Pilot*", ""),
                                                 textInput("copilot", "Co-Pilot*", ""),
                                                 dateInput("DoF",
                                                           "Date*",
                                                           autoclose = T,
                                                           value = as.character(Sys.Date()),
                                                           daysofweekdisabled = c(0),
                                                           format = "yyyy_mm_dd")),
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 tags$div(title="This name will be used on the folder name as guidance to understand the flight's purpose. Avoid using any names that contain numbers, dates or special characters. Also, avoid including the sensor's or UAV's name since those will be automatically included. (f.e. 1_FlightName_Phantom4RGB).",
                                                          textInput("flightNam", "Flight Name*", "")
                                                          ),
                                                 tags$div(title="In this field, you can upload or draw on the map the area you intend to cover with your flight. It doesn't have to be precise but must match the area of interest.",
                                                          fileInput("AOI", "Area of Interest", accept = c(".gpkg"))
                                                          ),
                                     ),
                                     h5(strong("Equipment used"), align = "left"),
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 uiOutput("AirCraft"),
                                                 uiOutput("Sensor")
                                                 ),
                                     h5(strong("Log Information"), align = "left"),
                                     textAreaInput("LogInformation",
                                                   NULL,
                                                   resize = "none",
                                                   value = "",
                                                   placeholder = "Add mission comments here...",
                                                   height = "125px"),
                                     tags$hr(style="border-color: gray;"),
                                     
                                     actionButton("add", NULL, icon = icon("plus"),
                                                  #style = 'margin-top:23px',
                                                  size ="lg",
                                                  width = "100%")
                                     ),
                        
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
             #Load Project Tab                                                  ----
             tabPanel("Load Project",
                      tags$head(
                        # Include our custom CSS
                        includeCSS(Style)
                      ),
                      icon = icon("table"),
                      sidebarLayout(
                        sidebarPanel(width = 5),
                        mainPanel(width = 7)
             )),
             ###################################################################
             # Processing wizard                                                ----
             tabPanel("Processing Wizzard",
                      tags$head(
                        # Include our custom CSS
                        includeCSS(Style)
                      ),
                      icon = icon("hat-wizard")),
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
  SetUpDF <<- data.frame(UAV = c("Phantom4","DJIM600","DJIM300","Wingtra","Mavic","LiBackpack"),
                         Sensors = c(paste0(".","-RGB"),
                                     paste0(".","-Altum","-MXDual","-LiAirV"),
                                     paste0(".","-Altum","-MXDual","-L1","-H20T"),
                                     paste0(".","-RX1RII","-Altum"),
                                     paste0(".","-3TAgisoft","-3TTerra","-3MAgisoft","-3MTerra"),
                                     paste0(".")
                                     ),
                         stringsAsFactors=FALSE
                         ) 

  #### Render elements                                                          ----
  # Render 3D
  output$World <- renderUI({
    library(threejs)
    data(ego)
    graphjs(ego, bg="#272b30")
  })
  
  # Render AirCraft and AirCraftM using definitions from fixed dataframe
  output$AirCraftM <- renderUI({
    selectizeInput("AirCraftM", "Aircraft",
                c("",SetUpDF$UAV),
                options = list(dropdownParent = 'body'))
  })
  output$AirCraft <- renderUI({
    selectizeInput("AirCraft", "Aircraft",
                   c("",SetUpDF$UAV),
                   options = list(dropdownParent = 'body'))
  })
  
  # Render Sensor and SensorM using definitions from fixed dataframe
  output$SensorM <- renderUI({
    selectizeInput("SensorM",
                "Sensor",
                SetUpDF$Sensors %>% str_replace(pattern = ".", replacement = "") %>% str_split(pattern = "-",simplify = T) %>% as.vector() %>% unique(),
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
                   choices = c("", list.dirs(path = TargetDrive,
                                             full.names = FALSE,
                                             recursive = FALSE)),
                   selected = "",
                   options = list(dropdownParent = 'body'))
  })

  # Render leaflet map with a reactive function base.map()                     
  output$map <- leaflet::renderLeaflet({
    base.mapload() 
  })
  
  # Render leaflet map with a reactive function base.map()                     
  output$mapload <- leaflet::renderLeaflet({
    base.map() 
  })
  
  # Render table only  with initial options                                     
  output$Flights <- DT::renderDataTable(addData(),
                                        editable = FALSE,
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
      tmp <- data.frame( Path = paste0(TargetDrive,"/", input$rootLoc) ,
                         FlightName=input$flightNam,
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

  #### Observe Events                                                           ----
  # Evaluate if mission already exists
  observeEvent(input$misnam,{
    
    # Create temporal mission full path
    NewMisLoc <- paste0(TargetDrive,"/",input$rootLoc,"/",input$DoF,"_",input$misnam)
    
    # Evaluate if mission exists
    if(dir.exists(NewMisLoc)){
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
      
      AvailableProjects <- list.dirs(path = paste0(TargetDrive,"/",input$rootLoc),
                                     full.names = FALSE,
                                     recursive = FALSE)
      
      updateSelectInput(session,
                        "ProjLoc",
                        choices=c("", AvailableProjects),
                        selected = "")
      
      Dir <- paste0(TargetDrive,"/",
                    input$rootLoc,"/",
                    input$ProjLoc, "/0_Flights/")
    }
  })
  
  # Enable or disable Mission selector based on type of creation             .......................
  observeEvent(input$TypeMF,{
    if(input$TypeMF == "Mission"){
      shinyjs::hideElement("ProjLoc")
      shinyjs::showElement("misnam")
    } else if(input$TypeMF == "Flights"){
      shinyjs::hideElement("misnam")
      shinyjs::showElement("ProjLoc")
    }
  })
  


  
  
  # Button to clean SetUp options
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
    
    # Check if table has length greater than one and mission is selected
    if(nrow(FlightsDF)>0 && input$TypeMF == "Mission"){
      
      # Create Mission Structure 
      

      
      # Loop through the rows to create each folder structure
      for(i in 1:nrow(FlightsDF)){
        
        print(FlightsDF[i,"Path"])
        
        FLightFinalName <- paste0(gsub("-","",FlightsDF[i,"DateF"]),"_",FlightsDF$FlightName[i],"_",FlightsDF[i,"AirCraft"], FlightsDF[i,"Sensor"]) 
        CreateFolder(FlightsDF[i,"Path"], paste0(FlightsDF[i,"AirCraft"], FlightsDF[i,"Sensor"]), FLightFinalName)
        
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
    
    # Check if table has length greater than one and Flights is selected
    else if(nrow(FlightsDF)>0 && input$TypeMF == "Flights"){
      
      # Load main Project Structure
      MainStructure <- noquote(readLines(paste0(Root,"/www/0_FolderStructures/0_FlightBase.txt")))
      
      # Modify the line that contains foldername= and add the dynamic values
      MainNameIndex <- grep('set foldername=', MainStructure)
      MainStructure[MainNameIndex] <- paste0("set foldername=", input$ProjLoc)
      
      # Set Folder location to write all the structure
      Target <- paste0(TargetDrive,"/",input$rootLoc)
      
      # Modify the starting index
      IndexStart <- length(list.dirs(paste0(TargetDrive,"/",input$rootLoc,"/",input$ProjLoc,"/0_Flights/"), recursive = F))
      
      print(IndexStart)
      
      for(i in 1:nrow(FlightsDF)){
        
        newi <- i + IndexStart
        
        #Create data name for filling Flight fields
        SetUp <- paste0(FlightsDF[i,"AirCraft"], FlightsDF[i,"Sensor"])
        FlightName <- paste0(IndexStart+i,"_",SetUp)
        
        # Laod single SetUpStructure
        FlightStruct <- GetSetup(Root,SetUp)
        
        # Modify the line that contains Subfolders_i and add the flightname
        Index <- grep('set Subfolders_i', FlightStruct)
        FlightStruct[Index] <- paste0("set Subfolders_",newi,"=",newi,"_",FlightsDF[i,"FlightName"],"_",SetUp)
        
        # Replace all "_i%" with the Flight sequence
        FlightStruct <- str_replace(FlightStruct, "_i%", paste0("_",newi,"%")) %>% noquote()
         
        # Locate :: to replace with flight info
        IndexMain <- grep('::', MainStructure)
        MainStructure[IndexMain] <- " "
         
        # Add the flight file to the main 
        MainStructure <- append(x = MainStructure, after = IndexMain, values = FlightStruct)
        
        # Save the location path of the log file as a field in the dataframe
        FlightsDF$LogLoc[i] <- paste0(
          Target,"/",input$ProjLoc,"/0_Flights/",newi,"_",FlightsDF$FlightName[i],
          "_",FlightsDF$AirCraft[i],FlightsDF$Sensor[i],"/3_FlightFiles/0_Log/")
         
      }
      
      CreateFolder(Root, Target, MainStructure, FlightsDF, input$misnam, IndexStart)
      
      # Modal dialog to check if structure was created
      showModal(modalDialog(
        title = "Folder structure created",
        "Please check",
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
  
  # Edited Features
  observeEvent(input$map_draw_edited_features, {
    Aoi_Pol <<- ModPolToSf(input$map_draw_edited_features)
  })
  
  # Created Features
  observeEvent(input$map_draw_new_feature, {
    Aoi_Pol <<- ModPolToSf(input$map_draw_new_feature, T)
  })
  
  # Deleted Features
  observeEvent(input$map_draw_deleted_features, {
    Aoi_Pol <<- NULL
  })
  
  #### Reactive Functions                                                       ----
  # Create base map (tiles + gray path) on a reactive function (Base Map Create)                
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
  
  # React to selection of cell in Table 
  SelectedCel <- reactive({!is.null(input$Flights_rows_selected)})  

}
################################################################################
#######################    EXECUTE THE APP   ################################### ----
# 
shinyApp(ui,server)          
################################################################################ñ
