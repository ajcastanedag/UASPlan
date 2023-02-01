#########################    UAS JMU PLATFORM    ############################### ----
# Do not close when setwd() fails.
# Add reset all fields button.
# Fill markdown files with information (from drive)
# 
################################################################################
##### Load libraries                                                            -----
pacman::p_load("shiny","shinyWidgets", "shinyjs", "shinythemes", "shinyFiles",
               "leaflet","leaflet.extras", "tidyverse", "rmarkdown", "shinyBS",
               "easycsv","sf","sfheaders","shinyalert","threejs")

##### Set working directory (temporal for testing)                              ----- 
#Root <- "\\\\132.187.202.41\\c$\\UASPlan\\App"                                  # From remote location 
#Root<- "D:\\UASPlan\\App"                                                        # From office Aj 
#Root <- "D:\\PhD_Main\\UASPlan\\App"                                            # From home Aj 
Root<- "D:\\UASPlan\\App"                                                       # From office Aj 
#Root <- "D:\\PhD_Main\\UASPlan\\App"                                            # From home Aj 
#Root <- "C:\\UASPlan\\App"                                                      # From LidarPc
#Root <- "D:\\02_UAS\\UAS_MB\\App\\UASPlan\\App"                                 # MB 
#Root <- "C:\\Users\\Lsfe1\\Documents\\UASPlan\\App"                              # Laptop UAS
################################################################################
setwd(Root)
##### Add resource path                                                         ----- 
addResourcePath(prefix = 'media', directoryPath = paste0(Root,"\\www"))
##### Include Functions file-> IF NOT SPECIFIED LIDAR COMPUTER FILE WILL BE USED----- 
source(paste0(Root,"\\0_Functions.R"))
##### Possible output locations general directory (E drive)                     ----- 
TargetDrive <- "\\\\132.187.202.41\\d$\\1_Projects"
##### Set path to general style                                                 ----- 
Style <- paste0(Root,"\\www\\2_Style\\UAS_Style_AJCG.css")
################################################################################
#################################    UI   ###################################### ----
ui <- tagList(
  tags$head(
    # Include our custom CSS
    includeCSS(Style)
    #tags$link(rel = "stylesheet", type = "text/css", href = "/www/2_Style/UASstyle.css")
  ),
  useShinyjs(),
  navbarPage(title = div(img(src='4_Graphs/Logo.png',
                             style="margin-top: -10px; padding-right:10px; padding-bottom:10px",
                             height = 50)),
             windowTitle="JMU UAS Flight book",
             theme = shinytheme("slate"),
             ###################################################################
             # INFO Tab                                                         ---- 
             tabPanel("Info",
                      icon = icon("circle-info"),
                      # Start fluid Page
                      fluidPage(
                        titlePanel("Important information"),
                        tags$hr(style="border-color: gray;"),
                        fluidRow(
                          column(3, align="center",
                                 selectInput("AirCraftM", "Aircraft",
                                             c("","Phantom4", "DJIM600", "DJIM300", "Wingtra", "LiBackpack"))
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
                          column(12,
                                 uiOutput("MDdisplay"),
                                 uiOutput("EquipmentPreFlightList"),
                                 uiOutput("PackingListGeneral"),
                                 uiOutput("FlightExcecution"),                                 
                                 uiOutput("PostFlightProtocol"),
                                 uiOutput("EquipmentReturn"))
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
             # Create                                                           ----  
             tabPanel("Create",
                      # Include our custom CSS
                      tags$head(includeCSS(Style)),
                      icon = icon("location"),
                      sidebarLayout(
                        sidebarPanel(width = 5,
                                     tags$div(title="In this tab, you will find all the necessary fields to create a new mission structure or add flights to existing missions. The target drive is set up in the lidar computer's D drive.",
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
                                                 selectizeInput("AirCraft", "Aircraft*",
                                                             c("", "Phantom4", "DJIM600", "DJIM300", "Wingtra", "Mavic", "LiBackpack"),
                                                             options = list(dropdownParent = 'body')),
                                                 selectizeInput("Sensor", "Sensor*",
                                                             c("","RGB", "Altum", "MXDual", "L1", "H20T"),
                                                             options = list(dropdownParent = 'body'))
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
                        
                        mainPanel(leafletOutput("map", height = "60vh"), width = 7,
                                  br(),
                                  tags$div(title="Main Data table: All values stored here will be used to generate folder structure. Please check and modify if needed by double click on the fields.",
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
                        sidebarPanel(width = 5,
                                     h4(strong("Import Project Log File"),
                                        align = "left"),
                                     
                                     fileInput("LogFile", "Area of Interest", accept = c(".gpkg")),
                                     
                                     ),
                        
                        mainPanel(#leafletOutput("mapload", height = "60vh"), 
                          width = 7,
                          uiOutput("World"),
                      )
             )),
             ###################################################################
             # Processing wizard                                                ----
             tabPanel("Processing Wizzard",
                      tags$head(
                        # Include our custom CSS
                        includeCSS(Style)
                      ),
                      icon = icon("hat-wizard"),),
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
  FlightsDF <<- data.frame(FlightName=character(),
                           Pilot=character(),
                           Copilot=character(),
                           DateF=character(),
                           DateC=character(),
                           AirCraft=character(),
                           Sensor=character(),
                           LogText=character(),
                           geometry=character(),
                           stringsAsFactors=FALSE)

  #### Render elements                                                          ----
  # Load introduction information
  output$MDdisplay <- renderUI({includeMarkdown("./Protocols/0_Introduction.md")})
  output$EquipmentPreFlightList <- renderUI({includeMarkdown("./Protocols/2_EquipmentPreFlightList.md")})
  output$PackingListGeneral <- renderUI({includeMarkdown("./Protocols/3_PackingListGeneral.md")})
  output$FlightExcecution <- renderUI({includeMarkdown("./Protocols/4_FlightExcecution.md")})
  output$PostFlightProtocol <- renderUI({includeMarkdown("./Protocols/5_PostFlightProtocol.md")})
  output$EquipmentReturn <- renderUI({includeMarkdown("./Protocols/6_EquipmentReturn.md")})

    
  # Render 3D
  output$World <- renderUI({
    library(threejs)
    data(ego)
    graphjs(ego, bg="#272b30")
  })
  
  # Get the folder options from remote folder (D) to create project
  output$rootLoc <- output$rootLocf <- renderUI({
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
                                        editable = TRUE,
                                        options = list(dom = 't'))
  
  #### Reactive events                                                          ----
  # Include the filled data into the table and reset fields
  addData <- eventReactive(input$add, {
    if(input$add>0 &&
       input$flightNam != "" &&
       input$AirCraft != "" &&
       input$Sensor != "" &&
       input$pilot != "" &&
       input$copilot != ""){
      
      # Assign NA if polygon is empty to keep tmp df seize equal to FlightsDF
      if(is.null(Aoi_Pol$geometry)){
        Aoi_Pol$geometry <- NA
      }
      
      # Create temporal data frame 
      tmp <- data.frame( FlightName=input$flightNam,
                         Pilot=input$pilot,
                         Copilot=input$copilot,
                         DateF=input$DoF,
                         DateC="Tosolve",
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
  # Update Sensor options depending on selected Aircraft
  observeEvent(input$AirCraft, {
    if(input$AirCraft == "Phantom4"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("","RGB"),
                        selected = "RGB")
      shinyjs::enable("Sensor")}
    else if (input$AirCraft == "DJIM600"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("", "Altum", "MXDual", "LiAirV"))
      shinyjs::enable("Sensor")}
    else if (input$AirCraft == "DJIM300"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("", "Altum", "MXDual","L1", "H20T"))
      shinyjs::enable("Sensor")}
    else if (input$AirCraft == "Wingtra"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("","RX1RII", "Altum"))
      shinyjs::enable("Sensor")}
    else if (input$AirCraft == "LiBackpack"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("--"))
      shinyjs::disable("Sensor")}
    else if (input$AirCraft == "Mavic"){
      updateSelectInput(session,
                        "Sensor",
                        choices=c("--"))
      shinyjs::disable("Sensor")}
    else updateSelectInput(session,
                           "Sensor",
                           choices=c("","RGB", "RX1RII", "Altum", "MXDual", "LiAirV","L1", "H20T"))
  }) 
  
  # Get the folders from selected project and update table                      
  observeEvent(input$rootLoc,{
    if(input$rootLoc != ""){
      
      AvailableProjects <- list.dirs(path = paste0(TargetDrive,"\\",input$rootLoc),
                                     full.names = FALSE,
                                     recursive = FALSE)
      
      updateSelectInput(session,
                        "ProjLoc",
                        choices=c("", AvailableProjects),
                        selected = "")
      
      Dir <- paste0(TargetDrive,"\\",
                    input$rootLoc,"\\",
                    input$ProjLoc, "\\0_Flights\\")
    }
  })
  
  # Enable or disable Mission selector based on type of creation
  observeEvent(input$TypeMF,{
    
    if(input$TypeMF == "Mission"){
      shinyjs::hideElement("ProjLoc")
      shinyjs::showElement("misnam")
    } else if(input$TypeMF == "Flights"){
      shinyjs::hideElement("misnam")
      shinyjs::showElement("ProjLoc")
    }
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
    else if (input$AirCraftM == "LiBackpack"){
      updateSelectInput(session,
                        "SensorM",
                        choices=c("--"))}
    else updateSelectInput(session,
                           "SensorM",
                           choices=c("","RGB", "RX1RII", "Altum", "MXDual", "LiAir V","L1", "H20T"))
  }) 
  
  # Render markdown depending on selected set up and suggest RTK 
  observeEvent(input$SensorM, {
    if(input$SensorM == ""){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/0_Introduction.md")})
    } else if(input$AirCraftM == "Phantom4" && input$SensorM == "RGB"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/Phantom4RGB.md")})
    }
    else if(input$AirCraftM == "DJIM600" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/M600Altum.md")})
    }
    else if(input$AirCraftM == "DJIM600" && input$SensorM == "MXDual"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/M600MX.md")})
    }
    else if(input$AirCraftM == "DJIM600" && input$SensorM == "LiAirV"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/M600LiAir.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeHTML("./Protocols/SystemWF/M300Altum.html")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "MXDual"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/M300MX.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "L1"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/M300L1.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "H20T"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/M300H20T.md")})
    }
    else if(input$AirCraftM == "DJIM300" && input$SensorM == "H20T"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/M300H20T.md")})
    }
    else if(input$AirCraftM == "Wingtra" && input$SensorM == "RX1RII"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/WingtraSony.md")})
    }
    else if(input$AirCraftM == "Wingtra" && input$SensorM == "Altum"){
      output$MDdisplay <- renderUI({includeMarkdown("./Protocols/SystemWF/WingtraAltum.md")})
    }
    
    if(input$SensorM %in% c("LiAirV","L1") || input$AirCraftM %in% c("LiBackpack")){
      updateSelectInput(session,
                        "RTKstat",
                        choices = "YES",
                        selected = "YES")} else (
                          updateSelectInput(session,
                                            "RTKstat",
                                            choices = c("YES","NO"),
                                            selected = "NO"))
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
    
    # Check if table has length greater than one 
    if(nrow(FlightsDF)>0 && input$TypeMF == "Mission"){
      
      # Set Folder location to write all the structure
      Target <- paste0(TargetDrive,"\\",input$rootLoc,"\\")
      
      # Load main Project Structure
      MainStructure <- noquote(readLines(paste0(Root,"\\FolderStructures\\0_ProjectBase.txt")))
      
      # Modify the line that contains foldername= and add the dynamic values
      MainNameIndex <- grep('set foldername=', MainStructure)
      MainStructure[MainNameIndex] <- paste0("set foldername=", FlightsDF[1,"DateF"], "_", input$misnam)
      
      # Loop through the rows to create batch of text to write in the main Structure
      for(i in 1:nrow(FlightsDF)){
        
        #Create data name for filling Flight fields
        SetUp <- paste0(FlightsDF[i,"AirCraft"], FlightsDF[i,"Sensor"])
        
        # Laod single SetUpStructure
        FlightStruct <- GetSetup(Root,SetUp)
        
        # Modify the line that contains Subfolders_i and add the flightname
        Index <- grep('set Subfolders_i', FlightStruct)
        FlightStruct[Index] <- paste0("set Subfolders_",i,"=",i,"_",FlightsDF[i,"FlightName"],"_",SetUp)
        
        # Replace all "_i%" with the Flight sequence
        FlightStruct <- str_replace(FlightStruct, "_i%", paste0("_",i,"%")) %>% noquote()
        
        # Locate :: to replace with flight info
        IndexMain <- grep('::', MainStructure)
        MainStructure[IndexMain] <- " "
        
        # Add the flight file to the main 
        MainStructure <- append(x = MainStructure, after = IndexMain, values = FlightStruct)
        
        # Update field 
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
      
      CreateFolder(Root, Target, MainStructure, FlightsDF, input$misnam, 1)
      
      # Modal dialog to check if structure was created
      showModal(modalDialog(
        title = "Folder structure created",
        "Please check",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      ))
      
    } else if(nrow(FlightsDF)>0 && input$TypeMF == "Flights"){
      
      # Load main Project Structure
      MainStructure <- noquote(readLines(paste0(Root,"\\FolderStructures\\0_FlightBase.txt")))
      
      # Modify the line that contains foldername= and add the dynamic values
      MainNameIndex <- grep('set foldername=', MainStructure)
      MainStructure[MainNameIndex] <- paste0("set foldername=", input$ProjLoc)
      
      # Set Folder location to write all the structure
      Target <- paste0(TargetDrive,"\\",input$rootLoc)
      
      # Modify the starting index
      IndexStart <- length(list.dirs(paste0(TargetDrive,"\\",input$rootLoc,"\\",input$ProjLoc,"\\0_Flights\\"), recursive = F))
      
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
      
      } else (shinyalert("Error!", "No flights added to the table!.", type = "error"))
    
    
    
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
    
  })
  
  # Autofill the blocked element between flight and mission
  observe({
    
    Loc <- strsplit(input$ProjLoc, "_")[[1]][2]
    
    updateTextInput(session, "misnam",
                    value = Loc)
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
  
  # Create base map (tiles + gray path) on a reactive function (Base Map Load)                   
  base.mapload <- reactive({
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
      addDrawToolbar(targetGroup = "Imported", position = "bottomright",
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
# {library(shiny)
#   vwr = dialogViewer('modellvergleiche-irt-with-brms', width = 1600, height = 1200)
#   runGadget(shinyAppDir(appDir = 'D:\\UASPlan\\App\\'), viewer = vwr)
# } # Run app in presized window
shinyApp(ui,server)          
################################################################################