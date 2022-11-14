pacman::p_load("shiny","shinyWidgets", "shinyjs", "shinythemes", "shinyFiles", "leaflet", "tidyverse", "markdown")

setwd("D://PhD//2_SideJobs//UASPlan//App")

addResourcePath(prefix = 'pics', directoryPath = paste0(getwd(),"/www"))

Data <- data.frame(Date=character(),
                   Aircraft=character(), 
                   Sensor=character(), 
                   Name=character(),
                   stringsAsFactors=FALSE) 

source("0_Functions.R")

#file.sources <- c("1_GUI.R","2_Server.R")
#invisible(sapply(file.sources,source,.GlobalEnv))

ui <- tagList(
  useShinyjs(),
  navbarPage(title = div(img(src='pics/Logo.png', style="margin-top: -10px; padding-right:10px; padding-bottom:10px", height = 50)),
             windowTitle="JMU UAS Flight book",
             theme = shinytheme("slate"),
             #######################################################################  INFO Tab
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
                          column(6, align="center",
                                 selectInput("AirCraftM", "Aircraft",
                                             c("","Phantom4", "DJI-M600", "DJI-M300", "Wingtra"))
                                 ),
                          column(6, align="center",
                                 selectInput("SensorM", "Sensor",
                                             c("","RGB", "RX1R II", "Altum", "MX-Dual", "LiAir V","L1", "H20T"))
                                 )
                          ),
                        
                        br(),
                        tags$hr(style="border-color: gray;"),
                        
                        fluidRow(
                          column(12, align="center",
                                 uiOutput("MDdisplay"))
                          ),
                        
                         fluidRow(
                           column(3, div(style = "height:50px"),
                                  actionButton("getPro", "Protocol", icon = icon("arrow-down")),
                                  actionButton("getCheck", "Check List", icon = icon("arrow-down")),
                                  offset = 9)
                           
                         ),
                        br(),
                        tags$hr(style="border-color: gray;"),
                      ),
                        # ),
                        # 
                        # fluidRow(
                        #   
                        #   column(1,
                        #          uiOutput("MDdisplay"))
                        #   
                        # ),
                        
                      #   fluidRow(
                      #     
                      #     column(2,
                      #            actionButton("getPro", "Protocol", icon = icon("arrow-down"))),
                      #     
                      #     column(2,
                      #            actionButton("getCheck", "Check List", icon = icon("arrow-down"))
                      #     
                      #     
                      #   ),
                      #   
                      # ),

                      
                      ),
             #######################################################################  Create Flight Project
             tabPanel("Create Project",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")
                      ),
                      icon = icon("location"),
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     h4(strong("Flight Data"), align = "left"),
                                     textInput("misnam", "Mision Name", ""),
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 textInput("pilot", "Pilot", ""),
                                                 textInput("copilot", "Co-Pilot", "")
                                                 
                                     ),
                                     dateInput("DoF", "Date:", value = as.character(Sys.Date()), daysofweekdisabled = c(0)) ,
                                     h4(strong("Area of Interest"), align = "left"), 
                                     fileInput("AOI", NULL, accept = c(".txt",".TXT")),
                                     h4(strong("Flights"), align = "left"),
                                     splitLayout(cellWidths = c("40%", "40%", "10%", "10%"),
                                                 selectInput("AirCraft", "Aircraft",
                                                             c("Phantom4", "DJI-M600", "DJI-M300", "Wingtra")),
                                                 selectInput("Sensor", "Sensor",
                                                             c("RGB", "Altum", "MX-Dual", "L1", "H20T")),
                                                 actionButton("add", NULL, icon = icon("plus"), size = "extra-small"),
                                                 actionButton("rem", NULL, icon = icon("minus"), size = "extra-small")
                                     ),
                                    
                                     h4(strong("Output Location:"), align = "left"),
                                     splitLayout(cellWidths = c("70%", "30%"),
                                                 textInput("LocalDir", NULL, "click..."),
                                                 actionButton("Create", "Create", icon = icon("plus"))
                                                 
                                     ),

                        ),
                        
                        mainPanel(leafletOutput("map"), width = 8,
                                  br(),
                                  DT::dataTableOutput("FlightsDF"))
                      )),
             #######################################################################  Load Project
              tabPanel("Load Project",
                       tags$head(
                         # Include our custom CSS
                         includeCSS("UASstyle.css")
                       ),
                       icon = icon("table"),
                       
             )
  )
)

################################################################################
server <- function(input, output, session) {
  
  output$MDdisplay <- renderUI({includeMarkdown("./Protocols/Introduction.md")})

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
  

    
  
  
  
  
  
  
  onclick("LocalDir", ReturnWD())
  
  observeEvent(input$add, {
    
    print(c(as.character(input$DoF),input$AirCraft,input$Sensor, "test" ))
    
    Data %>% add_row(Date=as.character(input$DoF),
            Aircraft=input$AirCraft, 
            Sensor=input$Sensor, 
            Name="test")
    
    print(Data)
  })
  
  output$FlightsDF <- DT::renderDataTable({
      DT::datatable(Data, options = list(lengthMenu = c(15, 30, 45), pageLength = 15))
    }, rownames = FALSE, height = 100)

  
  output$map <- leaflet::renderLeaflet({
    # call reactive map
    base.map() 
  })
  
  
  # Create base map (tiles + gray path) on a reactive function
  base.map <- reactive({
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Cartographic',
                       options = providerTileOptions(opacity = 0.9)) %>%
      addScaleBar(position = "bottomleft",
                  scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE,
                                  updateWhenIdle = TRUE)) %>%
      fitBounds(9.96941167653942, 49.7836214950253, 9.983155389252369,49.789472652132595)
    
  })
  
  
  observeEvent(input$Create, {
    
    print(c(input$misnam,
            input$pilot,
            input$copilot))

  })
  
}


shinyApp(ui,server)          
