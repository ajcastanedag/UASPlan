pacman::p_load("shiny","shinyWidgets", "shinyjs", "shinythemes", "shinyFiles", "leaflet", "tidyverse")

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
             # tabPanel("Info",
             #          tags$head(
             #            # Include our custom CSS
             #            includeCSS("UASstyle.css")
             #          ),
             #          icon = icon("circle-info"),
             # ),
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
