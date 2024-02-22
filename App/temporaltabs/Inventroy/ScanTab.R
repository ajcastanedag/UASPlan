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
             tabPanel("Equipment",
                      # Include our custom CSS
                      tags$head(includeCSS(Style)),
                      icon = icon("qrcode"),
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     tags$div(title="HelpText.............",
                                              h4(strong("Register your equipment"),
                                                 align = "center"),
                                     ),
                                     
                                     tags$div(title="HelpText.............",
                                              h5(strong("Basic Data"),
                                                 align = "left"),
                                     ),
                                     
                                     splitLayout(
                                       cellWidths = c("50%", "50%"),
                                       div(
                                         style = "margin-top: 2px;",
                                         selectizeInput(
                                           "name",
                                           label = "Name",
                                           choices = list("", "Mirjana", "Antonio", "Batu", "Elio","Other"),
                                           selected = ".",
                                           options = list(dropdownParent = 'body')
                                         )
                                       ),
                                       div(
                                         style = "margin-top: 0px;", # Adjust margin as needed
                                         textAreaInput(
                                           "misnTargLocTxt",
                                           label = " Email",
                                           resize = "none",
                                           value = "",
                                           height = "40px"
                                         )
                                       )
                                     ),
                                     
                                     tags$div(title="This folder is assigned to you in the email, do not use another project location, as this might lead to confusion. If your folder does not appear, please get in touch with the UAS team immediately.",
                                              uiOutput("rootLoc")
                                     ),
                                     
                                     splitLayout(cellWidths = c("50%","50%"),
                                                 
                                                 tags$div(title="HelpText.............",
                                                          dateInput("DoF",
                                                                    "Date*",
                                                                    autoclose = T,
                                                                    value = as.character(Sys.Date()),
                                                                    daysofweekdisabled = c(0),
                                                                    format = "yyyy_mm_dd")
                                                 ),
                                                 tags$div(title="HelpText.............",
                                                          textAreaInput("loc",
                                                                        "Location of usage",
                                                                        resize = "none",
                                                                        value = "",
                                                                        height = "38px"),
                                                 )
                                     ),
                                     ###########################################
                                     tags$div(title="HelpText.............",
                                              h5(strong("Equipment data"),
                                                 align = "left"),
                                     ),
                                     
                                     splitLayout(cellWidths = c("50%", "50%"),
                                                 textInput("iventorynumber", "Number*", ""),
                                                 textInput("name", "Name*", "")),
                                     
                                     h5(strong("Log Information"), align = "left"),
                                     textAreaInput("LogInformation",
                                                   NULL,
                                                   resize = "none",
                                                   value = "",
                                                   placeholder = "Add mission comments here...",
                                                   height = "125px"),
                                     tags$hr(style="border-color: gray;"),
                                     
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
