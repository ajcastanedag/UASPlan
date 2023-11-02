library(shiny)

source("/home/antonio/Desktop/AppNew/Structures.R")

# Define the available systems
systems <- c("DCG50", "Phantom4RGB",
             "DJIM600Altum","DJIM600MXDual","DJIM600LiAirV",
             "DJIM300Altum","DJIM300MXDual","DJIM300L1","DJIM600H20T",
             "M3M","M3T"
             ,"WingtraAltum", "WingtraRGB")

ui <- fluidPage(
  titlePanel("Create Folder Structure App"),
  sidebarLayout(
    sidebarPanel(
      textInput("dir_path", "Enter Directory Path:"),
      selectInput("system", "Select System:", choices = systems),
      actionButton("create_structure", "Create Folder Structure")
    ),
    mainPanel(
      verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$create_structure, {
    req(input$dir_path, input$system)
    
    # Get the entered directory path and selected system
    dir_path <- input$dir_path
    selected_system <- input$system
    
    if (createFolderStructure(dir_path, selected_system, paste0(selected_system,"_","Name") )) {
      output$status <- renderText({
        paste("Folder structure for", selected_system, "created in:", dir_path)
      })
    } else {
      output$status <- renderText({
        paste("Folder structure for", selected_system, "already exists in:", dir_path)
      })
    }
  })
}

shinyApp(ui, server)
