pacman::p_load("shiny","shinyWidgets", "shinyjs", "shinythemes")

setwd("D://PhD//2_SideJobs//UASPlan//App")

addResourcePath(prefix = 'pics', directoryPath = paste0(getwd(),"/www"))

#file.sources <- c("1_GUI.R","2_Server.R")
#invisible(sapply(file.sources,source,.GlobalEnv))

ui <- tagList(
  useShinyjs(),
  navbarPage(title = div(img(src='pics/Logo.png', style="margin-top: -10px; padding-right:10px; padding-bottom:10px", height = 50)),
             windowTitle="JMU UAS Flight book",
             theme = shinytheme("slate"),
             #######################################################################  Create Flight Project
             tabPanel("Data Import",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("UASstyle.css")
                      ),
                      icon = icon("table"),
                      sidebarLayout(
                        sidebarPanel(width = 5,
                                     textInput("misionN", "Mision Name", ""),
                                     textInput("pilot", "Pilot", ""),
                                     textInput("copilot", "Co-Pilot", ""),
                                     dateInput("DoF", "Date:", value = as.character(Sys.Date()), daysofweekdisabled = c(0)),
                                     radioButtons("radio", h3("Radio buttons"),
                                                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                                 "Choice 3" = 3), selected = 1),
                                     checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
                                     sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
                                     selectInput("os_type", "Operating system used most frequently",
                                                 c("",  "Windows", "Mac", "Linux")),
                                     tableOutput("data"),
                                     actionButton("submit", "Submit", class = "btn-primary")
                        ),
                        mainPanel(width = 9,
                                  br(),
                                  DT::dataTableOutput("table1")
                        )
                        
                        
                      ))
  )
)


server <- function(input, output, session) {
}

shinyApp(ui,server)          
