#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(stringr)
library(DT)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Mental Health in Tech Survey Dashboard")

sidebar <- sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    menuItem("The Data", icon = icon("table"), tabName = "data"),
    
    radioButtons("type", 
                 "Show survey data for:", 
                 choices = c("Employees at Tech Companies" = 1,
                             "Employees at Non-Tech Companies" = 0))

  )
)

body <- dashboardBody(tabItems(
  
  tabItem("data",
          fluidPage(
            box(title = "IDK", DT::dataTableOutput("data", height = 250))
              
            )
          ))
)


ui <- dashboardPage(header, sidebar, body)



server <- function(input, output) {
  tech_subset <- reactive({
    req(input$type)  # ensure availablity of type before proceeding
    subset(tech, tech_co == input$type)
  }) 
  
  output$data <- DT::renderDataTable({
    DT::datatable(tech_subset(),
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })


}



shinyApp(ui, server)


