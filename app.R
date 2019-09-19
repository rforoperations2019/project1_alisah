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

ui <- dashboardPage(
  dashboardHeader(title = "Mental Health in Tech Survey"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(DTOutput("data", height = 250)),
      
      box(
        title = "Controls",
        radioButtons("type", 
                     "Show survey data for:", 
                     choices = c("Employees at Tech Companies" = 1,
                       "Employees at Non-Tech Companies" = 0))
      )
    )
  )
)

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

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}



shinyApp(ui, server)


