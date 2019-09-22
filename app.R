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
source("tech_survey.R")

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Mental Health in Tech Survey Dashboard")

sidebar <- sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    menuItem("Raw Data", icon = icon("table"), tabName = "data"),
    menuItem("Bar Charts", icon = icon("bar-chart"), tabName = "barz"),
    
    radioButtons("type", 
                 "Show survey data for:", 
                 choices = c("Employees at Tech Companies" = 1,
                             "Employees at Non-Tech Companies" = 0)),
    selectInput("xvar",
                 "Select Survey Question",
                 choices = c("Does your employer offer mental health benefits?" = 
                               "mh_benefits"))

  )
)

body <- dashboardBody(tabItems(
  
  tabItem("data",
          fluidPage(
            box(title = "IDK", DT::dataTableOutput("data"))
              
            )
          ),
  tabItem("barz",
          fluidPage(
            box(title = "Bar Charts", width = 12, plotOutput("bars"))
          )))
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
  output$bars <- renderPlot({
    tech$mh_benefits = str_wrap(tech$mh_benefits, width = 10)
    plot <- ggplot(transform(tech_subset(), 
                             num_employees = factor(num_employees, 
                                                    levels = c('1 to 5', 
                                                               '6 to 25','26-100', 
                                                               '100-500',
                                                               '500-1000','More than 1000')))) +
      geom_bar(aes_string(x=input$xvar)) + 
      facet_wrap(num_employees ~.)
    plot +
      xlab('Survey Responses') +
      ylab('Count') +
      ggtitle("Does your employer offer mental health benefits?\nby number of employees at company")
    
  })

}



shinyApp(ui, server)


