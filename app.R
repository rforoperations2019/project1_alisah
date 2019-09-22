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
library(scales)
source("tech_survey.R")

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Mental Health in Tech Survey Dashboard")


# Application sidebar -----------------------------------------------------
sidebar <- sidebar <- dashboardSidebar(
  
  #A very nice menu of the different tabs ------------------------------------
  sidebarMenu(
    id = "tabs",
    
    menuItem("Raw Data", icon = icon("table"), tabName = "data"),
    menuItem("Bar Charts", icon = icon("bar-chart"), tabName = "barz"),
    
    
    #Input information for filtering data for tables and plots -------------------
    radioButtons("type", 
                 "Show survey data for:", 
                 choices = c("Employees at Tech Companies" = 1,
                             "Employees at Non-Tech Companies" = 0)),
    
    #Input for selecting a survey question to explore ------------------------
    selectInput("xvar",
                 "Select Survey Question",
                 choices = c("Does your employer offer mental health benefits?" = 
                               "mh_benefits",
                             "Do you think you could take work leave for a mental health
                             issue?" = "mh_work_leave",
                             "If you took advantage of mental health resources,
                             would your anonymity be protected?" = "anonymity_protected"))

  )
)

# The body of the dashboard -----------------------------------------------------------
body <- dashboardBody(tabItems(
  
  # Data tab: features a data table ----------------------------------------------
  tabItem("data",
          fluidPage(
            box(title = "IDK", DT::dataTableOutput("data"))
              
            )
          ),
  
  # Bar Chart tab: features bar charts with survey questions faceted by company size ----
  tabItem("barz",
          fluidPage(
            box(title = "Bar Charts", width = 12, plotOutput("bars"))
          )))
)


# Putting everything together
ui <- dashboardPage(header, sidebar, body)


# Server function necessary to create my cool things.
server <- function(input, output) {
  
  #Subsetting the data that will be fed into data table and plots.
  tech_subset <- reactive({
    req(input$type)  # ensure availablity of type before proceeding
    subset(tech, tech_co == input$type)
  }) 
  
  # Creating a data table
  output$data <- DT::renderDataTable({
    DT::datatable(tech_subset(),
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  # Code for bar chart with variable survey question input
  output$bars <- renderPlot({
    plot <- ggplot(transform(tech_subset(), 
                             num_employees = factor(num_employees, 
                                                    levels = c('1 to 5', 
                                                               '6 to 25','26-100', 
                                                               '100-500',
                                                               '500-1000','More than 1000')))) +
      geom_bar(aes_string(x=input$xvar)) + 
      facet_wrap(~ num_employees, ncol=2)
    plot +
      xlab('Survey Responses') +
      ylab('Count') +
      scale_x_discrete(labels = wrap_format(8)) +
      ggtitle("Does your employer offer mental health benefits?\nby number of employees at company")
    
  })

}


# Running the application
shinyApp(ui, server)


