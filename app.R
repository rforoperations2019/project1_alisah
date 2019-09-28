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
header <- dashboardHeader(title = "Mental Health in Tech Survey",
                          titleWidth = 282) 


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
                 "Select Survey Question to Explore via Bar Charts",
                 choices = c("Does your employer offer mental health benefits?" = 
                               "mh_benefits",
                             "How easy would it be to take work leave for a mental health
                             issue?" = "mh_work_leave",
                             "Do you feel your employer takes mental health
                             as seriously as physical health?" = "mh_serious_ph"))
  )
)

# The body of the dashboard -----------------------------------------------------------
body <- dashboardBody(tabItems(
  
  # Data tab: features a data table ----------------------------------------------
  tabItem("data",
          fluidRow(valueBoxOutput(outputId = "rows"),
                   valueBoxOutput(outputId = "diagnosed"),
                   valueBoxOutput(outputId = "treated")),
            fluidRow(box(title = "The Data", DT::dataTableOutput("data")))
              
            )
          ),
  
  # Bar Chart tab: features bar charts with survey questions faceted by company size ----
  tabItem("barz",
          fluidPage(
            box(title = "Main Plot", width = 12, plotOutput("plot_main")),
            box(title = "Bar Charts", width = 12, plotOutput("bars"))
          ))
)


# Putting everything together
ui <- dashboardPage(header, sidebar, body, skin = "green")


# Server function necessary to create my cool things.
server <- function(input, output) {
  
  #Subsetting the data that will be fed into data table and plots.
  tech_subset <- reactive({
    req(input$type)  # ensure availablity of type before proceeding
    subset(tech, tech_co == input$type)
  }) 
  
  # Counting rows of the tech subset for the valueBox ------------------------
  output$rows <- renderInfoBox({
    num <- nrow(tech_subset())
    
    valueBox("Number of Responses", 
            value = num, 
            icon = icon("comment-dots"), color = "blue")
  })
  
  # Counting number of people with mental illness for the valueBox ------------------------
  output$diagnosed <- renderInfoBox({
    mh_true <- nrow(subset(tech_subset(), ever_diagnosed == "Yes"))
    
    
    valueBox("Respondents Previously Diagnosed 
             with a Mental Health Condition", 
            value = mh_true, 
            icon = icon("brain"), color = "yellow") 
  }) 
  
  # Counting the number of people treated for mental illness ------------
  # Counting number of people with mental illness for the valueBox ------------------------
  output$treated <- renderInfoBox({
    treat_true <- nrow(subset(tech_subset(), ever_treatment == 1))
    
    
    valueBox("Respondents Treated for 
             a Mental Health Condition", 
             value = treat_true, 
             icon = icon("stethoscope"), color = "teal")
  }) 
  
  # Creating a reactive plot title for bar charts --------------------
  plot_title <- reactive({
    req(input$xvar)
    if (input$xvar == 'mh_benefits') {
      title <- "Question: Does your employer offer mental health benefits as part of their healthcare?"
    }
    if (input$xvar == 'mh_work_leave') {
      title <- "Question: Asking for medical leave from work for a mental health issue would be..."
    }
    if (input$xvar == 'mh_serious_ph') {
      title <- "Question: Does your employer take mental health as seriously as physical health?"
    }
    return(title)
  })
  # Creating a data table
  output$data <- DT::renderDataTable({
    DT::datatable(tech_subset(),
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  # Code for bar chart with variable survey question input
  output$bars <- renderPlot({
    plot <- ggplot(tech_subset()) +
      geom_bar(aes_string(x=input$xvar)) + 
      facet_wrap(~ num_employees, ncol=2)
    plot +
      labs(x = 'Survey Responses', y = 'Count', title = plot_title(),
           subtitle = "by size of company", 
           caption = "Thanks to Kaggle for this dataset") +
      scale_x_discrete(labels = wrap_format(8)) +
      theme(plot.title = element_text( face="bold", size=17)) +
      theme(axis.title = element_text(face="bold", size=13)) 
      
    
  })
  output$plot_main <- renderPlot({
    plot_main <- ggplot(tech_subset()) +
      geom_bar(aes_string(x=input$xvar)) 
    plot_main +
      labs(x = 'Survey Responses', y = 'Count', title = plot_title()) +
      scale_x_discrete(labels = wrap_format(8)) +
      theme(plot.title = element_text( face="bold", size=17)) +
      theme(axis.title = element_text(face="bold", size=13)) 
    
  })

}


# Running the application
shinyApp(ui, server)


