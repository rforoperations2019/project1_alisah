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
load("tech.Rdata")

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Mental Health in Tech Survey",
                          titleWidth = 282,
                          # Drop down menu with hard coded values ------------------------------
                          dropdownMenu(type = "messages",
                                       messageItem(from = "Alisa",
                                                   message = "Be sure to breathe today", 
                                                        icon = icon("spa"))
                          ),
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = HTML("Sample size is small; <br> selection bias likely!"),
                                         icon = icon("warning"))
                          ))


# Application sidebar -----------------------------------------------------
sidebar <- sidebar <- dashboardSidebar(
  
  #A very nice menu of the different tabs ------------------------------------
  sidebarMenu(
    id = "tabs",
    
    menuItem("Raw Data", icon = icon("table"), tabName = "data"),
    menuItem("Bar Charts Company Size", icon = icon("poll"), tabName = "barz"),
    menuItem("Stacked Bars and Treatment", icon = icon("layer-group"), tabName = "stackz"),
    
    
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
            fluidRow(box(title = h3("The Data"), DT::dataTableOutput("data")))
              
            
          ),
  
  # Bar Chart tab: features bar charts with survey questions faceted by company size ----
  tabItem("barz",
          fluidPage(
            box(title = h4("Main Plot"), width = 12, plotOutput("plot_main")),
            box(title = h4("By Size of Company"), width = 12, plotOutput("bars"))
          )),
  
  # I don't know yet tab --------------------------
  # Stacked Chart tab for questions by treatment = YES or NO -------------
  tabItem("stackz",
          fluidRow(column(width = 4, selectInput("questions", "Select a Question", 
                      choices = c("Does your mental health condition
                      interfere with your work
                      when it is not treated?" = "interferes_when_not_treated",
                      "Would being identified as a person
                      with a mental health 
                      issue hurt your career?" = "mh_identity_hurt_career",
                      "Would discussing a mental health disorder with 
                      your employer have 
                      negative consequences?" = "discuss_mh_employer_neg_consequences")
                      )),
            column(width = 8, box(title = h3("Stacks on Stacks"), 
                                   width = 12, plotOutput("stacks")))
          ))
))


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
    
    valueBox("Respondents", 
            value = num, 
            icon = icon("comment-dots"), color = "blue")
  })
  
  # Counting number of people with mental illness for the valueBox ------------------------
  output$diagnosed <- renderInfoBox({
    mh_true <- nrow(subset(tech_subset(), ever_diagnosed == "Yes"))
    
    
    valueBox("Respondents have been diagnosed with
             a mental health condition at some point", 
            value = mh_true, 
            icon = icon("brain"), color = "olive") 
  }) 
  
  # Counting the number of people treated for mental illness ------------
  output$treated <- renderInfoBox({
    treat_true <- nrow(subset(tech_subset(), ever_treatment == "Yes"))
    
    
    valueBox("Respondents have received treatment 
             for a mental health condition", 
             value = treat_true, 
             icon = icon("stethoscope"), color = "light-blue")
  }) 
  
  # Creating a reactive plot title for bar charts --------------------
  plot_title <- reactive({
    req(input$xvar)
    if (input$xvar == 'mh_benefits') {
      title <- "Question: Does your employer offer mental health benefits\n as part of their healthcare?"
    }
    if (input$xvar == 'mh_work_leave') {
      title <- "Question: Asking for medical leave from work for a mental\n health issue would be..."
    }
    if (input$xvar == 'mh_serious_ph') {
      title <- "Question: Does your employer take mental health\n as seriously as physical health?"
    }
    return(title)
  })
  
  # Creating a reactive plot title for stacked barcharts --------------------
  stack_title <- reactive({
    req(input$questions)
    if (input$questions == 'mh_identity_hurt_career') {
      title2 <- "Question: Would being identified as a person with a mental health issue\n hurt your career?"
    }
    if (input$questions == 'interferes_when_not_treated') {
      title2 <- "Question: Does your mental health condition\n interfere with your work when it is not treated?" 
    }
    if (input$questions == 'discuss_mh_employer_neg_consequences') {
      title2 <- "Question: Would discussing a mental health disorder\nwith your employer have negative consequences?"
    }
    return(title2)
  })
  
  # Creating a data table ------------------------------------
  output$data <- DT::renderDataTable({
    DT::datatable(tech_subset(),
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  # Code for bar chart with variable survey question input
  output$bars <- renderPlot({
    plot <- ggplot(tech_subset()) +
      geom_bar(aes_string(x=input$xvar), fill = "cornflowerblue") + 
      facet_wrap(~ num_employees, ncol=2)
    plot +
      labs(x = 'Survey Responses', y = 'Count', title = plot_title()) +
      scale_x_discrete(labels = wrap_format(8)) +
      theme(plot.title = element_text( face="bold", size=17, hjust =0.5)) +
      theme(axis.text.x = element_text(size=13)) +
      theme(axis.text.y = element_text(size=10)) +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_text(face = "bold", size = 11))
      
    
  })
  output$plot_main <- renderPlot({
    plot_main <- ggplot(tech_subset()) +
      geom_bar(aes_string(x=input$xvar), fill = "gold") 
    plot_main +
      labs(x = 'Survey Responses', y = 'Count', title = plot_title(),
           caption = "Thanks to Kaggle for this dataset") +
      scale_x_discrete(labels = wrap_format(8)) +
      theme(plot.title = element_text( face="bold", size=17, hjust = 0.5)) +
      theme(axis.title = element_text(face="bold", size=13)) +
      theme(axis.text = element_text(size=13))
    
  })
  
  output$stacks <- renderPlot({
    what <- ggplot(tech_subset(), 
                   aes_string("ever_treatment",
                       fill = input$questions)) 
    what <- what + geom_bar()
    what +
      labs(x = 'Ever Received Mental Health Treatment?', 
           y = 'Count', title = stack_title(),
           caption = "Thanks to Kaggle for this dataset",
           fill = "Responses") +
      theme(plot.title = element_text( face="bold", size=17, hjust = 0.5)) +
      theme(axis.title = element_text(face="bold", size=13)) +
      theme(legend.text = element_text(size = 12)) +
      theme(plot.subtitle = element_text(size = 10)) 
  })

}


# Running the application
shinyApp(ui, server)


