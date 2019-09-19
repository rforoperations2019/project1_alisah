# Creating multiple types of visuals from the same data is an important way to convey
# information to application users. Students will create a Dashboard using a static
# download of an Open Data or a Dataset from their own place of employment (make
#                                                                           sure you have permission to use it for this assignment first!)
# Students may make their application in either flexdashboard or shinydashboard
# layouts and deploy on shinyapps.io.
# Directions:
#   • Include at least:
#   • Three (3) input/filters
# • Three (3) single numeric based boxes/gauges
# • One (1) datatable
# • Three (3) interactive and reactively responsive charts. (use ggplot2 for now)
# • These elements should be places throughout a dashboard with at least
# three (3) pages or tabs with an analytical themes or question about the
# data.
# • On the server side your plots and tables must utilize the reactive function for
# any and all datasets.
# • Your final app must work when deployed to shinyapps.io


setwd('~/CMU/Semester_3/R_Shiny/Project_1')

library(readr)
tech <- read_csv("~/CMU/Semester_3/R_Shiny/Project_1/mental-heath-in-tech-2016_20161114.csv")


tech <- tech[rowSums(is.na(tech)) < ncol(tech)/3, ]  #removing rows where more 
                                                    #than 1/3 responses are null
tech <- tech[,colSums(is.na(tech)) < nrow(tech)/4 ]   #removing columns where more


tech <- tech[-c(16:27)]    #removing columns about previous employers                                                      #than 1/3 of values are null
colnames(tech)
tech <- tech[!(tech$`Are you self-employed?`==1),] #removing self-employed observations
tech <- tech[,-c(5, 6,7, 13, 32, 33)]


for (i in colnames(tech)){
  print(i)
  print(table(tech[[i]]))
}

colnames(tech)[colnames(tech)=='How many employees does your company or organization have?'] <- "num_employees"
colnames(tech)[colnames(tech)=='Does your employer provide mental health benefits as part of healthcare coverage?'] <- "mh_benefits"



tech$mh_benefits = str_wrap(tech$mh_benefits, width = 10)
plot <- ggplot(transform(tech, 
       num_employees = factor(num_employees, 
                                levels = c('1 to 5', 
                                           '6 to 25','26-100', 
                                           '100-500',
                                           '500-1000','More than 1000')))) +
  geom_bar(aes_string(x='mh_benefits')) + 
  facet_wrap(num_employees ~.)
plot +
  xlab('Survey Responses') +
  ylab('Count') +
  ggtitle("Does your employer offer mental health benefits?\nby number of employees at company")

