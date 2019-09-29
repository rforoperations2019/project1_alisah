


setwd('~/CMU/Semester_3/R_Shiny/project1_alisah/')

library(readr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(stringr)

tech <- read_csv("~/CMU/Semester_3/R_Shiny/project1_alisah/mental-heath-in-tech-2016_20161114.csv")
tech <- data.frame(tech)

tech <- tech[rowSums(is.na(tech)) < ncol(tech)/3, ]  #removing rows where more 
                                                    #than 1/3 responses are null
tech <- tech[,colSums(is.na(tech)) < nrow(tech)/4 ]   #removing columns where more


tech <- tech[-c(16:27)]    #removing columns about previous employers                                                      #than 1/3 of values are null
colnames(tech)
tech <- tech[!(tech$Are.you.self.employed.==1),] #removing self-employed observations
tech <- tech[,-c(1, 5, 6,7, 13, 30, 32, 33)]



#Recoding all columns for usability while creating the app

colnames(tech)[colnames(tech)=='How.many.employees.does.your.company.or.organization.have.'] <- "num_employees"
colnames(tech)[colnames(tech)=='Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.'] <- "mh_benefits"
colnames(tech)[colnames(tech)=='Is.your.employer.primarily.a.tech.company.organization.'] <- "tech_co"
colnames(tech)[colnames(tech)=='What.is.your.age.'] <- "age"
colnames(tech)[colnames(tech)=='Do.you.work.remotely.'] <- "remote_work?"
colnames(tech)[colnames(tech)=='What.country.do.you.live.in.'] <- "country"
colnames(tech)[colnames(tech)=="If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.NOT.being.treated.effectively."] <- "interferes_when_not_treated"
colnames(tech)[colnames(tech)=="If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.being.treated.effectively."] <- "interferes_when_treated"
colnames(tech)[colnames(tech)=="Have.you.ever.sought.treatment.for.a.mental.health.issue.from.a.mental.health.professional."] <- "ever_treatment"
colnames(tech)[colnames(tech)=="Have.you.been.diagnosed.with.a.mental.health.condition.by.a.medical.professional."] <- "ever_diagnosed"
colnames(tech)[colnames(tech)=="Do.you.currently.have.a.mental.health.disorder."] <- "current_mh_disorder"
colnames(tech)[colnames(tech)=="Have.you.had.a.mental.health.disorder.in.the.past."] <- "past_mh_disorder"
colnames(tech)[colnames(tech)=="Do.you.have.a.family.history.of.mental.illness." ] <- "fam_history_mental_illness"
colnames(tech)[colnames(tech)=="Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.provided.by.your.employer." ] <- "anonymity_protected"
colnames(tech)[colnames(tech)=="If.a.mental.health.issue.prompted.you.to.request.a.medical.leave.from.work..asking.for.that.leave.would.be."  ] <- "mh_work_leave"
colnames(tech)[colnames(tech)=="Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences."] <- "discuss_mh_employer_neg_consequences"
colnames(tech)[colnames(tech)=="Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences."     ] <- "discuss_ph_employer_neg_consequences"
colnames(tech)[colnames(tech)=="Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers."  ] <- "comfortable_discuss_mh_disorder_coworkers"
colnames(tech)[colnames(tech)=="Have.you.heard.of.or.observed.negative.consequences.for.co.workers.who.have.been.open.about.mental.health.issues.in.your.workplace." ] <- "neg_consequences_mh_workplace"
colnames(tech)[colnames(tech)=="Would.you.be.willing.to.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview." ] <- "ph_issue_interview"
colnames(tech)[colnames(tech)=="Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview." ] <- "mh_issue_interview"
colnames(tech)[colnames(tech)=="Do.you.feel.that.being.identified.as.a.person.with.a.mental.health.issue.would.hurt.your.career."   ] <- "mh_identity_hurt_career"
colnames(tech)[colnames(tech)=="Do.you.think.that.team.members.co.workers.would.view.you.more.negatively.if.they.knew.you.suffered.from.a.mental.health.issue."   ] <- "coworkers_view_negatively"
colnames(tech)[colnames(tech)=="How.willing.would.you.be.to.share.with.friends.and.family.that.you.have.a.mental.illness."  ] <- "share_with_friends_family"
colnames(tech)[colnames(tech)=="Have.you.observed.or.experienced.an.unsupportive.or.badly.handled.response.to.a.mental.health.issue.in.your.current.or.previous.workplace." ] <- "unsupportive_mh_incident"
colnames(tech)[colnames(tech)=="Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health."] <- "mh_serious_ph"

# Removing outliers from age variable; individuals above 60 are outliers -------
tech <- subset(tech, age < 60 & age > 18)

#Ordering factors by magnitude ---------------------------------------------------

tech$num_employees = factor(tech$num_employees, 
                       ordered = TRUE, 
                       levels = c('1 to 5', 
                                  '6 to 25','26-100', 
                                  '100-500',
                                  '500-1000','More than 1000'))

pretty_numbers <- function(num_employees, value){
  x = paste0("Number of employees: ", num_employees[[value]])
  return(x)
}
  
tech$mh_benefits = factor(tech$mh_benefits,
                          ordered = TRUE,
                          levels = c('Not eligible for coverage / N/A',
                                     "I don't know",
                                     "No",
                                     "Yes"))

tech$mh_work_leave = factor(tech$mh_work_leave,
                          ordered = TRUE,
                          levels = c("I don't know",
                                     "Very easy",
                                     "Somewhat easy",
                                     "Neither easy nor difficult",
                                     "Somewhat difficult",
                                     "Very difficult"))

tech$anonymity_protected = factor(tech$anonymity_protected,
                                  ordered = TRUE,
                                  levels = c("I don't know",
                                             "No",
                                             "Yes"))

tech$interferes_when_not_treated = factor(tech$interferes_when_not_treated,
                                      ordered = TRUE,
                                      levels = c("Not applicable to me",
                                                 "Never",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Often"))


tech$mh_serious_ph = factor(tech$mh_serious_ph,
                            ordered = TRUE,
                            levels = c("No",
                                       "I don't know",
                                       "Yes"))

tech$mh_identity_hurt_career = factor(tech$mh_identity_hurt_career,
                                      ordered = TRUE,
                                      levels = c("No, it has not",
                                                 "No, I don't think it would",
                                                 "Maybe",
                                                 "Yes, I think it would",
                                                 "Yes, it has"))

tech$discuss_mh_employer_neg_consequences = factor(tech$discuss_mh_employer_neg_consequences,
                                                   ordered = TRUE,
                                                   levels = c("No",
                                                              "Maybe",
                                                              "Yes"))

# Recoding 0/1 responses as Yes/No so humans understand it ----------

tech$ever_treatment[tech$ever_treatment=="0"] <- "No"
tech$ever_treatment[tech$ever_treatment=="1"] <- "Yes"


