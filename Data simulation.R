#'---
#' title: "Example Data simulation"
#' author: 'Elena Camargo'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: true
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
script_seed<-401783
knitr::opts_chunk$set(
  echo = debug > -1,
  warning = debug > 0,
  message = debug > 0,
  class.output = "scroll-20",
  attr.output = 'style="max-height: 150px; overflow-y: auto;"'
)

# Libraries ----
library(ggplot2); # visualization
library(GGally);
library(rio);# simple command for importing and exporting
library(pander); # format tables
library(printr); # set limit on number of lines printed
library(broom); # allows to give clean data set
library(dplyr); #add dplyr library
library(survival); #time to event or "survival data"- that occurs over time 

options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);


#' # Simulated variables
# Variables ----
simdata <- function(start_date=as.Date("2020-02-20")
                    ,end_date=as.Date("2020-08-03")
                    ,follow_up=as.Date("2024-02-06")
                    ,Columns_to_keep =c ('ID','Enrolled','Follow_up','Race','Sex','DOB'
                                         ,'Date_of_progression','Date_of_death')
                    
                    ,n_patients=100
                    #SaveTo
                    ,Saveto = ''
){Demographics<-seq(start_date,end_date,by=1) %>% sample(.,n_patients,replace=TRUE) %>% 
  data.frame(
    ID=seq_len(n_patients)
    ,Enrolled=.
    #For 2/27 add a column for individual_end_date_followup that is exactly 2 years after enrolled
    #HINT: you can add integer to a date
    ,Age=rnorm(n_patients,65*365.25,20*365.25)
    ,Sex=sample(c("M","F"),n_patients,replace=TRUE)
    ,Race=sample(c("White","Hispanic or Latino","Black"
                   ,"American Indian, Aleutian or Eskimo"
                   ,"Hawaiian or Pacific Islander"
                   ,"other Asian"
                   , "Other"
                   ,"unknown"), 
                 n_patients,replace=TRUE,prob=c(0.6,0.18,0.13,0.06,0.01,0.01,
                                                0.02,0))
    #    ,Baseline_risk=rnorm(n_patients,0.005,0.001) %>% abs()
    #    ,Baseline_death_pre_progression=rnorm(n_patients,0.00001,0.001) %>% abs()
    #    ,Baseline_death_post_progression=rnorm(n_patients,0.005,0.001) %>% abs()
    
    ,Baseline_risk=rnorm(n_patients, 0.005,0.001)%>% abs()
    ,Baseline_death_pre_progression=rnorm(n_patients, 0.000001,0.00001)%>% abs()
    ,Baseline_death_post_progression=rnorm(n_patients, 0.005,0.001)%>% abs()
    
  ) %>% 
  mutate(DOB=round(Enrolled-Age)
         ,Final_risk=Baseline_risk*ifelse(Sex=="F",0.8,1)
         ,Final_death_pre_progression=Baseline_death_pre_progression*ifelse(Sex=="F",0.8,1)
         ,Final_death_post_progression=Baseline_death_post_progression*ifelse(Sex=="F",0.8,1)
         ,Day_of_progression=(rgeom(n=n(), prob=Final_risk)+1)
         ,Day_of_death_pre_progression=(rgeom(n=n(), prob=Final_death_pre_progression)+1)
         ,Day_of_death_post_progression=(Day_of_progression+rgeom(n=n(), prob=Final_death_post_progression)+1)
         ,Day_of_death=ifelse(Day_of_death_pre_progression<=Day_of_progression
                              ,Day_of_death_pre_progression
                              ,Day_of_death_post_progression)
         ,Date_of_progression=Enrolled+Day_of_progression
         ,Date_of_death=Enrolled+Day_of_death
         ,Date_of_death0=Date_of_death
         ,Date_of_progression=ifelse(
           Date_of_progression>pmin(follow_up,Date_of_death),NA,Date_of_progression) %>% as.Date()
         ,Follow_up=follow_up
         ,Date_of_death=ifelse(
           Date_of_death>Follow_up,NA,Date_of_death)%>% as.Date()
  );
if(!missing(Saveto)){export(Demographics[,Columns_to_keep]
                            ,Saveto,overwrite=TRUE)}
Demographics[,Columns_to_keep]
}

#debug(simdata)

#n_patients<-100
#start_date<-as.Date("2020-02-20")
#end_date<-as.Date("2020-08-03")
#follow_up<-as.Date("2024-02-06")
#Columns_to_keep <-c ('ID','Enrolled','Follow_up','Race','Sex','DOB'
#                     ,'Date_of_progression','Date_of_death')

#' # Functions
# Generate_event_old<-function(xx,threshold){(runif(xx)<threshold )%>% which() %>% 
#     min() %>% ifelse(is.infinite(.),NA,.)}
# 
# Generate_event(100,.05)

#' Example of wrap expression
#data.frame(sample(seq(start_date,end_date,by=1),n_patients,replace = TRUE))


#' Example of piped expression
#seq(start_date,end_date,by=1) %>% sample(n_patients,replace=TRUE) %>% 
 # data.frame

#' Example of piped expression - period is latest result
#seq(start_date,end_date,by=1) %>% sample(.,n_patients,replace=TRUE) %>% 
 # data.frame(.)



#' #'Example of debugging 
#' seq(start_date,end_date,by=1) %>% sample(n_patients,replace=TRUE) %>% {
#'   baz<-.
#'   browser()
#'   baz} %>% 
#'   data.frame


#' # Curly brackets
1/{sqrt(25)->foo; 
  if(pi>-Inf) {bar<-foo-2} else {bar<-runif(1)}
  message("hello Elena")
  bar
}
1/3

# Demographics data frame ----
#' Demographics
set.seed(script_seed)

Demog<-simdata(Saveto='Simulated_Data.xlsx')

# Exported Data ----
#export(Demographics[,Columns_to_keep],"Simulated_Data.xlsx",overwrite=TRUE)
#survfit(Surv(Day_of_progression)~Sex,data=Demog) %>% plot()

#' Different ways to subset a data column using subset and which
#+ Subsetting
# Demographics[Demographics$Date_of_progression > Demographics$Date_of_death,];
# which(Demographics$Date_of_progression > Demographics$Date_of_death);
# Demographics[which(Demographics$Date_of_progression > Demographics$Date_of_death),];
# subset(Demographics,Date_of_progression > Date_of_death);
# 
# sum(Demographics$Date_of_progression > Demographics$Date_of_death,na.rm = TRUE)
# mean(Demographics$Date_of_progression > Demographics$Date_of_death,na.rm = TRUE)
# with(Demographics,sum(Date_of_progression > Date_of_death,na.rm = TRUE))

#' Progression of cancer and overall survival data
#Assuming p=50% risk of progression in 12 months, we can calculate the risk per day
1-(0.5)^(1/365)
#This frequency (0.00189) is equal to 1 in 500 odds


