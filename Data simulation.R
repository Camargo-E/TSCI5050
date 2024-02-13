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
knitr::opts_chunk$set(
  echo = debug > -1,
  warning = debug > 0,
  message = debug > 0,
  class.output = "scroll-20",
  attr.output = 'style="max-height: 150px; overflow-y: auto;"'
)


library(ggplot2); # visualization
library(GGally);
library(rio);# simple command for importing and exporting
library(pander); # format tables
#library(printr); # set limit on number of lines printed
library(broom); # allows to give clean data set
library(dplyr); #add dplyr library

options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);


#' # Simulated variables 
n_patients<-25
start_date<-as.Date("2020-02-20")
end_date<-as.Date("2020-08-03")
follow_up<-as.Date("2024-02-06")

#' # Functions
Generate_event<-function(xx,threshold){(runif(xx)<threshold )%>% which() %>% 
    min() %>% ifelse(is.infinite(.),NA,.)}
Generate_event(100,.05)

#' Example of wrap expression
data.frame(sample(seq(start_date,end_date,by=1),n_patients,replace = TRUE))


#' Example of piped expression
seq(start_date,end_date,by=1) %>% sample(n_patients,replace=TRUE) %>% 
  data.frame

#' Example of piped expression - period is latest result
seq(start_date,end_date,by=1) %>% sample(.,n_patients,replace=TRUE) %>% 
  data.frame(.)



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

#' Demographics
Demographics<-seq(start_date,end_date,by=1) %>% sample(.,n_patients,replace=TRUE) %>% 
  data.frame(
    ID=seq_len(n_patients)
    ,Enrolled=.
    ,Age=rnorm(n_patients,65,20)
    ,Sex=sample(c("M","F"),n_patients,replace=TRUE)
    ,Race=sample(c("White","Hispanic or Latino","Black","American Indian,
                   Aleutian or Eskimo","Hawaiian or Pacific Islander",
                   "other Asian", "Other","unknown"), 
                 n_patients,replace=TRUE,prob=c(0.6,0.18,0.13,0.06,0.01,0.01,
                                                0.02,0))
    ,Baseline_risk=rnorm(n_patients,0.002,0.0001)
   

    ) %>% 
    mutate(DOB=Enrolled-Age
           ,Final_risk=Baseline_risk*ifelse(Sex=="F",0.8,1)
           ,Day_of_progression=as.numeric(follow_up-Enrolled) %>% 
             mapply(Generate_event,.,Final_risk)
           #%>% {which(runif(.)<Final_risk}%>% 
           #  which() %>% min() %>% ifelse(is.infinite(.), NA,.)
           # ,ifelse(is.infinite(Day_of_progression),NA,Day_of_progression)
           );



Enrolled<-Demographics$Enrolled[10]
Final_risk<-Demographics$Final_risk[10]
Day_of_progression<-as.numeric(follow_up-Enrolled) %>% {runif(.)<Final_risk} %>% 
  which() %>% min()



#' Progression of cancer and overall survival data
#Assuming p=50% risk of progression in 12 months, we can calculate the risk per day
1-(0.5)^(1/365)
#This frequency (0.00189) is equal to 1 in 500 odds


