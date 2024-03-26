#'---
#' title: "Example Data analysis"
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


library(ggplot2); # visualisation
library(GGally);
library(rio);# simple command for importing and exporting
library(pander); # format tables
library(printr); # set limit on number of lines printed
library(broom); # allows to give clean dataset
library(dplyr); #add dplyr library
library(survival);
library(survminer)

options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
panderOptions('missing','-');
panderOptions('table.alignment.default','left');
panderOptions('table.alignment.rownames','right')
Inputdata<-"Simulated_Data.xlsx"

# Data Columns
Data_columns <- c(
  start='Enrolled',
  followup='Follow_up',
  dob='DOB',
  date1='Date_of_progression',
  date2='Date_of_death'
)

Covariates<-c(
  "Race","Sex"
)

# Import data ----

#' # Import Data
#' 
#' 
#+ this is where we import data
dat0<-import(Inputdata) %>% 
  rename(any_of(Data_columns)) %>% 
  mutate(age=as.numeric((start-dob)/365.25),
         maxfollowup=as.numeric(followup-start),
         censor1=!is.na(date1),
         censor2=!is.na(date2),
         #event1 and event2 are days that have elapsed since enrollment
         #censor is event occurred, if occurred= TRUE, if not =FALSE
         event2=coalesce(as.numeric(date2-start),maxfollowup),
         event1=coalesce(as.numeric(date1-start),event2)
         )
head(dat0)

fit0<-survfit(Surv(event1,censor1)~Sex,dat0)
