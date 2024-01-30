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
start_date<-as.Date("2023-02-20")
end_date<-as.Date("2023-08-03")

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
Demographcis<-seq(start_date,end_date,by=1) %>% sample(.,n_patients,replace=TRUE) %>% 
  data.frame(
    ID=seq_len(n_patients)
    ,Enrolled=.
    ,Age=rnorm(n_patients,65,20)) %>% 
    mutate(DOB=Enrolled-Age)




