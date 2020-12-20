knitr::opts_chunk$set(echo = FALSE, warning=FALSE)
library(dplyr)
library(knitr)
library(ggplot2)
library(tidyverse)
library(scales)
library(kableExtra)


survey_data <- read.csv("C:/Users/lumuc/Desktop/final 304/cleaned_data.csv")   #read in the data 
survey_data_2 <- read.csv("C:/Users/lumuc/Desktop/final 304/cleaned_data_2.csv")  #read in the data 

survey_data <- survey_data %>% #Select variables of interest 
  select(q20,
         q11,
         q13,
         q3,
         q2,
         q4)

# logistic regression for model 1
my_logit <- glm(
  q11 ~ q13 + q20 + q3 + q2 + q4,
  data = survey_data, family = "binomial"
)


survey_data_2 <- survey_data_2 %>% #Select variables of interest 
  select(q20,
         q11,
         q13,
         q3,
         q2,
         q4)

 # logistic regression for model 2
my_logit_2 <- glm(
  q11 ~ q13 + q20 + q3 + q2 + q4,
  data = survey_data_2, family = "binomial"
)

# get the summary for both the model 1 and model 2.
summary(my_logit)
summary(my_logit_2)

