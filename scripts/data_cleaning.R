library(haven)
library(tidyverse)
library(microbenchmark)

# read in the raw data
survey_data<- read_dta("C:/Users/lumuc/Desktop/final 304/111.dta")

# Add the labels
survey_data <- labelled::to_factor(survey_data)

# select variables of interest we want to investigate
survey_data_reduced <- 
  survey_data %>% 
  select(q11,
         q13,
         q20,
         q2,
         q3,
         q4,) %>% 
  drop_na()

# clean out the no-response or the uselsss data for people choose not to answer in each variable
survey_data_reduced <- survey_data_reduced %>% 
  filter(q11 != "(-9) Don't know / Undecided" & q11 != "(8) Will not vote" & q11 != "(-8) Refused" &q11 != "(10) Will spoil ballet") %>%
  filter(q13 != "(-9) Don't know" & q13 != "(-8) Refused") %>%
  filter(q3 != "(3) Other") %>%
  filter(q20 != "(-9) Aware of leader, but don't know" & q20 != "(-8) Refused" & q20 != "(-6) Don't know leader")

# create the variable q11 if vote for Liberal, use 1 to represent. If not, use 0. 
survey_data_reduced <-
  survey_data_reduced %>%
  mutate(q11 = ifelse(q11 == "(1) Liberal (Grits)", 1, 0))

# change the type of q2 and q20 from character to numeric.
survey_data_reduced$q2 <- as.numeric(as.character(survey_data_reduced$q2))

survey_data_reduced$q20 <- as.numeric(as.character(survey_data_reduced$q20))

# creating 4 age groupings instead of using date of birth 
survey_data_reduced<- survey_data_reduced %>% 
  mutate(q2 = case_when(as.numeric(q2) >= 1995 ~ "18-24", 
                        as.numeric(q2) >= 1975 ~ "25-44",
                        as.numeric(q2) >= 1955 ~ "45-64",  
                        as.numeric(q2) <= 1954 ~ "65+"))

# creating 4 levels of rating for variable q20
survey_data_reduced <- 
  survey_data_reduced %>%
  mutate(q20 = case_when(
    between(q20, 0, 40) ~ "bad",
    between(q20, 41, 60) ~ "ok",
    between(q20, 61, 80) ~ "good",
    between(q20, 81, Inf) ~ "perfect"))

# The following code is used for the another model called model 2. 
survey_data_reduced_2 <- 
  survey_data %>% 
  select(q11,
         q13,
         q20,
         q2,
         q3,
         q4,) %>% 
  drop_na()


survey_data_reduced_2 <- survey_data_reduced_2 %>% 
  filter(q11 != "(-9) Don't know / Undecided" & q11 != "(8) Will not vote" & q11 != "(-8) Refused" &q11 != "(10) Will spoil ballet") %>%
  filter(q13 != "(-9) Don't know" & q13 != "(-8) Refused") %>%
  filter(q3 != "(3) Other") %>%
  filter(q20 != "(-9) Aware of leader, but don't know" & q20 != "(-8) Refused" & q20 != "(-6) Don't know leader")

# creating the variable q11 if vote for the Conservative, use 1 to represent. If not, use 0. 
survey_data_reduced_2 <-
  survey_data_reduced_2 %>%
  mutate(q11 = ifelse(q11 == "(2) Conservatives (Tory, PCs, Conservative Party of Canada)", 1, 0))

survey_data_reduced_2$q2 <- as.numeric(as.character(survey_data_reduced_2$q2))

survey_data_reduced_2$q20 <- as.numeric(as.character(survey_data_reduced_2$q20))


survey_data_reduced_2<- survey_data_reduced_2 %>% 
  mutate(q2 = case_when(as.numeric(q2) >= 1995 ~ "18-24", 
                        as.numeric(q2) >= 1975 ~ "25-44",
                        as.numeric(q2) >= 1955 ~ "45-64",  
                        as.numeric(q2) <= 1954 ~ "65+"))


survey_data_reduced_2 <- 
  survey_data_reduced_2 %>%
  mutate(q20 = case_when(
    between(q20, 0, 40) ~ "bad",
    between(q20, 41, 60) ~ "ok",
    between(q20, 61, 80) ~ "good",
    between(q20, 81, Inf) ~ "perfect"))

# create the new data after being cleaned.
write_csv(survey_data_reduced, "C:/Users/lumuc/Desktop/final 304/cleaned_data.csv")
write_csv(survey_data_reduced_2, "C:/Users/lumuc/Desktop/final 304/cleaned_data_2.csv")