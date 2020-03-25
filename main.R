##################################################
# Author: Daniel Durrenberger
# Company: Skeeled
# Object: Technical test
# Date: 25.03.2020
# 
######################################

library(tidyverse)
library(dplyr)
library(dbplyr)
library(xgboost)
library(tibble)
library(ggplot2)
library(caret)
library(gsubfn)

########## 1.0 Load files
jobs <- read.csv(file = './jobs.csv', header = TRUE, skip = 1, fileEncoding = "CP1252") %>% 
  as.tibble
titles <- read.csv(file = './titles.csv', header = TRUE, skip = 0, fileEncoding = "CP1252") %>% 
  as.tibble

########## 1.1 Add type of contract column to jobs
jobs <- jobs %>% 
  mutate(duration = as.Date(To) - as.Date(From)) %>%
  mutate(type = if_else(duration<0, 'permanent', 
                        if_else(duration<364, 'less than a year', 
                                if_else(duration>365, 'fixed term (more than a year)', 'one year'))
                        ) %>% factor) %>%
  select(-duration) %>%
  as.tibble

########### 2.0 Clean job titles in titles
jobs <- jobs %>%
  mutate(tempTitles = tolower(JobTitle)) %>%
  mutate(tempTitles = tempTitles %>% 
           gsub('/(m/f/)', "", .) %>%
           gsub('m/f', "", .) %>%
           gsub('m-f', "", .) %>%
           gsub('/(f/m/)', "", .) %>%
           gsub('f/m', "", .) %>%
           gsub('f-m', "", .)) %>%  
  mutate(tempTitles = gsub('[[:punct:]]', ' ', tempTitles)) %>%
  mutate(tempTitles = tempTitles %>% 
          gsub('senior', "", .) %>%
          gsub('sr.', "", .) %>%
          gsub('junior', "", .) %>%
          gsub('jr.', "", .)) %>%
  mutate(tempTitles = tempTitles %>% 
          gsub('fixed term', "", .) %>%
          gsub('trainee', "", .) %>%
          gsub('part time', "", .) %>%
          gsub('full time', "", .) %>%
          gsub('permanent', "", .) %>%
          gsub('stage', "", .) %>%
          gsub('50\\%', "", .) %>%
          gsub('100\\%', "", .) ) %>%
  mutate(tempTitles = gsub(" for our offices at porto", "", tempTitles)) %>%
  mutate(tempTitles = gsub('  ', ' ', tempTitles)) %>%
  mutate(tempTitles = gsub('  ', ' ', tempTitles)) %>%
  mutate(tempTitles = gsub('  ', ' ', tempTitles)) %>%
  mutate(tempTitles = gsub('  ', ' ', tempTitles)) %>%
  mutate(tempTitles = tempTitles %>% gsub('^ ', '', .)) %>%
  mutate(tempTitles = tempTitles %>% gsub(' $', '', .)) %>%
  mutate(CleanedTitle = factor(tempTitles)) %>%
  select(-tempTitles)
 

########### 3.0 Add AlternativeTitle column to jobs




