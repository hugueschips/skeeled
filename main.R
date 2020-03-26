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
library(tibble)
library(mgsub)
library(stopwords) # stopwords
library(maps) # cities


########## 1.0 Load files
jobs <- read.csv(file = './jobs.csv', header = TRUE, skip = 1, fileEncoding = "CP1252") %>% 
  as.tibble
titles <- read.csv(file = './titles.csv', header = TRUE, skip = 0, fileEncoding = "CP1252") %>% 
  as.tibble

########## 1.1 Add type of contract column to jobs
jobs <- jobs %>% 
  mutate(duration = as.Date(To) - as.Date(From)) %>%
  mutate(Type = if_else(duration<0, 'permanent', 
                        if_else(duration<364, 'less than a year', 
                                if_else(duration>365, 'fixed term (more than a year)', 'one year'))
                        ) %>% factor) %>%
  select(-duration) %>%
  as.tibble

########### 2.0 Prepare a list of cities to remove. Let's assume cities are only at the end
data(world.cities)
cities <- paste0(" ", tolower(world.cities$name), "$")

########### 2.1 Prepare a list of stopwords to remove
spaced_stopwords <- paste0(' ', stopwords(), ' ')

########### 2.2 Prepare a list of manually spotted words to remove (optional)
words_to_remove <- c('offices')

########### 2.3 Clean JobTitles in jobs
### To clean the JobTitles column, we
###### convert it to lower case
###### remove (m/f), m-f, etc...
###### remove punctuation
###### remove senior, sr., etc...
###### remove fixed term, full time, etc... using versions without punctuation
###### remove stopwords
###### remove cities
###### remove extra spaces
###### convert as factor
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
  # mutate(tempTitles = mgsub(tempTitles, words_to_remove, " ", recycle = TRUE)) %>%
  # mutate(tempTitles = mgsub(tempTitles, words_to_remove, " ", recycle = TRUE)) %>%
  mutate(tempTitles = mgsub(tempTitles, spaced_stopwords, " ", recycle = TRUE)) %>%
  mutate(tempTitles = mgsub(tempTitles, spaced_stopwords, " ", recycle = TRUE)) %>%
  ### we have do it twice because of space before and after the words
  mutate(tempTitles = mgsub(tempTitles, cities, " ", recycle = TRUE)) %>%
  mutate(tempTitles = gsub('  ', ' ', tempTitles)) %>%
  mutate(tempTitles = gsub('  ', ' ', tempTitles)) %>%
  mutate(tempTitles = gsub('  ', ' ', tempTitles)) %>%
  mutate(tempTitles = gsub('  ', ' ', tempTitles)) %>%
  mutate(tempTitles = tempTitles %>% trimws) %>%
  mutate(CleanedTitle = factor(tempTitles)) %>%
  select(-tempTitles)
 jobs

########### 3.0 Add AlternativeTitle column to jobs
matchingWordsRate <- function(textA, textB) {
  stra <- str_split(textA, " ") %>% unlist
  strb <- str_split(textB, " ") %>% unlist
  matches <- intersect(stra, strb) %>% length
  return(matches/(length(strb) + length(stra)))
}

closestTitle <- function(textA) {
  out <- titles %>% select(OccupationTitle)
  out$Score <- mapply(matchingWordsRate, titles$OccupationTitle, textA)
  out <- out %>% arrange(-Score)
  return(out$OccupationTitle[[1]] %>% as.character)
}

jobs$AlternativeTitle <- sapply(jobs$CleanedTitle, closestTitle)


########### 4.0 Prepare a list of languages meant to be detected
language <- c('french', 'english', 'german', 'spanish', 
          'portuguese', 'chinese', 'italian', 'danish')
example <- jobs$Requirements[[2]] %>% as.character

detected_languages <- mapply(grepl, language, example)
which(detected_languages) %>% names %>% paste(collapse = ", ")

detectLanguages <- function(x) {
  detected_languages <- mapply(grepl, language, x)
  out <- which(detected_languages) %>% names %>% paste(collapse = ", ")
  return(out)
}
detectLanguages(example)

########### 4. Extract required language(s) in jobs
jobs$Languages <- sapply(tolower(jobs$Requirements), detectLanguages)

jobs %>% select(JobTitle, AlternativeTitle, Type, Languages) %>%
  show





