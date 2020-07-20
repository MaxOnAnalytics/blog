
### Predicting the Next Best Fantasy WR Data Scraping

## Load libraries

library(rvest)
library(stringr)
library(MASS)
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(mice)
library(caret)
library(car)
library(ggplot2)
library(ggpmisc)
library(randomForest)
library(corrplot)
library(glmnet)
library(caretEnsemble)
library(jtools)
library(reactable)

select <- dplyr::select
filter <- dplyr::filter
group_by <- dplyr::group_by
summarise <- dplyr::summarise
mutate <- dplyr::mutate

## Create functions to scrape data from Pro Football Reference

# Scraping draft data

scrapePFR_draft <- function(start_year, end_year){
  data <- data.frame()
  for(i in start_year:end_year){
    cat('Loading Year: ', i, '\n')
    url <- paste("https://www.pro-football-reference.com/years/", as.character(i), "/draft.htm", sep = "")
    table <- url %>% read_html() %>% html_nodes("#drafts") %>% html_table() %>% first()
    for(j in 1:ncol(table)){
      colnames(table)[j] <- str_trim(paste(colnames(table)[j], table[1, j]))
      colnames(table)[j] <- ifelse(colnames(table)[j] == "", "X", colnames(table)[j])
      colnames(table)[j] <- tolower(str_replace(colnames(table)[j], " ", "_"))
    }
    table <- table %>% filter(pos != "Pos") %>% mutate(draft_year = i) %>% 
      rename(round = rnd, team_drafted = tm, position_drafted = pos, draft_age = age, school = `college/univ`) %>%
      mutate(round = as.numeric(round), 
             pick = as.numeric(pick), 
             draft_age = as.numeric(draft_age), 
             player = gsub("\\'", "", gsub("\\.", "", gsub("\\*", "", gsub("\\+", "", player)))), 
             school = gsub("\\'", "", gsub("\\.", "", gsub("\\*", "", gsub("\\+", "", school))))) %>%
      select(draft_year, round, pick, team_drafted, player, position_drafted, draft_age, school)
    data <- rbind(table, data)
  }
  return(data)
}


# Scraping NFL receiving production data

scrapePFR_receiving <- function(start_year, end_year){
  data <- data.frame()
  for(i in start_year:end_year){
    cat('Loading Year: ', i, '\n')
    url <- paste("https://www.pro-football-reference.com/years/", as.character(i), "/receiving.htm", sep = "")
    table <- url %>% read_html() %>% html_nodes("#receiving") %>% html_table() %>% first()
    for(j in 1:ncol(table)){
      colnames(table)[j] <- str_trim(colnames(table)[j])
      colnames(table)[j] <- ifelse(colnames(table)[j] == "", "X", colnames(table)[j])
      colnames(table)[j] <- tolower(gsub(pattern = " ", replacement = "_", colnames(table)[j]))
    }
    table <- table %>% filter(pos != "Pos") %>% mutate(season = i) %>% 
      rename(team = tm, position = pos, games = g, games_started = gs, 
             targets = tgt, catches = rec, yards = yds, tds = td, fumbles = fmb) %>%
      mutate(player = gsub("\\'", "", gsub("\\.", "", gsub("\\*", "", gsub("\\+", "", player)))), 
             age = as.numeric(age), 
             games = as.numeric(games), 
             games_started = as.numeric(games_started), 
             targets = as.numeric(targets), 
             catches = as.numeric(catches), 
             yards = as.numeric(yards), 
             tds = as.numeric(tds), 
             fumbles = as.numeric(fumbles)) %>%
      select(season, player, team, age, position, games, games_started, 
             targets, catches, yards, tds, fumbles)
    data <- rbind(table, data)
  }
  return(data)
}



# Scraping NFL fantasy data

scrapePFR_fantasy <- function(start_year, end_year){
  data <- data.frame()
  for(i in start_year:end_year){
    cat('Loading Year: ', i, '\n')
    url <- paste("https://www.pro-football-reference.com/years/", as.character(i), "/fantasy.htm", sep = "")
    table <- url %>% read_html() %>% html_nodes("#fantasy") %>% html_table() %>% first()
    for(j in 1:ncol(table)){
      colnames(table)[j] <- paste0(colnames(table)[j], "_", table[1, j])
      colnames(table)[j] <- str_trim(colnames(table)[j])
      colnames(table)[j] <- ifelse(colnames(table)[j] == "", "X", colnames(table)[j])
      colnames(table)[j] <- tolower(gsub(pattern = " ", replacement = "_", colnames(table)[j]))
      colnames(table)[j] <- ifelse(str_sub(colnames(table)[j], 1, 1) == "_",
                                   str_sub(colnames(table)[j], 2, str_length(colnames(table)[j])),
                                   colnames(table)[j])
    }
    table <- table %>% mutate(season = i) %>% filter(fantpos != "FantPos") %>%
      rename(team = tm, position = fantpos, games = games_g, games_started = games_gs, 
             fumbles = fumbles_fmb, fumbles_lost = fumbles_fl) %>%
      mutate(player = gsub("\\'", "", gsub("\\.", "", gsub("\\*", "", gsub("\\+", "", player)))))
    
    for(k in 5:ncol(table)){
      table[, k] <- as.numeric(as.character(table[, k]))
    }
    
    data <- rbind(table, data)
  }
  return(data)
}

## Scrape draft data from 2008 to 2020

draft_data <- scrapePFR_draft(start_year = 2008, end_year = 2020)
write.csv(draft_data, "NFL Draft Data 2008-2020.csv", row.names = F)

## Scrape NFL receiving stats from 2008 to 2019

nfl_prod <- scrapePFR_receiving(start_year = 2008, end_year = 2019)
write.csv(nfl_prod, "NFL WR Production 2008-2019.csv", row.names = F)

## Scrape NFL fantasy data from 2008 to 2019

nfl_fantasy <- scrapePFR_fantasy(start_year = 2008, end_year = 2019)
write.csv(nfl_fantasy, "NFL Fantasy 2008-2019.csv", row.names = F)



