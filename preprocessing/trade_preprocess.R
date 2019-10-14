library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(DT)
library(GGally)
library(randomForest)
library(stringr)
library(udpipe)
library(tidyverse)
library(tidytext)

setwd('~/Dropbox/Press release')
source("~/github/GlobalDataCenter/Analysis/preprocess_functions.R")

a_2018 <- read_csv('data/rawdata/trade2018_statement.csv')%>% 
  select('chamber', 'congress', 'date', 'name', 'party', 'state', 'title', 'contents')
a_2018$date <- as.Date(a_2018$date)
a_2018 <- filter(a_2018, date < as.Date("2018-08-30"))
a_2017 <- read_csv('data/rawdata/trade2017_statement.csv')%>% 
  select('chamber', 'congress', 'date', 'name', 'party', 'state', 'title', 'contents')
a_2016 <- read_csv('data/rawdata/trade2016_statement.csv')%>% 
  select('chamber', 'congress', 'date', 'name', 'party', 'state', 'title', 'contents')
a_2015 <- read_csv('data/rawdata/trade2015_statement.csv')%>% 
  select('chamber', 'congress', 'date', 'name', 'party', 'state', 'title', 'contents')

trade_df <- rbind(a_2018, a_2017, a_2016, a_2015)

trade_df$contents[(trade_df$contents == 'server error')] <- NA
sum(is.na(trade_df$contents))

trade_df <- trade_df[!duplicated(trade_df$title),]
trade_df <- trade_df[!duplicated(trade_df$url),]

trade_df$rawcontents <- trade_df$contents

trade_df$contents <- boiler_fun_press(trade_df$contents)
trade_df$contents = prep_fun(trade_df$contents)
trade_df$contents = prep_fun2(trade_df$contents)

nadf <- trade_df %>% filter(is.na(contents))

## regional_category 

midwest <- c('ND', 'SD', 'NE', 'KS',' MO', 'IA', 'MN', 'WI', 'MI', 'IL', 'IN', 'OH')
north <- c('CT', 'MA', 'RI', 'NY', 'NJ', 'PA', 'DE', 'MD', 'NH','VT', 'ME') # check MD and DE 
south <- c('VA', 'WV', 'KY', 'TN', 'NC', 'SC', 'FL', 'GA', 'AL', 'MS', 'LA', 'AK', 'TX', 'OK')
west <- c('WY', 'CO', 'UT', 'NV', 'ID', 'CA', 'OR', 'WA', 'AK', 'MT', 'NM', 'AZ', 'HI')
etc <- c('MP', 'PR', 'GU', 'MH', 'AS', 'DC')

## Seperating senate and house

senate <- trade_df %>% 
  filter(chamber == "Senate"&!is.na(contents)) %>%
  select('congress', 'date', 'name', 'party', 'state', 'title', 'contents') %>% 
  mutate(year = format(as.Date(date), "%Y"),
         month = format(as.Date(date), "%m"),
         day = format(as.Date(date), "%d")) %>% 
  mutate(region = ifelse(state %in% midwest, 'midwest',
                         ifelse(state %in% west, 'west',
                                ifelse(state %in% south, 'south', 'north'))))
senate$name <-  as.factor(senate$name)

house <- trade_df %>% 
  filter(chamber == 'House') %>% 
  select('congress', 'date', 'name', 'party', 'state', 'title', 'contents') %>% 
  mutate(year = format(as.Date(date), "%Y"),
         month = format(as.Date(date), "%m"),
         day = format(as.Date(date), "%d")) %>%
  mutate(region = ifelse(state %in% midwest, 'midwest',
                         ifelse(state %in% west, 'west',
                                ifelse(state %in% south, 'south', 
                                       ifelse(state %in% north, 'north','etc')))))
house$name <- as.factor(house$name)

## pos tagging

udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
senate_udpipe_output <- udpipe_annotate(udmodel_english, senate$contents)
house_udpipe_output <- udpipe_annotate(udmodel_english, house$contents)

save(senate_pose, file = "data/trade_senate_udipe.Rdata")
## using spacyr?

senate_pose <- data.frame(senate_udpipe_output) %>% 
  filter(upos == 'NOUN'| upos == 'ADJ')

house_pose <- data.frame(house_udpipe_output) %>% 
  filter(upos == 'NOUN'| upos == 'ADJ')

save(senate_pose, file = "data/trade_senat_pose.Rdata")
save(house_pose, file = "data/trade_housee_pose.Rdata")


## preprocessing for noun and adj

nested_house <- house_pose %>%
  select(doc_id, token) %>% 
  nest(token) %>% 
  mutate(noun = map(data, unlist),
         noun = map_chr(data, paste, collapse = " ")) 

nested_senate <- senate_pose %>%
  select(doc_id, token) %>% 
  nest(token) %>% 
  mutate(noun = map(data, unlist), 
         noun = map_chr(data, paste, collapse = " ")) 

nested_senate$noun = sub("c", " ", nested_senate$noun)
nested_senate$noun <- gsub("[]$*?[^{|\\#%&~_/<=>!,:;`\")(}@-],", " ", nested_senate$noun)
nested_senate$noun <- gsub("\"", " ", nested_senate$noun)
nested_senate$noun <- gsub("[(\\.)'’“”—…–•]", "", nested_senate$noun)
nested_senate$noun <- gsub("\n", "", nested_senate$noun)
nested_senate$noun <- gsub("today", "", nested_senate$noun)

nested_house$noun = sub("c", " ", nested_house$noun)
nested_house$noun <- gsub("[]$*?[^{|\\#%&~_/<=>!,:;`\")(}@-],", " ", nested_house$noun)
nested_house$noun <- gsub("\"", " ", nested_house$noun)
nested_house$noun <- gsub("[(\\.)'’“”—…–•]", "", nested_house$noun)
nested_house$noun <- gsub("\n", "", nested_house$noun)
nested_house$noun <- gsub("today", "", nested_house$noun)

house <- rowid_to_column(house)
names(house)[1] <- "doc_id"
nested_house$doc_id <- gsub("doc", "", nested_house$doc_id)
house <- merge(house, nested_house, by = "doc_id")
house$doc_id <- as.integer(house$doc_id)
house <- house[order(house$doc_id),] %>% 
  select(-c(doc_id, data))

senate <- rowid_to_column(senate)
names(senate)[1] <- "doc_id"
nested_senate$doc_id <- gsub("doc", "", nested_senate$doc_id)
senate <- merge(senate, nested_senate, by = 'doc_id')
senate$doc_id <- as.integer(senate$doc_id)
senate<- senate[order(senate$doc_id),] %>% 
  select(-c(doc_id, data))

save(senate, file = "data/trade senate.Rdata")
save(house, file = "data/trade house.Rdata")

