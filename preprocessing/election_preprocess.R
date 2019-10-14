## Loading package

library(dplyr)
library(tidyverse)
library(naniar)
library(reshape)
library(stringr)

## LOADING data

setwd('~/Dropbox/Press release')
# senate_df is noun text data
load('data/trade senate.Rdata')

## senate by state

head(stateSenate)

load('data/election data/1976-2018-senate.RData')
# stateSenate is election data
stateSenate <- x %>% 
  select("year", "state_po", "office", "candidate", "party","candidatevotes", "totalvotes") %>% 
  #filter(2017 >= year & year >= 2014) %>% 
  replace_with_na_at(.vars = c("candidate","party"),condition = ~.x == 'NA')
stateSenate <- stateSenate[complete.cases(stateSenate), ]
stateSenate <- mutate(stateSenate, percentage = stateSenate$candidatevotes/stateSenate$totalvotes * 100)

## matching name

stateSenate$candidate <- gsub(", Jr.", "", stateSenate$candidate)
stateSenate$candidate <- gsub("Robert P Casey Jr", "Robert P Casey", stateSenate$candidate)
stateSenate$candidate <- gsub("Joseph Manchin III", "Joseph Manchin", stateSenate$candidate)
stateSenate$candidate <- str_extract(stateSenate$candidate, '[A-Za-z]{3,15}+$')
senate$name <- gsub("Joe Manchin III", "Joseph Manchin", senate$name)
senate$name <- str_extract(senate$name, '[A-Za-z]{3,15}+$')
PRname <- unique(senate$name)
ElectionName <- c(unique(stateSenate$candidate))
setdiff(PRname, ElectionName)

## changing party
stateSenate$party<- gsub("working families", "D", stateSenate$party)
stateSenate$party<- gsub("democratic-farmer-labor", "D", stateSenate$party)
stateSenate$party<- gsub("democrat", "D", stateSenate$party)
stateSenate$party<- gsub("republican", "R", stateSenate$party)
stateSenate$party<- gsub("independent", "I", stateSenate$party)

## matching party ex

s14 <- stateSenate %>% 
  filter(year == 2014 & percentage > 20, candidate %in% PRname) %>% 
  select(c('candidate', "state_po", "party" , "percentage")) 
colnames(s14) <- c('name', 'state', 'party', 'percentage')
s14["electionyear"] <- 14 

## Only using 2016 seantor election - check election senator name

# We only consider the 115th Senate election, so we will list the members of the 115th Senate.

s16 <- stateSenate %>% 
  filter(year == 2016, candidate %in% PRname)
sum16 <- aggregate(s16$candidatevotes , by=list(candidate=s16$candidate), FUN=sum)
s16 <- s16[!duplicated(s16$candidate),]
s16 <- merge(s16, sum16, key = candidate) %>% 
  select(-c('candidatevotes', 'percentage'))
colnames(s16)[7] <- 'votes'
s16 <- mutate(s16, sen16 = s16$votes/s16$totalvotes * 100) %>% 
  filter(sen16 > 20) %>% 
  select(c('candidate', "state_po", "party", "sen16")) 
colnames(s16) <- c('name', 'state', 'party', 'percentage')
s16["electionyear"] <- 16

s18 <- stateSenate %>% 
  filter(year == 2018, candidate %in% PRname)
sum18 <- aggregate(s18$candidatevotes , by=list(candidate=s18$candidate), FUN=sum)
s18 <- s18[!duplicated(s18$candidate),]
s18 <- merge(s18, sum18, key = candidate) %>% 
  select(-c('candidatevotes', 'percentage'))
colnames(s18)[7] <- 'votes'
s18 <- mutate(s18, sen18 = s18$votes/s18$totalvotes * 100) %>% 
  filter(sen18 > 20)%>% 
  select(c('candidate', "state_po", "party", "sen18")) 
colnames(s18) <- c('name', 'state', 'party', 'percentage')
s18["electionyear"] <- 18

## Adding senator election data

senate_df <- rbind(s14,s16, s18)

## overall election data by county

election_contest <- read_csv('data/election data/2018-elections-unoffical-master/election-context-2018.csv')
statedf <- group_by(election_contest, state) %>% 
  summarise(trump16 = sum(trump16), clinton16 = sum(clinton16), otherpres16 = sum(otherpres16), romney12 = sum(romney12), 
            obama12 = sum(obama12), other12 = sum(otherpres12), demhouse16 =  sum(demhouse16), rephouse16 = sum(rephouse16),
            otherhouse16 = sum(otherhouse16), repgov16 = sum(repgov16), demgov16 = sum(demgov16), demsen16 = sum(demsen16),
            repsen16 = sum(repsen16), repgov14 = sum(repgov14),demgov14 =sum(demgov14), total_population = sum(total_population))

statedf$state <- c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA','ME', 'MD', 'MA', 'MI', 'MN','MS', 'MO', 'MT', 'NE','NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK','OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
statedf <- mutate(statedf, 
                  voter16 = statedf$trump16 + statedf$clinton16 + statedf$otherpres16,
                  voter12 = statedf$romney12 + statedf$obama12 + statedf$other12,
                  house16 = statedf$demhouse16 + statedf$rephouse16 + statedf$otherhouse16,
                  gov16 = statedf$repgov16 + statedf$demhouse16,
                  gov14 = statedf$repgov14 + statedf$demgov14)
statedf <- mutate(statedf,                  
                  pre16 = statedf$trump16/statedf$voter16*100,
                  pre12 = statedf$obama12/statedf$voter12*100,
                  house16d = statedf$demhouse16/statedf$house16*100,
                  gov14d = statedf$demgov14/statedf$gov14*100,
                  gov16d = statedf$demgov16/statedf$gov16*100) %>% 
  select(c('state', 'pre16', 'pre12'))

## We don`t have Alaska, adding AL`s election data`
al <- data.frame('AK', 51.3, 40.8)
colnames(al) <- c("state", "pre16", "pre12")
result <- rbind(statedf, al)

senate_df <- merge(senate,senate_df, by=c("state", 'name', 'party'), all.x = TRUE)
senate_df <- senate_df[!duplicated(senate_df$content),]
senate_df <- merge(senate_df, result, by=c("state"), all.x = TRUE) 

## missing electionyear and percentage fix
apply(senate_df, 2, function(x) any(is.na(x)))

unique(senate_df[is.na(senate_df$electionyear),]$name)
unique(senate_df[is.na(senate_df$percentage),]$name)

A <- filter(senate_df, name == "King")
na.names <- unique(senate_df[is.na(senate_df$percentage),]$name)
for(i in 1:length(na.names)){
  print(unlist(senate[senate$name == na.names[i], 'electionyear']))
}

## 2017 United States Senate by-election in Alabama

senate_df[senate_df$name == na.names[1], 'electionyear'] <- 17
senate_df[senate_df$name == na.names[1], 'percentage'] <- 50.0
na.names[2]
i = 2
sub <- subset(senate_df, senate_df$name == na.names[i])
sub[is.na(sub$electionyear), c('title', 'date', 'electionyear')]
senate_df[senate_df$name == na.names[i] & is.na(senate_df$electionyear), 'electionyear'] <- 18
senate_df[senate_df$name == na.names[i] & is.na(senate_df$percentage), 'percentage'] <- 54.31433
i = 3
sub <- subset(senate_df, senate_df$name == na.names[i])
senate_df[senate_df$name == na.names[i] & is.na(senate_df$electionyear), 'electionyear'] <- 18
senate_df[senate_df$name == na.names[i] & is.na(senate_df$percentage), 'percentage'] <- 53.63
i = 4
sub <- subset(senate_df, senate_df$name == na.names[i])
sub[is.na(sub$electionyear), c('state', 'title', 'date', 'electionyear')]
senate_df[senate_df$name == na.names[i] & is.na(senate_df$electionyear), 'electionyear'] <- 18
senate_df[senate_df$name == na.names[i] & is.na(senate_df$percentage), 'percentage'] <- 67.31946

## check the fix
sum(apply(senate_df, 2, function(x) any(is.na(x))))==0

save(senate_df, file = "data/senate-merging-election.Rdata")

## Loading senator list
senate_list <- read_csv('data/senator1518.csv')
retireS16 <- na.omit(senate_list)
senate_list$last_name <- gsub("Joe Manchin III", "Joseph Manchin", senate_list$last_name)
senate_list$last_name <- str_extract(senate_list$last_name, '[A-Za-z]{3,15}+$')
senate_list <- select(senate_list, last_name, party, state, title, votes_with_party_pct)
colnames(senate_list)[4] <- "session"
colnames(senate_list)[1] <- "name"

## senator Cramer and Sinema are elected senator in 2018
setdiff(PRname, senate_list$name)

## making group A+B

changestate <- retireS16$state
changeS <- filter(senate_list, state %in% changestate & session == "Senator, 3rd Class")
alS <- filter(senate_list, name %in% c('Sessions', 'Jones'))
groupA <- rbind(changeS, alS) ## change senate
groupA <- merge(senate_df, groupA , by=c("state", 'name', 'party'))

sum(apply(groupA, 2, function(x) any(is.na(x))))==0
save(groupA, file = "data/groupAtext.Rdata")

### problem 1. Group A means failing election senators and being elected for the first time senators, but trade text doesn`t contain failing senators` noun.`

## Making group C and D
`%notin%` <- Negate(`%in%`)
groupC <- filter(senate_list, state %notin% changestate & session == "Senator, 3rd Class")
groupC <- merge(senate_df, groupC , by=c("state", 'name', 'party'))
sum(apply(groupC, 2, function(x) any(is.na(x))))
save(groupC, file = "data/groupCtext.Rdata")

groupD <- filter(senate_list, !session == "Senator, 3rd Class" & name %notin% c('Sessions', 'Jones'))
groupD <- merge(senate_df, groupD , by=c("state", 'name', 'party'))
sum(apply(groupD, 2, function(x) any(is.na(x))))
save(groupD, file = "data/groupDtext.Rdata")






## house by district data

load('data/election data/1976-2018-house.RData')



StateElection <- rbind(statePresident, stateSenate)


house <- house %>% filter(office == 'US House')
candidate <- group_by(house, candidate)
sum <- summarise(groupby(df, candidate), sum = sum(votes))
