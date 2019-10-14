#####################################
## title: "Public news about korea"
## author: "Juwon Oh"
## date: "2019년 6월 02일"
## output: html_document
#####################################

## Loading package and data

library(dplyr)
library(knitr)
library(readr)
library(stringr)
library(tm)
library(Cairo)
library(quanteda)
library(tidyverse)
library(tidytext)
library(lattice)
library(stm)        # Package for sturctural topic modeling
library(stmCorrViz) # Package for hierarchical correlation view of STMs
require(splines)
library("zoo")

# user specific working directory setup and loading news data

if(Sys.getenv("LOGNAME") == "park"){
  setwd("~/Dropbox/press release")
  source("~/Github/Sooahn/GlobalDataCenter/Analysis/preprocess_functions.R")
  
}else{
  setwd('~/Dropbox/Press release')
  source("~/github/GlobalDataCenter/Analysis/preprocess_functions.R")
}


#######################################################################
## Using stm for all data
#######################################################################

load('data/senate-merging-election.Rdata')
source('code/help.R')

## Independent seantors were excluded to leave only the topic distribution by party.
senate_df$party
senate.party <- subset(senate_df, party == 'D' | party =='R')

### Preprocessing

npout <- preproc(senate_party)
## Removing 23994 of 34331 terms (41340 of 1030972 tokens) due to frequency
## Your corpus now has 5792 documents, 10337 terms and 989632 tokens.

docs <- out$documents
vocab <- out$vocab
meta <- out$meta
### Preprocessing

out <- preproc(senate.party)
## Removing 23994 of 34331 terms (41340 of 1030972 tokens) due to frequency
## Your corpus now has 5792 documents, 10337 terms and 989632 tokens.

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

meta$trump.datrs <- as.numeric(meta$trump.dates)


congressPrevFitAll <- stm(out$documents, out$vocab, K=30,
                          prevalence=~ party+ s(trump.dates) + region + pre16 ,
                          max.em.its=75, data=out$meta, init.type="Spectral", seed=seed)

prepAll30 <- estimateEffect(1:30 ~ party + s(trump.dates, df = 30) + region + pre16, congressPrevFitAll,
                            meta = out$meta, uncertainty = "Global", prior = 0.01)


rank = order(unlist(Result$means))
topicRnk <- topic[rank, ]

Result <- plot(
  prepAll30,
  "party",
  method = "difference",
  cov.value1 = "Social Science",
  cov.value2 = "Computing",
  verbose.labels = F,
  ylab = "Expected Difference in Topic Probability by Party (with 95% CI)",
  xlab = "More Likely Republican                          Not Significant                       More Likely Democrat",
  main = "Effect of Subject on Topic Prevelance for Press Release",
  xlim = c(-0.1, 0.1)
)

# order based on Expected Topic Proportion
rank = order(unlist(Result$means))
topicRnk <- topic[rank, ]

plot(
  prep,
  "Subject",
  method = "difference",
  cov.value1 = "Social Science",
  cov.value2 = "Computing",
  verbose.labels = F,
  topics = topicRnk$TopicNumber,
  #labeltype = "custom",
  #custom.labels  = apply(topicNames$prob, 1, function(x) paste0(x, collapse = " + ")),
  ylab = "Expected Difference in Topic Probability by Subject (with 95% CI)",
  xlab = "More Likely Computing                           Not Significant                       More Likely Social Science",
  main = "Effect of Subject on Topic Prevelance for UNCC Research",
  xlim = c(-0.1, 0.1)
)


## Spline only - party and dates
seed = 19999 ## sample(1:10000, 1)
congressPrevFit <- stm(out$documents, out$vocab, K=30,
                        prevalence=~ party + s(trump.dates),
                        max.em.its=75, data=out$meta, init.type="Spectral", seed=seed)

save(congressPrevFit, file="congressPrevFit.RData")
## Explore major topics for significant variables

plot(congressPrevFit, type="summary", xlim=c(0,.4))

### Keywords in topics
plot(congressPrevFit, type="labels", topics=c(27,25,7,24))
print(sageLabels(congressPrevFitAll))

load('data/stm data/congressPrevFit.Rdata')
prep <- estimateEffect(1:30 ~ party + s(trump.dates), congressPrevFit,
                        meta = out$meta, uncertainty = "Global", prior = 0.01)
summary(prep, topics=27) 
summary(prep, topics=25)
summary(prep, topics=7)
summary(prep, topics=24)


## Regardless of the subject, the effects of the variables appear to be nearly identical.
## party ID is slightly significant, since presidential candidate Bernie Sanders`s primary. date 7 is significant.

## binary election only 

## state variable and date variable
load('data/stm data/congressPrevFitS.Rdata')
congressPrevFitS <- stm(out$documents, out$vocab, K=30,
                         prevalence=~ party + trump.binary + state,
                         max.em.its=75, data=out$meta, init.type="Spectral", seed=seed)
save(congressPrevFitS, file="congressPrevFitS.RData")

prepS <- estimateEffect(1:30 ~ party + trump.binary + state, CongressPrevFitS,
                         meta = out$meta, uncertainty = "Global", prior = 0.01)

summary(prepS, topics=27)
## Binary and AR state are significant variable. KS is slightly


plot(prepS, "trump.dates", method = "continuous", topics = 27,
     model = 27, printlegend = FALSE, xaxt = "n")


## Region variable, party variable and date variable.
congressPrevFitR <- stm(out$documents, out$vocab, K=30,
                         prevalence=~ region + party + s(trump.dates),
                         max.em.its=75, data=out$meta, init.type="Spectral", seed=seed)
save(congressPrevFitR, file="congressPrevFitR.RData")
prepR <- estimateEffect(1:30 ~ party + s(trump.dates) + region, congressPrevFitR,
                         meta = out$meta, uncertainty = "Global", prior = 0.01)
summary(prepR, topics=27)
##date 7/ NORTH / intercept / date1

## Let's put the important variables.(EXCEPTED state, binary)

congressPrevFitAll <- stm(out$documents, out$vocab, K=30,
                           prevalence=~ party+ s(trump.dates) + region + pre16 ,
                           max.em.its=75, data=out$meta, init.type="Spectral", seed=seed)
save(congressPrevFitAll, file="congressPrevFitAll.RData")
plot(congressPrevFitAll, type="summary", xlim=c(0,.4))


prepAll30 <- estimateEffect(1:30 ~ party + s(trump.dates, df = 30) + region + pre16, congressPrevFitAll,
                          meta = out$meta, uncertainty = "Global", prior = 0.01)

save(congressPrevFitAlldf30, file="congressPrevFitAlldf30.RData")

## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("2015-01-01"), as.Date("2018-08-30"), by = "day")
xx <- sort(unique(out$meta$trump.dates))
xx.dates <- as.Date(as.numeric(xx), origin ='2016-11-08')

## Create a time series object and define b-spline
set.seed(25)
myts <- ts(xx,     # random data
           start = c(2015, as.numeric(format(inds[1], "%j"))),
           frequency = 365)
tbs <- bs(xx, degree=30)
bsts14 <- ts(tbs[,14],     # random data
             start = c(2015, as.numeric(format(inds[1], "%j"))),
             frequency = 365)
bsts18 <- ts(tbs[,18],     # random data
             start = c(2015, as.numeric(format(inds[1], "%j"))),
             frequency = 365)
bsts26 <- ts(tbs[,26],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
bsts14
plot(bsts26, col="red")
lines(bsts18, col="green")
lines(bsts14, col="blue")

## which date is the maximum of bsts 7 degree?
time(bsts14)[which.max(bsts14)]
time(bsts18)[which.max(bsts18)]
time(bsts26)[which.max(bsts26)]

## Changes in topic proportion over time

major.topics <- c(10, 14, 30, 13)
topics = 30
monthseq <- seq(from=as.Date("2015-01-01"), to=as.Date("2018-08-29"), by="month")
monthnames <- months(monthseq)
model.stm.labels <- labelTopics(congressPrevFitAll, major.topics)
labelsname <- c('topic 10 : trade, steel, workers', 'topic 14 : american, jobs, agreement',
                'topic 30 : tariffs, trump, administration', 'topic 13 : program, infrastructure, funding')
labelsname[1]
par(mfrow=c(2,2))
for (i in 1:4){
  plot.estimateEffect(prepAll30, "trump.dates", method = "continuous", topics = major.topics[i], main = labelsname[i],
       printlegend = F, xaxt='n', ylim=c(-.10,.30))
  axis(1,at=c(-676,-309, 56, 420),labels=c(2015,2016,2017,2018),las=2)
  abline(v=c(-676,-309, 56, 420),lty=2,lwd=1,col="grey45") 
  # spline axis
  axis(1,at=c(-252,-134, 120),labels=c('s14','s18','s26'),las=2)
  abline(v=c(-252,-134, 120),col="blue")
  # president election
  axis(1,at=c(1),labels=c('President Election'),las=2)
  abline(v=c(1),col="green")
}

a
b <- unique(as.integer(meta$trump.dates))
sort(b)[-252]
xx.dates


## df 20

load('data/stm data/congressPrevFitAlldf30.Rdata')

congressPrevFitAlldf20 <- stm(out$documents, out$vocab, K=30,
                              prevalence=~ party+ s(trump.dates, df= 20) + region + pre16 ,
                              max.em.its=75, data=out$meta, init.type="Spectral", seed=seed)
save(congressPrevFitAlldf20, file="congressPrevFitAlldf20.RData")

prepAll20 <- estimateEffect(1:30 ~ party + s(trump.dates, df = 20) + region + pre16, congressPrevFitAlldf20,
                            meta = out$meta, uncertainty = "Global", prior = 0.01)
summary(prepAll20, topics=10)

myts <- ts(xx,     # random data
           start = c(2015, as.numeric(format(inds[1], "%j"))),
           frequency = 365)
tbs <- bs(xx, degree=20)
bsts9 <- ts(tbs[,9],     # random data
             start = c(2015, as.numeric(format(inds[1], "%j"))),
             frequency = 365)
bsts12 <- ts(tbs[,12],     # random data
             start = c(2015, as.numeric(format(inds[1], "%j"))),
             frequency = 365)
bsts17 <- ts(tbs[,17],     # random data
             start = c(2015, as.numeric(format(inds[1], "%j"))),
             frequency = 365)
plot(bsts9, col="red")
lines(bsts12, col="green")
lines(bsts17, col="blue")

## which date is the maximum of bsts 7 degree?
time(bsts9)[which.max(bsts9)]
time(bsts12)[which.max(bsts12)]
time(bsts17)[which.max(bsts17)]

## The date is almost identical between spline df 20 and spline df 30. For 30: 2016.162, 2016.49, 2017.184.
## In the case of 20: 2016.115, 2016.49, 2017.137. There is a slight difference in the p value and the coefficient.


## Using CausalImpact

library(CausalImpact)
time.points <- seq.Date(as.Date("2015-01-01"), as.Date("2018-08-30"), length.out = 100)
data <- zoo(means[[14]], time.points)

pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))

data <- cbind(a, means[[14]])

impact <- CausalImpact(data, pre.period, post.period)

## using 

summary(prepAll20, topics=27) 
summary(prepAll30, topics=25)
A <- summary(prepAll30, topics=10)
ab <- as.data.frame(A$tables)
B <-summary(prepAll, topics=1)
bc <- as.data.frame(B$tables)

#######################################################################
## Model A. Select losers and winners only (senators who lost seats and who won seats in the same state)
#######################################################################


## Preprocessing

load('data/groupAtext.Rdata')
start.date = "2016-01-01"
outA <- preproc(groupA)

## Removing 16309 of 24378 terms (30237 of 635446 tokens) due to frequency 
## Your corpus now has 3749 documents, 8069 terms and 605209 tokens.
docsA <- outA$documents
vocabA <- outA$vocab
metaA <- outA$meta

## Spline only - party and dates
seed = 19999 ## sample(1:10000, 1)
congressPrevFitA <- stm(outA$documents, outA$vocab, K=30,
                        prevalence=~ party + s(trump.dates),
                        max.em.its=75, data=outA$meta, init.type="Spectral", seed=seed)
save(congressPrevFitA, file="congressPrevFitA.RData")
## Explore major topics for significant variables

plot(congressPrevFitAAll, type="summary", xlim=c(0,.4))

### Keywords in topics
plot(congressPrevFitA, type="labels", topics=c(11,24,4,18))
print(sageLabels(congressPrevFitA))

## see the estimates- Topic 11: federal transportation / Topic 24: tariffs / Topic 4:manufacture industry / Topic 18: small business

prepA <- estimateEffect(1:30 ~ party + s(trump.dates), congressPrevFitA,
                        meta = outA$meta, uncertainty = "Global", prior = 0.01)
summary(prepA, topics=3) 
summary(prepA, topics=15)
summary(prepA, topics=14)
summary(prepA, topics=4)

## Regardless of the subject, the effects of the variables appear to be nearly identical.
## party I is significant, since presidential candidate Bernie Sanders`s primary.
## intercept, date 7 are slighly significant.

## binary election only 

## state variable and date variable

load('data/stm data/congressPrevFitAS.Rdata')
congressPrevFitAS <- stm(outA$documents, outA$vocab, K=30,
                         prevalence=~ party + trump.binary + state,
                         max.em.its=75, data=outA$meta, init.type="Spectral", seed=seed)
save(congressPrevFitAS, file="congressPrevFitAS.RData")
prepAS <- estimateEffect(1:30 ~ party + trump.binary + state, CongressPrevFitAS,
                         meta = outA$meta, uncertainty = "Global", prior = 0.01)

summary(prepAS, topics=11)## Binary and AR state are significant variable. KS is slightly

## Region variable, party variable and date variable

congressPrevFitAR <- stm(outA$documents, outA$vocab, K=30,
                         prevalence=~ region + party + s(trump.dates),
                         max.em.its=75, data=outA$meta, init.type="Spectral", seed=seed)
save(congressPrevFitAR, file="congressPrevFitAR.RData")
prepAR <- estimateEffect(1:30 ~ party + s(trump.dates) + region, congressPrevFitAR,
                         meta = outA$meta, uncertainty = "Global", prior = 0.01)
summary(prepAR, topics=11)## rep/south/west/date 7/


congressPrevFitAAll <- stm(outA$documents, outA$vocab, K=30,
                           prevalence=~ party + state + s(trump.dates) + region + pre12 + pre16 ,
                           max.em.its=75, data=outA$meta, init.type="Spectral", seed=seed)

save(congressPrevFitAAll, file="congressPrevFitAAll.RData")
load('data/stm data/congressPrevFitAAll.Rdata')

prepAAll <- estimateEffect(1:30 ~ party + state + s(trump.dates) + region + pre16, congressPrevFitAAll,
                         meta = outA$meta, uncertainty = "Global", prior = 0.01)
summary(prepAAll, topics=3)##rep/south/west/date 7/

## bs
## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("2015-01-01"), as.Date("2019-07-12"), by = "day")
xx <- sort(unique(outB$meta$trump.dates))

xx.dates <- as.Date(as.numeric(xx), origin ='2016-11-08')

#######################################################################
## Model C. Select senators who continue to serve before and after the election (including winners of the 2016 election)
#######################################################################
## ```{r, results='hide', warning=FALSE}

load('data/groupCtext.Rdata')
## Preprocessing

start.date = "2016-01-01"
outC <- preproc(groupC)

## Document-feature matrix of: 5,798 documents, 34,331 features (99.5% sparse).
## Your corpus now has 5792 documents, 10337 terms and 989632 tokens.Warning messages:
docsC <- outC$documents
vocabC <- outC$vocab
metaC <- outC$meta

## Spline only - party and dates
seed = 19999 ## sample(1:10000, 1)
congressPrevFitC <- stm(outC$documents, outC$vocab, K=30,
                       prevalence=~ party + s(trump.dates),
                       max.em.its=75, data=outC$meta, init.type="Spectral", seed=seed)
save(congressPrevFitC, file="congressPrevFitC.RData")
## Explore major topics for significant variables

par(bty="n",col="grey40",lwd=5)
plot(congressPrevFitCS, type="summary", xlim=c(0,.4))

### Keywords in topics
plot(congressPrevFitC, type="labels", topics=c(27,24,20,25,14,19,3,11))
print(sageLabels(congressPrevFitC))

## see the estimates- Topic 27: Trade agreement / Topic 8: China question / Topic 19: farm / Topic 20: tariff

prepC <- estimateEffect(1:30 ~ party + s(trump.dates), congressPrevFitC,
                       meta = outC$meta, uncertainty = "Global", prior = 0.01)
summary(prepC, topics=17) 
summary(prepC, topics=29)
summary(prepC, topics=23)
summary(prepC, topics=16)

## Regardless of the subject, the effects of the variables appear to be nearly identical.
## party R is significant, intercept, date 5,6,7 are slighly significant.

## binary election only 

## state variable and date variable
congressPrevFitCS <- stm(outC$documents, outC$vocab, K=30,
                        prevalence=~ party + trump.binary + state,
                        max.em.its=75, data=outC$meta, init.type="Spectral", seed=seed)
save(congressPrevFitCS, file="congressPrevFitCS.RData")

prepCS <- estimateEffect(1:30 ~ party + trump.binary + state, congressPrevFitCS,
                        meta = outC$meta, uncertainty = "Global", prior = 0.01)

summary(prepCS, topics=27)## Binary and AR state are significant variable. KS is slightly

plot(prepCS, "trump.binary", method = "continuous", topics = 27, printlegend = F)


## Region variable, party variable and date variable

congressPrevFitCR <- stm(outC$documents, outC$vocab, K=30,
                         prevalence=~ region + party + s(trump.dates),
                         max.em.its=75, data=outC$meta, init.type="Spectral", seed=seed)
save(congressPrevFitCR, file="congressPrevFitCR.RData")

plot(congressPrevFitCR, type="summary", xlim=c(0,.4))

prepCR <- estimateEffect(1:30 ~ party + s(trump.dates) + region, congressPrevFitCR,
                         meta = outC$meta, uncertainty = "Global", prior = 0.01)
summary(prepCR, topics=27)## north/date 7652


congressPrevFitCAll <- stm(outC$documents, outC$vocab, K=30,
                           prevalence=~ party + state + s(trump.dates, df = 20) + region + pre16 ,
                           max.em.its=75, data=outC$meta, init.type="Spectral", seed=seed)

save(congressPrevFitCALL, file="congressPrevFitCALL.RData")
plot(congressPrevFitCAll, type="summary", xlim=c(0,.4))

load('data/stm data/congressPrevFitCAll.Rdata')
prepCAll <- estimateEffect(1:30 ~ party + state + s(trump.dates) + region + pre12 + pre16, congressPrevFitCAll,
                         meta = outC$meta, uncertainty = "Global", prior = 0.01)

summary(prepCR, topics=27)## north/date 7652

## In summary, the most significant and strongly affecting variable is the north region variable.
## The time variables were shown to be in the order of spline 7, 6, 5, 2 in common.


## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("2015-01-01"), as.Date("2018-08-30"), by = "day")
xx <- sort(unique(outC$meta$trump.dates))
xx.dates <- as.Date(as.numeric(xx), origin ='2016-11-08')

## Create a time series object
set.seed(25)
myts <- ts(xx,     # random data
           start = c(2015, as.numeric(format(inds[1], "%j"))),
           frequency = 365)
tbs <- bs(xx, degree=10)
tbs
bsts1 <- ts(tbs[,1],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
bsts6 <- ts(tbs[,6],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
bsts8 <- ts(tbs[,8],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
bsts7 <- ts(tbs[,7],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
plot(bsts1)
lines(bsts6, col="green")
lines(bsts7, col="red")
lines(bsts8, col="blue")

## which date is the maximum of bsts 7 degree?
xx.dates[which.max(bsts6)]
xx.dates[which.max(bsts7)]
xx.dates[which.max(bsts8)]


#######################################################################
## Model D. Select senators who did not go through the 2016 election (only off-election senatros)
#######################################################################

load('data/groupDtext.Rdata')
## Preprocessing

start.date = "2016-01-01"
outD <- preproc(groupD)

## Removing 16309 of 24378 terms (30237 of 635446 tokens) due to frequency 
## Your corpus now has 3749 documents, 8069 terms and 605209 tokens.
docsD <- outD$documents
vocabD <- outD$vocab
metaD <- outD$meta

## Spline only - party and dates
seed = 19999 ## sample(1:10000, 1)
congressPrevFitD <- stm(outD$documents, outD$vocab, K=30,
                        prevalence=~ party + s(trump.dates),
                        max.em.its=75, data=outD$meta, init.type="Spectral", seed=seed)
save(congressPrevFitD, file="congressPrevFitD.RData")
## Explore major topics for significant variables

par(bty="n",col="grey40",lwd=5)
plot(congressPrevFitDAll, type="summary", xlim=c(0,.4))

### Keywords in topics
plot(congressPrevFitD, type="labels", topics=c(11,13,28,26))
print(sageLabels(congressPrevFitD))

## see the estimates- Topic 11: Trade agreement / Topic 13: tariffs / Topic 28:manufacture industry / Topic 9: small business

prepD <- estimateEffect(1:30 ~ party + s(trump.dates), congressPrevFitD,
                        meta = outD$meta, uncertainty = "Global", prior = 0.01)
summary(prepD, topics=5) 
summary(prepD, topics=7)
summary(prepD, topics=8)
summary(prepD, topics=20)

## Regardless of the subject, the effects of the variables appear to be nearly identical.
## party I is significant, since presidential candidate Bernie Sanders`s primary.
## intercept, date 7 are slighly significant.

## binary election only 

## state variable and date variable
congressPrevFitDS <- stm(outD$documents, outD$vocab, K=30,
                         prevalence=~ party + trump.binary + state,
                         max.em.its=75, data=outD$meta, init.type="Spectral", seed=seed)
save(congressPrevFitDS, file="congressPrevFitDS.RData")
prepDS <- estimateEffect(1:30 ~ party + trump.binary + state, CongressPrevFitDS,
                         meta = outD$meta, uncertainty = "Global", prior = 0.01)

summary(prepDS, topics=11)## Binary and AR state are significant variable. KS is slightly

## Region variable, party variable and date variable

congressPrevFitDR <- stm(outD$documents, outD$vocab, K=30,
                         prevalence=~ region + party + s(trump.dates),
                         max.em.its=75, data=outD$meta, init.type="Spectral", seed=seed)
save(congressPrevFitDR, file="congressPrevFitDR.RData")
prepDR <- estimateEffect(1:30 ~ party + s(trump.dates) + region, congressPrevFitDR,
                         meta = outD$meta, uncertainty = "Global", prior = 0.01)
summary(prepDR, topics=11)## rep/south/west/date 7/

congressPrevFitDAll <- stm(outD$documents, outD$vocab, K=30,
                           prevalence=~ party + s(trump.dates) + region + pre16 ,
                           max.em.its=75, data=outD$meta, init.type="Spectral", seed=seed)
save(congressPrevFitDAll, file="congressPrevFitDAll.RData")
load('data/stm data/congressPrevFitDAll.Rdata')

prepDAll <- estimateEffect(1:30 ~ party + s(trump.dates) + region + pre16, congressPrevFitDAll,
                           meta = outD$meta, uncertainty = "Global", prior = 0.01)
summary(prepDAll, topics=5) 

## bs
## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("2015-01-01"), as.Date("2019-07-12"), by = "day")
xx <- sort(unique(outB$meta$trump.dates))

xx.dates <- as.Date(as.numeric(xx), origin ='2016-11-08')

## Create a time series object
set.seed(25)
myts <- ts(xx,     # random data
           start = c(2015, as.numeric(format(inds[1], "%j"))),
           frequency = 365)
tbs <- bs(xx, degree=10)
tbs
bsts1 <- ts(tbs[,1],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
bsts6 <- ts(tbs[,6],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
bsts8 <- ts(tbs[,8],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
bsts7 <- ts(tbs[,7],     # random data
            start = c(2015, as.numeric(format(inds[1], "%j"))),
            frequency = 365)
plot(bsts1)
lines(bsts6, col="green")
lines(bsts7, col="red")
lines(bsts8, col="blue")

## which date is the maximum of bsts 7 degree?
xx.dates[which.max(bsts6)]
xx.dates[which.max(bsts7)]
xx.dates[which.max(bsts8)]


###########################################









   
plot(prepAll, covariate = s(trump.date), topics = c(1),
  model = congressPrevFit, method = "difference",
  cov.value1 = "Before Trump", cov.value2 = "After Trump",
  xlab = "Before Trump ... After Trump",
 main = "Effect of the Election of Trump")


## Understand and Explain

### Proportion of topics

par(bty="n",col="grey40",lwd=5)
plot(congressPrevFit, type="summary", xlim=c(0,.4))

### Keywords in topics

plot(congressPrevFit, type="labels", topics=c(26,28,20,25,30,3,16))
print(sageLabels(congressPrevFit))

### Topics labeling 

mod.out.corr <- topicCorr(congressPrevFit)
plot(mod.out.corr,  
     topics = c(1:30))

### EstimateEffect

##  - STM은 estimateEffect함수를 통해서 topical prevalence와 메타데이터들 사이의 관계를 관측한다. 사용자는 메타데이터의 변수를 지정하여 topic들을 estimate 한다. 이를 통해 특정 주제에 대한 문서의 비율을 볼 수 있으며, 주제에 따라 변수들이 어떤 상관관계를 가졌는지, 그 정도가 시간의 흐름에 따라 어떻게 변화하는지 볼 수 있다.
##  - estimateEffect 함수의 결과는 회귀 분석의 결과와 비슷하게 정리된다.
##  - 첫번째 예시는 토픽과 변수(씽크탱크)의 상관관계를 살펴본 것이다. Heritage foundation과 38North를 비교했다.
 
out$meta$party <- as.character(out$meta$party)
out$meta$state <- as.character(out$meta$state)
prep <- estimateEffect(1:30 ~ party+state+s(ndate), congressPrevFit, meta=out$meta, 
                       uncertainty="Global")

plot(prep, covariate="party", topics= c(26,28,20,25,30,3,16), model=congressPrevFit, 
     method="difference", cov.value1="R", cov.value2='D',
     xlab="Republicans vs. Democrats", main="Effect of Republicans vs. Democrats",
     xlim=c(-.30,.30))

## PLOT 함수에서 GGPLOT으로 바꾸기. 제대로 안나옴. 1) 토픽을 늘리건, 2) hLDA를 사용하건, 좀더 토픽이 심층적으로 나오게 살펴보기.
par(mar=c(1,1,1,1))
topics = 30
monthseq <- seq(from=as.Date("2015-01-01"), to=as.Date("2019-06-15"), by="month")
monthnames <- months(monthseq)

model.stm.labels <- labelTopics(congressPrevFit, 1:topics)
labelsname <- c('topic 1: Iran and North Korea', 'topic 2', 'topic 3', 'topic 4', 'topic 5', 'topic 6', 'topic 7', 'topic 8', 'topic 9', 'topic 10','topic 11', 'topic 12', 'topic 13', 'topic14: Agriculture', 'topic 15','topic 16: national security', 'topic 17', 'topic 18', 'topic 19', 'topic 20','topic 21', 'topic 22', 'topic 23', 'topic 24', 'topic 25: China and trade war', 'topic 26', 'topic 27', 'topic 28:TPP', 'topic 29', 'topic 30')

for (i in c(1:30))
{
  plot(prep, "date", method="continuous", topics = i, main = labelsname[i], printlegend = F,
       xaxt="n", xlab="Time", ylim=c(-.10,.30))
  axis(1, at=as.numeric(monthseq)-min(as.numeric(monthseq)), labels=monthnames)
}
## ```



########
