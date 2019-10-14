#########################
## package loading
#########################
library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(lattice)
library(wesanderson)
library(ggplot2)
library(ggforce)
library(gridExtra)
library(kableExtra)
require(NetworkChange) 

## loading data

setwd('~/Dropbox/Press release/data')
load('senate.Rdata')
load('house.Rdata')

## regional_category 

midwest <- c('ND', 'SD', 'NE', 'KS',' MO', 'IA', 'MN', 'WI', 'MI', 'IL', 'IN', 'OH')
north <- c('CT', 'MA', 'RI', 'NY', 'NJ', 'PA', 'DE', 'MD', 'NH','VT', 'ME') # check MD and DE 
south <- c('VA', 'WV', 'KY', 'TN', 'NC', 'SC', 'FL', 'GA', 'AL', 'MS', 'LA', 'AK', 'TX', 'OK')
west <- c('WY', 'CO', 'UT', 'NV', 'ID', 'CA', 'OR', 'WA', 'AK', 'MT', 'NM', 'AZ', 'HI')
etc <- c('MP', 'PR', 'GU', 'MH', 'AS', 'DC')

par(mfrow=c(1, 2))
hist(table(senate$name), col="brown")
hist(table(house$name), col="brown")


## monthly count
senate_monthly_n <- senate %>%
  group_by(year, month, party) %>%
  count() %>%
  mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
  mutate(president = ifelse(year=="2016"|year=="2015", "Obama", "Trump"))%>%
  ungroup()

house_monthly_n <- house %>%
  group_by(year, month, party) %>%
  count() %>%
  mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
  mutate(president = ifelse(year=="2016"|year=="2015", "Obama", "Trump"))%>%
  ungroup()

## library(dplyr)
## mutate(category=cut(a, breaks=c(-Inf, 0.5, 0.6, Inf), labels=c("low","middle","high")))

p0 <- ggplot(senate_monthly_n, aes(x=date, y=n, group=party, color=party)) + 
  geom_line() +
  ## geom_line(aes(x=date, y=n, col=source), size=2, alpha=0.6) +
  geom_point(alpha=0.6) +
  geom_vline(aes(xintercept=as.numeric(monthly_n$date)[monthly_n$date == "2017-01-01"][1]),
             linetype=4, colour="black") + 
  scale_shape_manual(values = c(1:length(unique(monthly_n$party)))) +
  scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
  labs(title="Senate", subtitle = "Press Release Data", y = "Absolute Frequency", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1))
p1 <- ggplot(house_monthly_n, aes(x=date, y=n, group=party, color=party)) + 
  geom_line() +
  ## geom_line(aes(x=date, y=n, col=source), size=2, alpha=0.6) +
  geom_point(alpha=0.6) +
  geom_vline(aes(xintercept=as.numeric(monthly_n$date)[monthly_n$date == "2017-01-01"][1]),
             linetype=4, colour="black") + 
  scale_shape_manual(values = c(1:length(unique(monthly_n$party)))) +
  scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
  labs(title="House", subtitle = "Press Release Data", y = "Absolute Frequency", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1))

pdf(file="plot/pressRelease_totalfreq.pdf",family="sans", width=12, height=12)
NetworkChange::multiplot(p0, p1)
dev.off()

## by state
## monthly count
senate_monthly_n <- senate %>%
  group_by(year, month, party, state) %>%
  count() %>%
  mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
  mutate(president = ifelse(year=="2016"|year=="2015", "Obama", "Trump"))%>%
  ungroup()

house_monthly_n <- house %>%
  group_by(year, month, party, state) %>%
  count() %>%
  mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
  mutate(president = ifelse(year=="2016"|year=="2015", "Obama", "Trump"))%>%
  ungroup()

p2 <- senate_monthly_n %>%
  ## filter(party == "D")%>%
  ggplot(aes(x=date, y=n, group=state, color=party)) + 
  geom_line() +
  ## geom_line(aes(x=date, y=n, col=source), size=2, alpha=0.6) +
  geom_point(alpha=0.6) +
  geom_vline(aes(xintercept=as.numeric(monthly_n$date)[monthly_n$date == "2017-01-01"][1]),
             linetype=4, colour="black") + 
  scale_shape_manual(values = c(1:length(unique(monthly_n$party)))) +
  ## scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
  labs(title="Senate", caption = "Press Release Data", y = "Absolute Frequency", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1)) +
  facet_wrap(~state)

p3 <- house_monthly_n %>%
  ## filter(party == "D")%>%
  ggplot(aes(x=date, y=n, group=state, color=party)) + 
  geom_line() +
  ## geom_line(aes(x=date, y=n, col=source), size=2, alpha=0.6) +
  geom_point(alpha=0.6) +
  geom_vline(aes(xintercept=as.numeric(monthly_n$date)[monthly_n$date == "2017-01-01"][1]),
             linetype=4, colour="black") + 
  scale_shape_manual(values = c(1:length(unique(monthly_n$party)))) +
  ## scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
  labs(title="House", caption = "Press Release Data", y = "Absolute Frequency", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1)) +
  facet_wrap(~state)

pdf(file="plot/pressRelease_senate_bystate.pdf",family="sans", width=12, height=10)
p2
dev.off()

pdf(file="plot/pressRelease_house_bystate.pdf",family="sans", width=12, height=10)
p3
dev.off()

## press release by name
## monthly count
senate_monthly_n <- senate %>%
  group_by(year, month, party, name) %>%
  count() %>%
  mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
  mutate(president = ifelse(year=="2016"|year=="2015", "Obama", "Trump"))%>%
  ungroup()

house_monthly_n <- house %>%
  group_by(year, month, party, name) %>%
  count() %>%
  mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
  mutate(president = ifelse(year=="2016"|year=="2015", "Obama", "Trump"))%>%
  ungroup()


p4 <- senate_monthly_n %>%
  ## filter(party == "D")%>%
  ggplot(aes(x=date, y=n, group=name, color=party)) + 
  geom_line() +
  ## geom_line(aes(x=date, y=n, col=source), size=2, alpha=0.6) +
  geom_point(alpha=0.6) +
  geom_vline(aes(xintercept=as.numeric(monthly_n$date)[monthly_n$date == "2017-01-01"][1]),
             linetype=4, colour="black") + 
  scale_shape_manual(values = c(1:length(unique(monthly_n$party)))) +
  ## scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
  labs(title="Senate", caption = "Press Release Data", y = "Absolute Frequency", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1)) +
  facet_wrap(~name)

p51 <- house_monthly_n %>%
  filter(party == "D")%>%
  ggplot(aes(x=date, y=n, group=name, color=party)) + 
  geom_line() +
  ## geom_line(aes(x=date, y=n, col=source), size=2, alpha=0.6) +
  geom_point(alpha=0.6) +
  geom_vline(aes(xintercept=as.numeric(monthly_n$date)[monthly_n$date == "2017-01-01"][1]),
             linetype=4, colour="black") + 
  scale_shape_manual(values = c(1:length(unique(monthly_n$party)))) +
  ## scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
  labs(title="House Democrats", caption = "Press Release Data", y = "Absolute Frequency", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1)) +
  facet_wrap(~name)
p52 <- house_monthly_n %>%
  filter(party == "R")%>%
  ggplot(aes(x=date, y=n, group=name, color=party)) + 
  geom_line() +
  ## geom_line(aes(x=date, y=n, col=source), size=2, alpha=0.6) +
  geom_point(alpha=0.6) +
  geom_vline(aes(xintercept=as.numeric(monthly_n$date)[monthly_n$date == "2017-01-01"][1]),
             linetype=4, colour="black") + 
  scale_shape_manual(values = c(1:length(unique(monthly_n$party)))) +
  ## scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
  labs(title="House Republicans", caption = "Press Release Data", y = "Absolute Frequency", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1)) +
  facet_wrap(~name)


pdf(file="plot/pressRelease_senate_byperson.pdf",family="sans", width=15, height=10)
p4
dev.off()

pdf(file="plot/pressRelease_house_byperson_dem.pdf",family="sans", width=20, height=15)
p51
dev.off()

pdf(file="plot/pressRelease_house_byperson_rep.pdf",family="sans", width=20, height=15)
p52
dev.off()


monthly_state <- senate %>%
  group_by(year, month) %>%
  summarise(midwest = sum(c('ND', 'SD', 'NE', 'KS',' MO', 'IA', 'MN', 'WI', 'MI', 'IL', 'IN', 'OH')==T),
            west = sum(c('WY', 'CO', 'UT', 'NV', 'ID', 'CA', 'OR', 'WA', 'AK', 'MT', 'NM', 'AZ', 'HI')T), 
            south = sum(c('VA', 'WV', 'KY', 'TN', 'NC', 'SC', 'FL', 'GA', 'AL', 'MS', 'LA', 'AK', 'TX', 'OK')==T), 
            north = sum(c('CT', 'MA', 'RI', 'NY', 'NJ', 'PA', 'DE', 'MD', 'NH','VT', 'ME')==T)) %>%
  gather("keyword", "n", -year, -month) %>%
  mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
  ungroup()



## text classification
## https://www.r-bloggers.com/text-classification-with-tidy-data-principles/
library(tidytext)
require(devtools)
install_github("dgrtwo/drlib")
require(drlib)
## source("https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R")

## midwest only
tidy_books <- senate %>%
  filter(is.element(state,
                    ## midwest sentaror only
                    c('ND', 'SD', 'NE', 'KS',' MO', 'IA', 'MN', 'WI', 'MI', 'IL', 'IN', 'OH'))) %>%
  unnest_tokens(word, title) 

## tidy_books
book_words <- tidy_books %>%
  count(name, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(name) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(book_words, aes(n/total, fill = name)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~name, ncol = 2, scales = "free_y")


freq_by_rank <- book_words %>% 
  group_by(name) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = name)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

## tf-idf
book_words <- book_words %>%
  bind_tf_idf(word, name, n)
book_words

## see the data
book_words %>%
  arrange(desc(tf_idf))

## visualize
## https://www.tidytextmining.com/tfidf.html#the-bind_tf_idf-function
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(name) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~name, ncol = 5, scales = "free") +
  coord_flip()


tidy_books <- tidy_books %>%
  group_by(name) %>% 
  count(name, word, sort = TRUE)  %>% 
  summarize(total = sum(n)) %>% 
  tidy_books(word, name, n)

mw <- tidy_books %>%
  filter(is.element(state,
                    ## midwest sentaror only
                    c('ND', 'SD', 'NE', 'KS',' MO', 'IA', 'MN', 'WI', 'MI', 'IL', 'IN', 'OH')))  %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ## count(name, word, sort = TRUE, party) %>%
  anti_join(get_stopwords()) %>%
  group_by(name) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(word, n, name), n,
             fill = name, color=party)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~name, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL, y = "Word count",
    title = "Most frequent words after removing stop words",
    subtitle = "Midwest Senators only"
  )

pdf(file="plot/tidy_senate_byregion_mw.pdf",family="sans", width=20, height=15)
mw
dev.off()
