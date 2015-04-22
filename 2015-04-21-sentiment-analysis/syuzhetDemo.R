## sentiment analysis with syuzhet
## vp nagraj
## last modified 4.21.15

setwd("~/Sites/Shiny/syuzhet/")

## install and load package — available via devtools
## install.packages("devtools")

## devtools::install_github("mjockers/syuzhet")
library(syuzhet)

## take a look at the functions
ls("package:syuzhet")

## compare with other text analysis packages

## install.packages("tm")
library(tm)
ls("package:tm")

## install.packages("qdap")
library(qdap)
ls("package:qdap")

## PART I — Narrative Structure (Moby Dick)

## partially adapted from syuzhet vignette: https://github.com/mjockers/syuzhet/blob/master/inst/doc/syuzhet-vignette.R
## vignette("syuzhet-vignette")

## load data from project gutenberg
path_to_text <- "http://www.gutenberg.org/cache/epub/2701/pg2701.txt"

## note: only works with http

moby_d <- get_text_as_string(path_to_text)

## split text into character vector by sentence 
## note: this implements the open nlp sentence tokenizer
moby_d <- get_sentences(moby_d)

## get sentiment vector — afinn method (based on afinn sentiment lexicon)
moby_d_sent_afinn <- get_sentiment(moby_d, method="afinn")

moby_d_sent_afinn

plot(
  moby_d_sent_afinn, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time (Line Number)", 
  ylab= "Emotional Valence",
  col="red"
)


## get mean of sentiment vectors over the narrative timeline as divided up into 100 even parts
moby_d_sent_afinn_vals <- get_percentage_values(moby_d_sent_afinn)

moby_d_sent_afinn_vals

plot(
  moby_d_sent_afinn_vals, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time (Percentage)", 
  ylab= "Emotional Valence",
  col="red"
)

## get sentiment vector — bing method
moby_d_sent_bing <- get_sentiment(moby_d, method="bing")

moby_d_sent_bing

## get sentiment with get_nrc_sentiment function
## more about the nrc lexicon: http://www.saifmohammad.com/WebPages/lexicons.html

moby_d_sent_nrc <- get_nrc_sentiment(moby_d)

class(moby_d_sent_nrc)

head(moby_d_sent_nrc)

moby_d_joy <- which(moby_d_sent_nrc$joy > 4)
moby_d[moby_d_joy]

## clean-up work space 

rm(list= ls())

## PART II — Sentiment Analysis (Twitter Data)

load("~/Sites/Shiny/tweets.Rda")
load("~/Sites/Shiny/tweets2.Rda")
library(twitteR)

## tweets come in as a list object — maybe there's a twitteR function to handle that

ls("package:twitteR")

## ... and there is — twListToDF()

obama_tweets_df <- twListToDF(obama_tweets)
obama_tweets_df2 <- twListToDF(obama_tweets2)

## get rid of the list of tweets to speed things up

rm(obama_tweets,obama_tweets2)

## also need to clean up invalid characters with regex ... don't ask

obama_tweets_df$text <- gsub("[^0-9A-Za-z///' ]", " ", obama_tweets_df$text)
obama_tweets_df2$text <- gsub("[^0-9A-Za-z///' ]", " ", obama_tweets_df2$text)

## now apply the get_nrc_sentiment function to the text variable

obama_tweets_nrc <- get_nrc_sentiment(obama_tweets_df$text)
obama_tweets_nrc2 <- get_nrc_sentiment(obama_tweets_df2$text)

library(tidyr)
library(dplyr)

tidy_obama_tweets <- 
  obama_tweets_nrc %>%
  select(1:8) %>%
  gather() %>%
  group_by(Sentiment=key) %>%
  summarise(Total=sum(value))

tidy_obama_tweets2 <- 
  obama_tweets_nrc2 %>%
  select(1:8) %>%
  gather() %>%
  group_by(Sentiment=key) %>%
  summarise(Total=sum(value))


library(ggplot2)

p_obama <- ggplot(NULL, aes(Sentiment,Total)) +
  geom_bar(aes(fill="march"), data=tidy_obama_tweets, stat="identity", alpha=0.5) +
  geom_bar(aes(fill="april"), data=tidy_obama_tweets2, stat="identity", alpha=0.5) +
  ylab("Total Weight") +
  theme(legend.title=element_blank()) +
  ggtitle("Obama Tweet Sentiment (March & April)")

p_obama

## now subsetting just positive and negative

tidy_obama_tweets_polarity <- 
  obama_tweets_nrc %>%
  select(9:10) %>%
  gather() %>%
  group_by(Sentiment=key) %>%
  summarise(Total=sum(value))

tidy_obama_tweets_polarity2 <- 
  obama_tweets_nrc2 %>%
  select(9:10) %>%
  gather() %>%
  group_by(Sentiment=key) %>%
  summarise(Total=sum(value))

p_obama2 <- ggplot(NULL, aes(Sentiment,Total)) +
  geom_bar(aes(fill="march"), data=tidy_obama_tweets_polarity, stat="identity", alpha=0.5) +
  geom_bar(aes(fill="april"), data=tidy_obama_tweets_polarity2, stat="identity", alpha=0.5) +
  ylab("Total Weight") +
  theme(legend.title=element_blank()) +
  ggtitle("Obama Tweet Sentiment (March & April)")


p_obama2

## PART III — Sentiment Analysis (Survey Data)

survey_results <- read.csv("~/Sites/Shiny/survey_responses.csv")

survey_results$Response.Text <- as.character(survey_results$Response.Text)

## a little clean-up

survey_results$Response.Date <- as.character(survey_results$Response.Date)
survey_results$Response.Date <- as.Date(survey_results$Response.Date, format="%B %d %Y")

## then something similar to the obama tweets above ...

survey_nrc <- get_nrc_sentiment(survey_results$Response.Text)

summary(survey_nrc)

library(tidyr)
library(dplyr)

tidy_survey <- 
  survey_nrc %>%
  select(9:10) %>%
  gather() %>%
  group_by(Sentiment=key) %>%
  summarise(Total=sum(value))

p_survey <- 
  ggplot(tidy_survey, aes(Sentiment,Total)) +
  geom_bar(stat="identity", alpha=0.5) +
  ylab("Total Weight") +
  ggtitle("Survey Sentiment")

p_survey

## end syuzhet demo

