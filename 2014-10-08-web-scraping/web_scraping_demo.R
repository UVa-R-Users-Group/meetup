# Clay Ford
# R Users Group October 2014 meetup
# some very basic web scraping examples


# EXAMPLE 1 ---------------------------------------------------------------

# IMDB Top 250 movies
# URL: http://www.imdb.com/chart/top?ref_=nv_ch_250_4

library(XML)
library(stringr)
library(lubridate)
library(dplyr)

# read the web page source code
movies <- readLines("http://www.imdb.com/chart/top?ref_=nv_ch_250_4", warn=FALSE)

# web scraping complete!
# let the clean up begin...

# find the index numbers that mark the begin and end of table
start <- grep("table class=\"chart\"", movies)
end <- grep("The formula for calculating the", movies)
movies250 <- movies[start:(end-3)]

# starts and ends with table tags
head(movies250, n=1)
tail(movies250, n=1)

# use readHTMLTable() from XML package to convert HTML table to data frame
imdb250 <- readHTMLTable(movies250)

# extract the data frame from the first list element
imdb250 <- imdb250[[1]]

# keep columns 2 and 3
imdb250 <- imdb250[,c(2,3)]

# fix the column names so they don't have spaces
names(imdb250) <- c("Title", "Rating")

# make rating numeric
imdb250$Rating <- as.numeric(as.character(imdb250$Rating))

# add a column for rank
imdb250$Rank <- as.numeric(row.names(imdb250))

# Title column needs work
imdb250$Title

# extract title using str_extract() from stringr
title <- str_extract(imdb250$Title, pattern = "\n.*\n")
title <- gsub("\n","",title) # find and replace
title <- str_trim(title) # remove leading and trailing spaces

# extract year
year <- str_extract(imdb250$Title, pattern = "\\([0-9]{4}\\)")
year <- gsub("\\(|\\)","",year)
imdb250$Year <- as.numeric(year)

imdb250$Title <- title
# Done!

# see which years have the most movies on the list
imdb250 %>% 
  group_by(Year) %>%
  summarise(Total=n()) %>%
  arrange(desc(Total)) %>%
  head(10)

# see the 1995 movies
imdb250 %>% 
  filter(Year==1995)
  
rm(movies, movies250, end, start, title, year)

# EXAMPLE 2 ---------------------------------------------------------------

# multiple pages
# Craigslist listings
# https://charlottesville.craigslist.org/search/cba

dat <- readLines("https://charlottesville.craigslist.org/search/cba?s=0&", warn=FALSE)

# function to prep craigslist data
# dat is data vector obtained from readLines()
CLPrep <- function(dat){
  # generate an R structure representing the HTML structure
  # htmlTreeParse() is in XML package;
  # useInternalNodes = TRUE so I can use XPath expressions
  raw2 <- htmlTreeParse(dat, useInternalNodes = TRUE)
  
  # raw2 is an XML-like structure with nodes
  # to see all the nodes: unlist(xpathApply(raw2, "//*", xmlName))
  
  # XPath uses path expressions to select nodes or node-sets in an XML document
  
  # the items are in nodes called "a" with a class attribute equal to "hdrlnk"
  item <- xpathApply(raw2,"//a[@class='hdrlnk']", xmlValue)
  item <- unlist(item)
  
  # date listed in nodes called "time" with a class attribute equal to "date"
  date.posted <- xpathApply(raw2,"//time", xmlGetAttr, "datetime")
  date.posted <- unlist(date.posted)
  # ymd_hm() is from the lubridate package
  # use Sys.timezone() to get time zone
  date.posted <- ymd_hm(date.posted, tz="America/New_York") 
  
  # item has the price listed twice or not all
  # first need to get element from dat that contains prices
  i <- grep("class=\"price\"", dat)
  tmp <- dat[i]
  # Split text at "</p>" since every item ends with that tag
  items <- strsplit(tmp, split = "</p>")
  items <- unlist(items)
  
  # get index number of items that have prices
  ind <- grep("class=\"price\"", items)
  
  # price listed in nodes called "span" with a class attribute equal to "price"
  prices <- xpathApply(raw2,"//span[@class='price']", xmlValue)
  prices <- unlist(prices)
  
  # keep every other price (remove dupes)
  prices <- prices[seq_along(prices) %% 2 == 1] 
  prices <- as.numeric(gsub("\\$","",prices))
  
  # fill in prices per ind (ie, row numbers which had prices)
  price <- rep(NA,length(item))
  price[ind] <- prices
  
  # create data frame
  data.frame(item, date.posted, price, stringsAsFactors = F)

}

# Note URL increments by 100:
# https://charlottesville.craigslist.org/search/cba?s=0&
# https://charlottesville.craigslist.org/search/cba?s=100&
# https://charlottesville.craigslist.org/search/cba?s=200&
# increment URLs by 100; stop if code contains "no results"

cl.out <- c() # create an empty object to store results
j <- 0 # for URL
repeat{
  raw <- readLines(paste0("https://charlottesville.craigslist.org/search/cba?s=",j,"&"), warn=F)
  if (any(grepl(pattern = "no results", x = raw))) break else {
    cl.out <- rbind(cl.out,CLPrep(dat=raw))
    j <- j + 100
  }
}

summary(cl.out$price)


