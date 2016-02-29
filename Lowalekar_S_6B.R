rm(list = ls())
setwd("~/Desktop/Coursework/Spring 16/Collect Store and Ret Data/Week 5:6")
library(RCurl)
library(XML)
library(scrapeR)

#setlist.fm is a website that has statistics about bands and their tour setlists
#my function scrapes stats about any band profile on the setlist.fm artist 'tour statistics' tab
#the data frame displays the song title and the number of times it has been played live in their concerts
#an example URL: "http://www.setlist.fm/stats/tool-2bd6d836.html" 
#or "http://www.setlist.fm/stats/metallica-3bd680c8.html"



dataScraper <- function(URL){
  #get the webpage from URL using a GET request
  webpage <- getURL(url = URL)
  #read webpage line by line 
  webpage <- readLines(tc <- textConnection(webpage))
  #parse the html structure
  pagetree <- htmlTreeParse(webpage,useInternalNodes = T)
  
  #look for songnames, store them in vector x
  x <- unlist(xpathApply(pagetree,"//*/td[@class = 'songName']", xmlValue))
  #look for songcount, store in vector y
  y <- unlist(xpathApply(pagetree,"//*/td[@class = 'songCount']", xmlValue))
  #clean up data in x
  x <- gsub("\n|Play Video|stats","", x)
  #create dataframe
  artistStats <<- data.frame(x, y)
  #look for the band name
  bandName <- unlist(xpathApply(pagetree,"//*/title", xmlValue))
  bandName <- gsub("\\Tour Statistics | setlist.fm","", bandName)
  #add column names
  colnames(artistStats) <<- c(paste(bandName,"Song Title"), "Live Play Count")
  return(artistStats)
  
}

#pass the URL, test band Tool
URL <- "http://www.setlist.fm/stats/tool-2bd6d836.html"
dataScraper(URL)

#pass different URL, test band Metallica
URL <- "http://www.setlist.fm/stats/metallica-3bd680c8.html"
dataScraper(URL)

#pass last test URL, test band Pink Floyd. This has a space in band name, checking if code breaks
URL <- "http://www.setlist.fm/stats/pink-floyd-13d6adc5.html"
dataScraper(URL)

#as can be seen on the website, there pink floyd have played 139 distinct songs live.
#testing for consistency

length(artistStats$`Live Play Count`)
#[1] 139
#we can manually compare the values for consistency as well

