setwd("~/Desktop/Coursework/Spring 16/Collect Store and Ret Data/Week 5:6")
library(RCurl)
library(XML)
library(scrapeR)

#setlist.fm is a website that has statistics about bands and their tours
#i am collecting data about a band called Tool
#the data frame has to display the song title and the number of times it has been played live
#this can be seen on the URL http://www.setlist.fm/stats/tool-2bd6d836.html

webpage <- getURL(url = "http://www.setlist.fm/stats/tool-2bd6d836.html")
webpage <- readLines(tc <- textConnection(webpage))

pagetree <- htmlTreeParse(webpage,useInternalNodes = T)

pagetree

x <- unlist(xpathApply(pagetree,"//*/td[@class = 'songName']", xmlValue))
y <- unlist(xpathApply(pagetree,"//*/td[@class = 'songCount']", xmlValue))

x <- gsub("\n|Play Video|stats","", x)

toolSongs <- data.frame(x, y)
colnames(toolSongs) <- c("Song Title", "Live Play Count")

toolSongs
