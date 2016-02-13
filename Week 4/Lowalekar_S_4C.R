rm(list=ls())

library(XML)

#1. parsing the XML tree
xmlobj <- xmlTreeParse("http://www.cs.washington.edu/research/xmldatasets/data/auctions/ebay.xml")
r <- xmlRoot(xmlobj)

#checking the number of nodes inside the parent node
xmlSize(r)

#function to count auctions had more than 5 bids
xmlTreeParse <- function(r){
  count <-  0
  for(i in 1:xmlSize(r)){
    if(as.integer(xmlValue(r[[i]][[5]][[5]])) > 5){
      count <-  count + 1
    }
  }
  paste(count,"auctions had more than 5 bids.")
}

#test case
xmlTreeParse(r)
# "5 auctions had more than 5 bids."


#2a. What was the highest closing price for the security?

#parsing the XML tree, clear the environment first
rm(list=ls())
x <- xmlTreeParse("http://www.barchartmarketdata.com/data-samples/getHistory15.xml")
r <- xmlRoot(x)

#checking the number of nodes 
xmlSize(r)

#function to check the highest closing price for the security

highestClosingPrice <- function(r){
  close <- c()
  for(i in 2:xmlSize(r)){
    close[i-1] <- (as.numeric(xmlValue(r[[i]][[7]])))
  }
  maxClose <- which.max(close)
  paste(close[maxClose], "was the highest closing price for the security.")
}

#testcase
highestClosingPrice(r)
#[1] "1768 was the highest closing price for the security."


#2b. what was the total volume traded

totalVolume <- function(r){
  total <- 0
  for(i in 2:xmlSize(r)){
    total <- total + (as.integer(xmlValue(r[[i]][[8]])))
  }
  paste(total, "was the total volume traded")
}

totalVolume(r)


#2c.what was the average trading volume during each HOUR of the trading day

#preprocessing to get the XML data into a dataframe
r2 <- xmlToDataFrame("http://www.barchartmarketdata.com/data-samples/getHistory15.xml")
#removing unnecessary rows and columns from the dataframe
r3 <- r2[2:nrow(r2), 3:ncol(r2)]

#parse the timestamp into date and time format
dateTime <- strptime(r3$timestamp, "%Y-%m-%dT%H:%M:%OS")

#add column having the date and time to the dataframe
r3$dateTime <- dateTime
final <- as.data.frame(r3)
final #this is the final dataframe that we use for analysis

#function to find the average trading volume during each HOUR of the trading day
averageVolume <- function(final){
  #aggregate slices the dataframe by the hours of the day (like GROUP BY in SQL)
  avgVolume <- as.data.frame(aggregate(x = as.numeric(r3$volume),by = (hour=list(r3$hour)),FUN = mean))
  colnames(avgVolume) <- c("Hour", "Avg Volume")
  avgVolume
}

#testcase
averageVolume(final)
