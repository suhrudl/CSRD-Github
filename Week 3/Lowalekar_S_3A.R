setwd("/Users/SuhrudL/Desktop/Coursework/Spring 16/Collect Store and Ret Data/Week 3")
rm(list=ls())

#ASSIGNMENT 3A : DATA SHAPING

#1. Load the data file "Acquisitions" into an appropriate data object of your
#   choice. The files contains dates and firms into which an investment was made.

acquisitions <- read.table("Acquisitions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

acquisitions$Date <- as.Date(acquisitions$Date, "%m/%d/%Y" )
class(acquisitions$Date)
class(acquisitions$Investment)

acquisitions

#2. Write a function called leastInvInterval() that finds the smallest
#   interval between successive investments.

leastInvInterval <- function(acquisitions){
  for(i in 1:(length(acquisitions$Date)-1)){
    datediff[i] <- difftime(acquisitions$Date[i+1], acquisitions$Date[i], units = "days")
  }
  return(paste(datediff[which.min(datediff)], " days is the smallest interval between successive investments.", sep = ""))
}

#testcase
leastInvInterval(acquisitions)
