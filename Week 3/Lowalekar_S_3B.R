setwd("/Users/SuhrudL/Desktop/Coursework/Spring 16/Collect Store and Ret Data/Week 3")
rm(list=ls())

#1. Load the data file "BirdStrikes" into an appropriate data object of your choice. 
#   How many bird strikes did not have a “Reported: Date” assigned, i.e., for where there 
#   is no value for "Reported: Date".

#using data frame, as it is a tabular data and the columns have different datatypes so matrix cannot
#be used

birdStrikes <- read.csv("Bird Strikes 2.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#loading into csv
missing <- length(which(birdStrikes$Reported..Date == ""))
#there are no NA values, instead there are "" blank values
paste(missing, " bird strikes did not have a “Reported: Date” assigned", sep = "")

#2. Which year had the most bird strikes? Write a function to calculate.

MaxStrikes <- function(birdStrikes){
  birdStrikes$Reported..Date <- as.Date(birdStrikes$Reported..Date, "%m/%d/%Y")
  #converting the values to dates
  Year <<- as.numeric(format(birdStrikes$Reported..Date, "%Y"))
  #extracting just the years
  maxYear <- names(which.max(table(Year)))
  #returning the most recurring year
  return(paste(maxYear, "had the most bird strikes", sep = " "))
}

MaxStrikes(birdStrikes)


#3. How many bird strikes were there for each year? Place the result into a data frame.

#to count the frequency of bird strikes each year, we tabulate "Year" and represent it as a DF
FreqStrike <- function(Year){
  freq <- as.data.frame(table(Year)) 
  #output
  return(freq)
}


#function call 
FreqStrike(Year)

#4. Write a function that calculates the number of birds strikes per airline and then put those 
#   results into a dataframe called AirlineStrikes. Write another function that accepts the 
#   dataframe AirlineStrikes as an argument, and returns the airline that has the most bird strikes.


AirlineFunc <- function(birdStrikes){
  #import data into AirlineStrikes as data frame using table() which gives frequency
  AirlineStrikes <<- as.data.frame(table(birdStrikes$Aircraft..Airline.Operator))
  return(AirlineStrikes)
}

MaxAir <- function(AirlineStrikes){
  #sorting the data in descending order of frequency
  SortedStrikes <- AirlineStrikes[order(-AirlineStrikes[,2]),]
  #selecting the second row as 1st is unknown
  return(paste(SortedStrikes[2,1],"is the airline that has the most bird strikes."))
}

#call AirlineFunc
AirlineFunc(birdStrikes)
#call MaxAir
MaxAir(AirlineStrikes)




