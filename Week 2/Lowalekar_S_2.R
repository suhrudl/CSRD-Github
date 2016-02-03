rm(list=ls())
##############
##############

setwd("~/Desktop/Coursework/Spring 16/Collect Store and Ret Data/Week 2/Assignment - Week 2")

#1. Load the data file into an appropriate data object of your choice.

#air is loaded to a data frame, as it has multiple columns and a huge number of rows. A two dimensional data structure
#like a table is more suitable for analysis than a vector. 
loaddata <- function(){
air <<- read.table(unz("AirlineDelays.zip", "AirlineDelays.txt"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
#replacing the NA elements by zero, as they add no further insight to the analysis 
#this can be seen in the data frame, as places where data is not available is NA
#substituting by 0 will provide easier analysis, and will have no no negative impacts on the dataset
air[is.na.data.frame(air)] <<- 0
View(air)
}
loaddata()

##############
##############

#2. Write a function called TotalNumDelays(Carrier) that finds and returns the total number of delays of a carrier
#calculating the 'delays' for a carrier in arrival, departure, and the other delays in columns 9-13 


TotalNumDelays <- function(Carrier){
    sum <- 0
    if(exists('air') == FALSE){
    loaddata()
  }
  #Description: Function to find total number of delays of a carrier. 
  #Args: Carrier. character variable, it denotes the carrier name
  #Returns: sum. numeric variable, denotes the total number of delays of Carrier  
  for(i in 1:length(air$YEAR)){  
    #cycling along the all the rows of air  
    if(air$CARRIER[i] == Carrier){
      for(j in c(6,7,9:13)){
        #cycling along columns 6,7,9,10,11,12,13 if air  
        if(as.numeric(air[i,j]) > 0){
          #increments sum by 1 each time a value greater than 0 is found in the column
          sum = sum + 1
        }
      }
    }
  }
  return(sum)
}


#testcase
TotalNumDelays("AA")

##############
##############

#3. Write a function called TotalDelaysByOrigin(Origin) that finds and returns the total number of delays for a particular airport.
#here, we will consider only column 6 (departure delay) of the data set as they have to be the delays by a particular airport

TotalDelaysByOrigin <- function(Origin) {
#Description: Function to find total number of delays for a particular airport 
#Args: Origin. character variable, it denotes the airport name
  sum <- 0
  if(exists('air') == FALSE){
    loaddata()
  }
#Returns: sum. numeric variable, denotes the total number of delays for the Origin airport  
  for (i in 1:length(air$YEAR)) {
    #cycling along the all the rows of air
    if (air$ORIGIN[i] == Origin) {
      if (as.numeric(air[i,6]) > 0) {
        #increments sum by 1 each time a value greater than 0 is found in the column 6
        sum = sum + 1
      }
    }
  }
  return(sum)
  sum <- 0
}

#testcase
TotalDelaysByOrigin("JFK")

##############
##############

#4. Write a function called AvgDelay(Carrier,Dest) that calculates and returns the average
#   arrival delay for a carrier flying into the specified destination airport

#here, we will consider only column 7(arrival delay) of the dataset as we are interested in delays
#caused by a carrier flying into a destination.

AvgDelay <- function(Carrier,Dest){
#Description: Function calculates average arrival delay for a carrier flying into the specified destination airport
#Args: Carrier, Dest. character variables. Denotes the carrier and the destination airport
#Returns: avg. Numerical variable. average arrival delay for a carrier flying into the specified destination airport   
  sum <- 0
  delay <- 0
  if(exists('air') == FALSE){
    loaddata()
  }
  for (i in 1:length(air$YEAR)) {
  #cycles through all the rows of the dataframe  
    if(air$CARRIER[i] == Carrier){
    #matches carrier  
      if (air$DEST[i] == Dest) {
      #matches destination airport  
        sum = sum + 1
        delay = delay + sum(air[i,7])
        #increments sum by 1 and increments delay minutes
      }
    }
  }
avg = delay/sum
return(avg)
#returns avg
avg <- 0
#sets avg back to zero for further use
}

#testcase
AvgDelay("AA", "LAX")

##############
##############

#5. The functions are optimized by the if condition that checks if the dataframe is loaded in memory
#   and loads it if it is not. this condition is already added to all the functions. 
