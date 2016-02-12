#setting the environment
install.packages("XML")
library(XML)

#1. loading the xml data to a dataframe
df <- xmlToDataFrame("http://www.xmldatasets.net/temp/179681356453762.xml")

#verifying if the data has been imported successfully 
View(df)



#2. fucntion to return senator names given state
senaterName <- function(stateName){
  x <- c()
  for(i in 1:length(df$first_name)){
    if(df$state[i] == stateName){
      x[i] <- paste(df$first_name[i], df$last_name[i])
    }
  }
  senators <- as.vector(na.omit(x))
  senators
}

#test case
senaterName("NH")
# [1] "Kelly Ayotte"   "Jeanne Shaheen"



#2. same function, implemented using which (for loops = more complexity)
senaterName <- function(stateName){
  x <- which(df$state == stateName)
  paste(df$first_name[x], df$last_name[x])
}

#test case
senaterName("MA")
# [1] "Scott P. Brown" "John F. Kerry" 



#3. function to return phone number given full name
senatorPhone <- function(fullName){
  x <- which(paste(df$first_name, df$last_name) == fullName)
  paste(df$phone[x])
}

#test case
senatorPhone("Jeanne Shaheen")
# [1] "(202) 224-4543"
