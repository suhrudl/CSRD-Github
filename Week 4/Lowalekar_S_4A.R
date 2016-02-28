rm(list=ls())


list.files()

library(openxlsx)
mydf <- read.xlsx("2013 Geographric Coordinate Spreadsheet for U S  Farmers Markets 8'3'1013.xlsx", startRow = 3, colNames = TRUE)

dates <- mydf$Season1Date

View(as.data.frame(dates))

splitDates <- strsplit(dates, split = ' to')

splitDates <- as.data.frame(splitDates)
splitDates <- t(splitDates)

row.names(splitDates) <- NULL
splitDates <- as.data.frame(splitDates)
splitDates

mydf$Season1Date <- strsplit(mydf$Season1Date, split = " to ")
mydf <- mydf[!is.na(mydf["Season1Date"]),]
