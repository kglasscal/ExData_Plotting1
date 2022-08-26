# Date;Time;Global_active_power;Global_reactive_power;Voltage;
# Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
#
# 5/4/2007;11:41:00;1.390;0.160;240.580;5.800;0.000;0.000;18.000

# Read only this time period 2007-02-01 and 2007-02-02
# grepl("[0-9]{2}-[0-9]{2}-[0-9]{2}", mystring)
library(dplyr)
library(stringr)

library(chron)

dayInSec <- function (timeToken, dateObj) {
#  print(timeToken)
#  print(dateObj)
  clockObj    <- chron(times = timeToken[,2], format=c('h:m:s'))
  hour        <- hours(clockObj)
  minute      <- minutes(clockObj)
  second      <- seconds(clockObj)

  dayFrac     <- (3600*hour + 60 * minute + second)/86400
  intDay      <- strtoi(dateObj)
  
  dayFrac     <- intDay - 1 + dayFrac
  print(sprintf("int day from dateObj %d", dayFrac))
  print(sprintf("day %f", dayFrac))
#  print(hour)
#  print(minute)
#  print(second)

  return (dayFrac)
}



getFormattedFileData <- function () {
  powerConsumption  <- file("household_power_consumption.txt", "r")
  data.frame(Date = datePoints)
  datePoints        <- c()
  dayInSecsPoints   <- c()
  dayPoints         <- c()

  done <- FALSE
  print(head(powerConsumption))
  #before <- as.Date("2007-02-01", format='%Y-%m-%d')
  before <- as.Date("01/02/2007", format='%d/%m/%Y')
  after  <- as.Date('02/02/2007', format='%d/%m/%Y')
  
  line <- readLines(powerConsumption, 1)
  
  while (!done) {
    line <- readLines(powerConsumption, 1)
    lineTokens <- str_split_fixed(line, ";", 3)
    
    dtString  <- paste(lineTokens[,1], lineTokens[,2], sep = " ")
    dateStr   <- strptime(dtString, format="%d/%m/%Y %H:%M:%S")
    dateObj   <- as.Date(lineTokens[,1], format='%d/%m/%Y')
    
    if (dateObj == before || dateObj == after) {
      datePoints    <- append(datePoints, dateStr)
      dayPoints     <- append(dayPoints, format(dateObj, "%a"))
      frac          <- dayInSec(lineTokens, dateObj)
      
      print(frac)
      dayInSecsPoints    <- append(dayInSecsPoints, strtoi(dateObj) - 1 + frac)
      
    } else if (dateObj > after){
      done <- TRUE
    }
  }
  
  close(powerConsumption)
  sourceData = data.frame(Date = datePoints)
  sourceData$Day <- dayPoints
  sourceData$DayInSecs <- dayInSecsPoints
  print(head(sourceData))
  print(tail(sourceData))
  #print(sourceData)
}



#while (!done) {
#  line        <- readLines(powerConsumption, 1)
#  lineTokens  <- str_split(line, ";")
#  dateObj     <- as.Date(timeStr[[1]][1], format='%d/%m/%Y')
#  lineTokens  <- str_split_fixed(line, ";", 3)
#  dateObj     <- as.Date(lineTokens[,1], format='%d/%m/%Y')

#  dateObjStr <- format(before, format = "%d/%m/%Y")
#  testStr <- sprintf("before = %s, dateObj = %s", beforeStr, dateObjStr)
#  print(testStr)

#  if (dateObj == before && dateObj == after) {
#    dayObj          <- format(dateObj, format = "%d")

#    clockObj    <- chron(times = lineTokens[[1]][2], format=c('h:m:s'))
#    hour        <- hours(clockObj)
#    minute      <- minutes(clockObj)
#    second      <- seconds(clockObj)
#    dayFrac     <- (3600*hour + 60 * minute + second)/86400

#    datePoints  <- append(datePoints, dateObj)
#    dayPoints   <- append(dayPoints, strtoi(dayObj) - 1 + dayFrac)

#  } else if (dateObj > after){
#    done <- TRUE
#  }
#}
#print(head(sourceData))
#print(tail(sourceData))
#print(sourceData)

#close(powerConsumption)
#sourceData = data.frame(Date = datePoints)
#sourceData$Day <- dayPoints

getFormattedFileData()
