# Date;Time;Global_active_power;Global_reactive_power;Voltage;
# Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
#
# 5/4/2007;11:41:00;1.390;0.160;240.580;5.800;0.000;0.000;18.000

# Read only this time period 2007-02-01 and 2007-02-02
# grepl("[0-9]{2}-[0-9]{2}-[0-9]{2}", mystring)
library(dplyr)
library(stringr)

library(chron)

# dayInSec <- function (timeToken, dateObj) {
# #  print(timeToken)
# #  print(dateObj)
#   clockObj    <- chron(times = timeToken[,2], format=c('h:m:s'))
#   hour        <- hours(clockObj)
#   minute      <- minutes(clockObj)
#   second      <- seconds(clockObj)
# 
#   dayFrac     <- (3600*hour + 60 * minute + second)/86400
#   intDay      <- strtoi(dateObj)
#   
#   dayFrac     <- intDay - 1 + dayFrac
#   print(sprintf("int day from dateObj %d", dayFrac))
#   print(sprintf("day %f", dayFrac))
# #  print(hour)
# #  print(minute)
# #  print(second)
# 
#   return (dayFrac)
# }

transformTime <- function (df) {
  day <- strtoi(format(df$Date, format = "%d"))

  clockObj    <- chron(times = df$Time, format = c("h:m:s"))
  hour        <- hours(clockObj)
  minute      <- minutes(clockObj)
  second     <- seconds(clockObj)

  dayFrac     <- (3600*hour + 60 * minute + second)/86400
  transformedT <- day - 1 + dayFrac
  return  (transformedT)
}

transformDF <- function (df) {
  df$Date  <- as.Date(df$Date, format = "%d/%m/%Y")
  df$Time  <- df$Time
  df$GAP   <- as.double(df$GAP)
  df$GRP   <- as.double(df$GRP)
  df$Volts <- as.double(df$Volts)
  df$GI    <- as.double(df$GI)
  df$SM1   <- as.double(df$SM1)
  df$SM2   <- as.double(df$SM2)
  df$SM3   <- as.double(df$SM3)
  return (df)
}

getFormattedFileData <- function () {
  done                 <- FALSE
  before <- as.Date("01/02/2007", format='%d/%m/%Y')
  after  <- as.Date('02/02/2007', format='%d/%m/%Y')
  
  powerConsumption     <- file("household_power_consumption.txt", "r")
  lineTokens <- readLines(powerConsumption, 1)

  powerConsumptionData  <- data.frame(Date = character(0),
                                      Time = character(0),
                                      GAP = double(0),
                                      GRP = double(0),
                                      Volts = double(0),
                                      GI = double(0),
                                      SM1 = double(0),
                                      SM2 = double(0),
                                      SM3 = double(0),
                                      stringsAsFactors= FALSE
                                      )
  
  repeat {
    line <- readLines(powerConsumption, 1)
    lineTokens <- str_split(line, ";")
    dateObj   <- as.Date(lineTokens[[1]][1], format='%d/%m/%Y')
    if (dateObj >= before && dateObj <= after) {
      powerConsumptionData[nrow(powerConsumptionData) + 1,] <- c(
        lineTokens[[1]][1],
        lineTokens[[1]][2],
        lineTokens[[1]][3],
        lineTokens[[1]][4],
        lineTokens[[1]][5],
        lineTokens[[1]][6],
        lineTokens[[1]][7],
        lineTokens[[1]][8],
        lineTokens[[1]][9]
      ) 

    } else if (dateObj > after){
      done <- TRUE
    }
    
    if (done) break
  }
  close(powerConsumption)
  powerConsumptionData       <- transformDF (powerConsumptionData)
  powerConsumptionData$day   <- transformTime(powerConsumptionData)
  
  return (powerConsumptionData)
  
}

powerConsumptionData = getFormattedFileData()
