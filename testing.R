library(dplyr)
library(stringr)
library(chron)

# ##############################################################################
# Author: Kevin Glass
# Assignment: Exploratory Data Analysis, Project 1
#
# This project consists of two component: getting and formatting the data and 
# generating a graphic. 
# 
# Getting and formatting the data requires a call to getFormattedFileData(). This
# function will:
# 1) Initialize several variables to control the read and store loop.
# 2) Open the source file for reading.
# 3) Initialize an empty data frame "powerConsumptionData."
# 4) Loop through the data until the finish value "after" is reached.
# 4a) Read one line from the data file on each iteration of the loop.
# 4b) Tokenize the line and add each token to "the next row" of the data.frame.
# 4c) When the last specified data is reached, terminate the loop.
# 5) Close the open file.
# 6) Transform the data.frame values to appropriate data types.
# 7) Add a day column with the fraction of each day. 
# ##############################################################################



# ******************************************************************************
# The "transformTime" function changes the representation of a date as %d/%m/%Y
# %H:%M:%Y to the day plus the fraction of the day completed. Each day is 
# relative to the start of the first date in the target data set. 
# 
# For example, in the current problem the start date is 01/02/2007, this is 
# represented by day = 0
# ******************************************************************************
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

# ******************************************************************************
# The "transformDF" converts each column in the data frame to a specified type.
# ******************************************************************************
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
