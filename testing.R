# Date;Time;Global_active_power;Global_reactive_power;Voltage;
# Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
#
# 5/4/2007;11:41:00;1.390;0.160;240.580;5.800;0.000;0.000;18.000

# Read only this time period 2007-02-01 and 2007-02-02
# grepl("[0-9]{2}-[0-9]{2}-[0-9]{2}", mystring)
library(dplyr)
library(stringr)

powerConsumption  <- file("household_power_consumption.txt", "r")
dateTimePoints    <- c()
dayPoints         <- c()

done <- FALSE

#before <- as.Date("2007-02-01", format='%Y-%m-%d')
before <- as.Date("01/02/2007", format='%d/%m/%Y')
after  <- as.Date('02/02/2007', format='%d/%m/%Y')

line <- readLines(powerConsumption, 1)
while (!done) {
  line <- readLines(powerConsumption, 1)
  timeStr <- str_split_fixed(line, ";", 3)
  
  dtString  <- paste(timeStr[,1], timeStr[,2], sep = " ")
  dateStr   <- strptime(dtString, format="%d/%m/%Y %H:%M:%S")
  dateObj   <- as.Date(timeStr[,1], format='%d/%m/%Y')

  if (dateObj == before || dateObj == after) {
    dateTimePoints    <- append(dateTimePoints, dateObj)
    dayPoints         <- append(dayPoints, format(dateObj, "%a"))
  } else if (dateObj > after){
    done <- TRUE
  }
}

close(powerConsumption)
sourceData = data.frame(Date = dateTimePoints)
sourceData$Day <- dayPoints

print(head(sourceData))
print(tail(sourceData))
#print(sourceData)

