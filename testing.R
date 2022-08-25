# Date;Time;Global_active_power;Global_reactive_power;Voltage;
# Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
#
# 5/4/2007;11:41:00;1.390;0.160;240.580;5.800;0.000;0.000;18.000

# Read only this time period 2007-02-01 and 2007-02-02
# grepl("[0-9]{2}-[0-9]{2}-[0-9]{2}", mystring)
library(dplyr)
library(stringr)

powerConsumption <- file("household_power_consumption.txt", "r")
dataPoints <- c()
done <- FALSE

before <- as.Date("01/02/2007", format='%d/%m/%Y')
after  <- as.Date('02/02/2007', format='%d/%m/%Y')

line <- readLines(powerConsumption, 1)
while (!done) {
  line <- readLines(powerConsumption, 1)
  datetime <- str_split_fixed(line, ";", 3)
  day <- as.Date(datetime[,1], format='%d/%m/%Y')
  dt = paste(datetime[,1], datetime[,2], sep = " ")

  time <- strptime(dt, format="%d/%m/%Y %H:%M:%S")

  if (day == before || day == after) {
    print(day)
    print(time)
  } else if (day > after){
    done <- TRUE
  }
}
close(powerConsumption)


#  day <- strtoi(time[[1]][1])
#  month <- strtoi(time[[1]][2])
#  year <- strtoi(time[[1]][3])
  
#  if (year == 2007) {
#    if (month == 2) {
#      if ((day == 1) || (day == 2)) {
#        print(date[,1])
#        print(line) 
#        print(time)
#        print(day)
#        print(month)
#        print(year)
#      } else if (day > 2) {
#        done <- TRUE
#      }
#    } else if (month > 2) {
#      done <- TRUE
#    }
#  } else if (year == 2007) {
#    done <- TRUE
#  }
#}

