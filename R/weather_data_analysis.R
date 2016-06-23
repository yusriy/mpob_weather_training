#### Weather station data analysis ####
# Author: Yusri Yusup
# Date created: 2016-06-22
# Version: 1.0
#


#### Preliminaries ####
library(openair)

#### Data import ####
# Import the data
w_data <- read.csv(file = 'data/penor_2011_2013.csv', sep = ',', skip = 1)
# Remove 1st and 2nd row
w_data <- w_data[-c(1,2),]
# Change to all numeric
for (i in 2:ncol(w_data)){
  w_data[,i] <- as.character(w_data[,i])
}
for (i in 2:ncol(w_data)){
  w_data[,i] <- as.numeric(w_data[,i])
}
# Format time
w_data$TIMESTAMP <- strptime(w_data$TIMESTAMP,format = '%m/%d/%y %H:%M')
w_data$TIMESTAMP <- as.POSIXct(w_data$TIMESTAMP)
# Rename timestamp
names(w_data)[1] <- 'date'
rm(i)

#### Averaging ####
w_data_monthly <- timeAverage(w_data, avg.time = 'month')
w_data_daily <- timeAverage(w_data, avg.time = 'day')

#### Calculate vapor pressure deficit ####
# Temporary air temperature variable in K
Temp <- w_data_monthly$AirTempC_Avg
RH <- w_data_monthly$RelHum_Avg
# Saturate vapor pressure, units of kPa
# From the ASCE Standardized Reference Evapotranspiration Equation
vpsat <- 0.6108 * exp(17.27 * Temp / (Temp + 237.3))
# Actual partial pressure, units of kPa
vpair <- vpsat * RH/100
# Vapor pressure deficit, units of kPa
vpd <- vpsat - vpair

#### Wind rose ####
windRose(w_data, ws = 'WindSpd_ms_Avg', wd = 'WindDir_Avg', paddle = FALSE)

#### Barplot ####

plot(w_data_monthly$date,w_data_monthly$Rain_mm_Tot, type = 'l', ylim = c(0, 4),
     ylab = 'Cumulative rain (mm)', xlab = 'Month')

plot(w_data$date,w_data$Rain_mm_Tot, type ='l')

barplot(w_data_monthly$Rain_mm_Tot, type = 'l', ylim = c(0, 4),
        ylab = 'Cumulative rain (mm)', xlab = 'Month', col = 'white')
box()


