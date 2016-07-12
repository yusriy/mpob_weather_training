#### Weather station data analysis ####
# Author: Yusri Yusup
# Date created: 2016-06-22
# Version: 1.0
#

#### Preliminaries ####
library(openair)
source('R/vap_deficit.R')
source('R/julian_conv.R')

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

#### Julian da ####
# Calculate julian day for hourly data
jd <- sapply(w_data$date, julian_conv)
w_data <- cbind(jd,w_data)
rm(jd)

# Calculate julian day for daily data
jd <- sapply(w_data_daily$date, julian_conv)
w_data_daily <- cbind(round(jd), w_data_daily)
rm(jd)

# Calculate julian day for monthly data
jd <- sapply(w_data_monthly$date, julian_conv)
w_data_monthly <- cbind(round(jd), w_data_monthly)
rm(jd)

#### Calculate vapor pressure deficit ####
# Hourly average of vpd
vpd <- vap_deficit(w_data$AirTempC_Avg, w_data$RelHum_Avg)
# Daily average of vpd
vpd_daily <- vap_deficit(w_data_daily$AirTempC_Avg, w_data_daily$RelHum_Avg)
# Monthly average of vpd
vpd_monthly <- vap_deficit(w_data_monthly$AirTempC_Avg, 
                           w_data_monthly$RelHum_Avg)
# Combine with data frame
w_data <- cbind(w_data, vpd)
w_data_daily <- cbind(w_data_daily, vpd_daily)
w_data_monthly <- cbind(w_data_monthly, vpd_monthly)
rm(vpd, vpd_daily, vpd_monthly)

#### Wind rose ####
windRose(w_data, ws = 'WindSpd_ms_Avg', wd = 'WindDir_Avg', paddle = FALSE)

#### Plots ####
# Monthly rain
plot(w_data_monthly$date,w_data_monthly$Rain_mm_Tot, type = 'o', ylim = c(0, 4),
     ylab = 'Cumulative rain (mm)', xlab = 'Month', main = 'Monthly rain')
# Temperature
plot(w_data_monthly$date,w_data_monthly$AirTempC_Avg, 
     type = 'o',
     ylab = 'Temperature (C)', xlab = 'Month', main = 'Monthly temperature')
# Vapor pressure deficit
plot(w_data_monthly$date,w_data_monthly$vpd_monthly, 
     type = 'o',
     ylab = 'VPD (kPa)', xlab = 'Month', main = 'Monthly vapor pressure deficit')


