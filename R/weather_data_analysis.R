#### Weather station data analysis ####
# Author: Yusri Yusup, PhD
# Affiliation: Universiti Sains Malaysia
# Date created: 2016-06-22
# Note: R script to analyze MPOB weather station data
# Version: 1.0
#

#### Preliminaries ####
library(openair)
library(dplyr) # To group the data for cumulative rain calculations
source('R/vap_deficit.R') # To calculate vapor pressure deficit [kPa]
source('R/julian_conv.R') # To calculate julian day
source('R/ET.R') # To calculate evapotranspiration [mm time-1]


#### Data import ####
# Import the data
w_data <- read.csv(file = file.choose(), sep = ',', skip = 1)
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


#### Data quality checks ####
# To be done in class

#### Averaging ####
# Note that rain is also averaged though the sum is needed
w_data_monthly <- timeAverage(w_data, avg.time = 'month')
w_data_daily <- timeAverage(w_data, avg.time = 'day')

#### Julian day ####
# Calculate julian day for hourly data
jd <- sapply(w_data$date, julian_conv)
w_data <- cbind(jd,w_data)
rm(jd)

# Calculate julian day for daily data
# Must use sapply() because there is an if statement within the function
jd <- sapply(w_data_daily$date, julian_conv)
w_data_daily <- cbind(round(jd), w_data_daily) # To make sure it is an integer
names(w_data_daily)[1] <- 'jd'
rm(jd)

# Calculate julian day for monthly data
jd <- sapply(w_data_monthly$date, julian_conv)
w_data_monthly <- cbind(round(jd), w_data_monthly) # To make sure it is an integer
names(w_data_monthly)[1] <- 'jd'
rm(jd)

#### Calculate vapor pressure deficit ####
# Assuming we use average air temperature, might change to max air temperature
# Hourly average of vpd
vpd <- vap_deficit(w_data$AirTempC_Avg, w_data$RelHum_Avg)
# Daily average of vpd
vpd_daily <- vap_deficit(w_data_daily$AirTempC_Avg, w_data_daily$RelHum_Avg)
# Monthly average of vpd
vpd_monthly <- vap_deficit(w_data_monthly$AirTempC_Avg, 
                           w_data_monthly$RelHum_Avg)

#### Calculate evapotranspiration ####
# Can only calculate from daily and monthly averaged values
et_daily <- ET(j = w_data_daily$jd,
               Tmin = w_data_daily$AirTempC_Min,
               Tmax = w_data_daily$AirTempC_Max,
               solar_rad = w_data_daily$SlrRad_W_Avg,
               wind_speed = w_data_daily$WindSpd_ms_Avg,
               z = 2, # Assuming the measured wind is at 2 m
               RHmax = w_data_daily$RelHum_Max,
               RHmin = w_data_daily$RelHum_Min,
               lat_deg = 5, # Assuming the latitude of the station is at 5 deg
               a = 0.23) # Albedo reference evapotranspiration for grass surface 
et_monthly <- ET(j = w_data_monthly$jd,
                 Tmin = w_data_monthly$AirTempC_Min,
                 Tmax = w_data_monthly$AirTempC_Max,
                 solar_rad = w_data_monthly$SlrRad_W_Avg,
                 wind_speed = w_data_monthly$WindSpd_ms_Avg,
                 z = 2, # Assuming the measured wind is at 2 m
                 RHmax = w_data_monthly$RelHum_Max,
                 RHmin = w_data_monthly$RelHum_Min,
                 lat_deg = 5, # Assuming the latitude of the station is at 5 deg
                 a = 0.23) # Albedo reference evapotranspiration for grass surface

# Combine with data frame
w_data <- cbind(w_data, vpd)
w_data_daily <- cbind(w_data_daily, vpd_daily, et_daily)
w_data_monthly <- cbind(w_data_monthly, vpd_monthly, et_monthly)
rm(vpd, vpd_daily, vpd_monthly, et_daily, et_monthly)

#### Cumulative rain (but just for rain) ####
# Sum the daily rain
rain_daily <- w_data %>%
  group_by(year = format(as.POSIXct(cut(date, breaks = 'year')), '%Y'),
           month = format(as.POSIXct(cut(date, breaks = 'month')), '%m'),
           day = format(as.POSIXct(cut(date, breaks = 'day')), '%d')) %>%
  summarize(rain_daily_tot = sum(Rain_mm_Tot, na.rm = TRUE))
# Add the dates in POSIX format
date <- paste(rain_daily$year, rain_daily$month, rain_daily$day, sep = '-')
date <- as.POSIXct(date)
rain_daily <- cbind(date, rain_daily)

# Sum the monthly rain
rain_monthly <- w_data %>%
  group_by(year = format(as.POSIXct(cut(date, breaks = 'year')), '%Y'),
           month = format(as.POSIXct(cut(date, breaks = 'month')), '%m')) %>%
  summarize(rain_monthly_tot = sum(Rain_mm_Tot, na.rm = TRUE))
date <- paste(rain_monthly$year, rain_monthly$month, '01', sep = '-')
date <- as.POSIXct(date)
rain_monthly <- cbind(date, rain_monthly)

#### Mean max T ####
# Mean maximum temperature between 0900 and 1300 daily
maxT_0900_1300 <- w_data
maxT_0900_1300 <- selectByDate(maxT_0900_1300,start=maxT_0900_1300$date[1],
                               end=maxT_0900_1300$date[nrow(maxT_0900_1300)],
                               hour=1:5) # Because hour 1 GMT is 9 MYT
maxT_0900_1300$date <- as.POSIXct(format(maxT_0900_1300$date, 
                                         tz = 'Asia/Kuala_Lumpur',
                                         usetz = TRUE))# Change it to MYT
maxT_daily <- maxT_0900_1300 %>%
  group_by(year = format(as.POSIXct(cut(date, breaks = 'year')), '%Y'),
           month = format(as.POSIXct(cut(date, breaks = 'month')), '%m'),
           day = format(as.POSIXct(cut(date, breaks = 'day')), '%d'))%>%
  summarize(maxT_0900_1300 = mean(AirTempC_Max, na.rm = TRUE))

# Add the dates in POSIX format
date <- paste(maxT_daily$year, maxT_daily$month, maxT_daily$day, sep = '-')
date <- as.POSIXct(date)
maxT_daily <- cbind(date, maxT_daily)
rm(maxT_0900_1300)

#### Max VPD ####
# Mean maximum temperature between 0900 and 1300 daily
maxVPD_0900_1300 <- w_data
maxVPD_0900_1300 <- selectByDate(maxVPD_0900_1300,start=maxVPD_0900_1300$date[1],
                                 end=maxVPD_0900_1300$date[nrow(maxVPD_0900_1300)],
                                 hour=1:5) # Because hour 1 GMT is 9 MYT
maxVPD_0900_1300$date <- as.POSIXct(format(maxVPD_0900_1300$date, 
                                           tz = 'Asia/Kuala_Lumpur',
                                           usetz = TRUE))# Change it to MYT
maxVPD_daily <- maxVPD_0900_1300 %>%
  group_by(year = format(as.POSIXct(cut(date, breaks = 'year')), '%Y'),
           month = format(as.POSIXct(cut(date, breaks = 'month')), '%m'),
           day = format(as.POSIXct(cut(date, breaks = 'day')), '%d'))%>%
  summarize(maxVPD_0900_1300 = max(vpd, na.rm = TRUE))

# Add the dates in POSIX format
date <- paste(maxVPD_daily$year, maxVPD_daily$month, maxVPD_daily$day, sep = '-')
date <- as.POSIXct(date)
maxVPD_daily <- cbind(date, maxVPD_daily)
rm(maxVPD_0900_1300,date)

#### Descriptive statistics ####
summary(w_data)
summary(w_data_daily)
summary(w_data_monthly)

#### Plots ####

## Change your "settings" here
# The default date format for POSIX data types
format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2011-04-01 00:00:00'
date2 <- '2011-05-30 00:00:00'

## Time series plot
# Daily cumulative rain
plot(rain_daily$date, rain_daily$rain_daily_tot, type = 'o', 
     xaxt = 'n', xlab = 'Date',
     ylab = 'Total rain (mm)',
     xlim = c(as.POSIXct(date1, format = format_date), 
              as.POSIXct(date2, format = format_date)))
axis.POSIXct(side = 1, at = rain_daily$date, 
             labels = format(rain_daily$date, '%m/%d'))

# Monthly cumulative rain
plot(rain_monthly$date, rain_monthly$rain_monthly_tot, type = 'o', 
     xaxt = 'n', xlab = 'Month',
     ylab = 'Total rain (mm)',
     xlim = c(as.POSIXct(date1, format = format_date), 
              as.POSIXct(date2, format = format_date)))
axis.POSIXct(side = 1, at = rain_monthly$date, 
             labels = format(rain_monthly$date, '%m'))

# Daily mean max T between 0900 and 1300
plot(maxT_daily$date, maxT_daily$maxT_0900_1300, type = 'o', 
     xaxt = 'n', xlab = 'Date',
     ylab = 'Mean max T (C)',
     xlim = c(as.POSIXct(date1, format = format_date), 
              as.POSIXct(date2, format = format_date)))
axis.POSIXct(side = 1, at = maxT_daily$date, 
             labels = format(maxT_daily$date, '%m/%d'))

# Daily max VPD between 0900 and 1300
plot(maxVPD_daily$date, maxVPD_daily$maxVPD_0900_1300, type = 'o', 
     xaxt = 'n', xlab = 'Date',
     ylab = 'Max VPD (kPa)',
     xlim = c(as.POSIXct(date1, format = format_date), 
              as.POSIXct(date2, format = format_date)))
axis.POSIXct(side = 1, at = maxT_daily$date, 
             labels = format(maxT_daily$date, '%m/%d'))

## Monthly averaged 
# Monthly averaged temperature
plot(w_data_monthly$date,w_data_monthly$AirTempC_Avg, 
     type = 'o', xaxt ='n',
     ylab = 'Temperature (C)', xlab = 'Month', main = 'Monthly temperature')
axis.POSIXct(side = 1, at = w_data_monthly$date, 
             labels = format(w_data_monthly$date, '%Y/%m'))

# Monthly averaged vapor pressure deficit
plot(w_data_monthly$date,w_data_monthly$vpd_monthly, 
     type = 'o', xaxt ='n',
     ylab = 'VPD (kPa)', xlab = 'Month', main = 'Monthly vapor pressure deficit')
axis.POSIXct(side = 1, at = w_data_monthly$date, 
             labels = format(w_data_monthly$date, '%Y/%m'))

# Monthly averaged evapotranspiration
png(filename = 'figs/et_monthly.jpg', width = 8, height = 8, res = 400, units = 'cm',
    pointsize = 8)
plot(w_data_monthly$date,w_data_monthly$et_monthly, 
     type = 'o', xaxt ='n',
     ylab = 'ET (mm day-1)', xlab = 'Month/Year', main = 'Monthly evapotranspiration')
axis.POSIXct(side = 1, at = w_data_monthly$date, 
             labels = format(w_data_monthly$date, '%Y/%m'))
dev.off()

# Openair package
#### Wind rose ####
windRose(w_data, ws = 'WindSpd_ms_Avg', wd = 'WindDir_Avg', paddle = FALSE)

#### Time plots ####
# Wind speed, wind direction, rain, air temperature
png(filename = 'figs/met_plot_all.jpg', width = 16, height = 16, res = 400, 
    units = 'cm')
timePlot(w_data,#selectByDate(w_data, start = "1/4/2011", end = "30/12/2011"),
         pollutant = c("WindSpd_ms_Avg",'WindDir_Avg','Rain_mm_Tot','AirTempC_Avg'),
         avg.time="day",data.thresh = 5,
         key=FALSE,
         name.pol=c(expression(paste('WS (m s'^'-1',')')),
                    expression(paste('WD (',degree,')')),
                    'Rain (mm)',
                    expression(paste("T (",degree,'C)')),'RH (%)'
                    ),
         date.format="%Y/%m",date.breaks=6,y.relation="free",
         ylab = "Penor",
         smooth=F,lwd = 1,col = c("grey30",'grey30','grey30','grey30'),
         lty = c(1,1,1,1),fontsize=10)
dev.off()

# Wind speed, wind direction, rain, air temperature
png(filename = 'figs/met_plot_all2.jpg', width = 16, height = 16, res = 400, 
    units = 'cm')
timePlot(w_data,#selectByDate(w_data, start = "1/4/2011", end = "30/12/2011"),
         pollutant = c('RelHum_Avg','SlrRad_W_Avg','HeatIndxC_Max','vpd'),
         avg.time="day",data.thresh = 5,
         key=FALSE,
         name.pol=c('RH (%)',
                    expression(paste('Solar radiation (','W m'^'-1', ')')),
                    'Heat index (max)',
                    'VPD'),
         date.format="%Y/%m",date.breaks=6,y.relation="free",
         ylab = "Penor",
         smooth=F,lwd = 1,col = c("grey30",'grey30','grey30','grey30'),
         lty = c(1,1,1,1),fontsize=10)
dev.off()
