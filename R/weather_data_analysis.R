#### Weather station data analysis ####
# Author: Yusri Yusup, PhD
# Affiliation: Universiti Sains Malaysia
# Date created: 2016-06-22
# Note: R script to analyze MPOB weather station data
# Version: 1.0
#


#### Preliminaries ####
# Need to install openair and dplyr first
#install.packages('openair')
#install.pacakges('dplyr')
library(openair) # To plot wind rose and time plots
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
# Remove all temperatures more than 50 C
w_data$AirTempC_Max[w_data$AirTempC_Max > 50] <- NA
# Set allowable RH values between 0 and 100%
w_data$RelHum_Avg[w_data$RelHum_Avg < 40 | w_data$RelHum_Avg > 100] <- NA
w_data$RelHum_Max[w_data$RelHum_Max < 40 | w_data$RelHum_Max > 100] <- NA
w_data$RelHum_Min[w_data$RelHum_Min < 40 | w_data$RelHum_Min > 100] <- NA
# Set max dew point to 50
w_data$DewPntC_Max[w_data$DewPntC_Max > 30] <- NA
# Set min dew point to less than 0 and more than 40
w_data$DewPntC_Min[w_data$DewPntC_Min < 0 | w_data$DewPntC_Min > 40] <- NA
# Set windchill min
w_data$WindChilC_Min[w_data$WindChilC_Min < 0 | w_data$WindChilC_Min > 40] <- NA
w_data$WindChilC_Max[w_data$WindChilC_Max > 60] <- NA
# Set HeatIndex
w_data$HeatIndxC_Max[w_data$HeatIndxC_Max > 60] <- NA
w_data$HeatIndxC_Min[w_data$HeatIndxC_Min < 0 | w_data$HeatIndxC_Min > 50] <- NA

#### Averaging ####
# Note that rain is also averaged though the sum is needed
w_data_monthly <- timeAverage(w_data, avg.time = 'month')
w_data_yearly <- timeAverage(w_data, avg.time = 'year')



#### Julian day ####
# Calculate julian day for hourly data
jd <- sapply(w_data$date, julian_conv)
w_data <- cbind(jd,w_data)
rm(jd)


# Calculate julian day for monthly data
jd <- sapply(w_data_monthly$date, julian_conv)
w_data_monthly <- cbind(round(jd), w_data_monthly) # To make sure it is an integer
names(w_data_monthly)[1] <- 'jd'
rm(jd)

# Calculate julian day for yearly data
jd <- sapply(w_data_yearly$date, julian_conv)
w_data_yearly <- cbind(round(jd), w_data_yearly) # To make sure it is an integer
names(w_data_yearly)[1] <- 'jd'
rm(jd)



#### Calculate vapor pressure deficit ####
# Assuming we use average air temperature, might change to max air temperature

# Monthly average of vpd
vpd_monthly <- vap_deficit(w_data_monthly$AirTempC_Avg + 273.15,
                           w_data_monthly$RelHum_Avg)
# Yearly average of vpd
vpd_yearly <- vap_deficit(w_data_yearly$AirTempC_Avg + 273.15,
                           w_data_yearly$RelHum_Avg)


#### Calculate evapotranspiration ####
# Can only calculate from yearly and monthly averaged values

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
et_yearly <- ET(j = w_data_yearly$jd,
                Tmin = w_data_yearly$AirTempC_Min,
                Tmax = w_data_yearly$AirTempC_Max,
                solar_rad = w_data_yearly$SlrRad_W_Avg,
                wind_speed = w_data_yearly$WindSpd_ms_Avg,
                z = 2, # Assuming the measured wind is at 2 m
                RHmax = w_data_yearly$RelHum_Max,
                RHmin = w_data_yearly$RelHum_Min,
                lat_deg = 5, # Assuming the latitude of the station is at 5 deg
                a = 0.23) # Albedo reference evapotranspiration for grass surface


# Combine with data frame
w_data_monthly <- cbind(w_data_monthly, vpd_monthly, et_monthly)
w_data_yearly <- cbind(w_data_yearly, vpd_yearly, et_yearly)
rm(vpd_monthly, et_monthly, vpd_yearly, et_yearly)

#### Cumulative rain (but just for rain) ####

# Sum the monthly rain
rain_monthly <- w_data %>%
  group_by(year = format(as.POSIXct(cut(date, breaks = 'year')), '%Y'),
           month = format(as.POSIXct(cut(date, breaks = 'month')), '%m')) %>%
  summarize(rain_monthly_tot = sum(Rain_mm_Tot, na.rm = TRUE))
date <- paste(rain_monthly$year, rain_monthly$month, '01', sep = '-')
date <- as.POSIXct(date)
rain_monthly <- as.data.frame(rain_monthly)
rain_monthly <- cbind(date, rain_monthly)

# Sum the yearly rain
rain_yearly <- w_data %>%
  group_by(year = format(as.POSIXct(cut(date, breaks = 'year')), '%Y')) %>%
  summarize(rain_yearly_tot = sum(Rain_mm_Tot, na.rm = TRUE))

date <- paste(rain_yearly$year, '01', '01', sep = '-')
date <- as.POSIXct(date)
rain_yearly <- as.data.frame(rain_yearly)
rain_yearly <- cbind(date, rain_yearly)
rm(date)

#### Mean max T ####
# Mean maximum temperature between 0900 and 1300 daily
#maxT_0900_1300 <- w_data
#maxT_0900_1300 <- selectByDate(maxT_0900_1300,start=maxT_0900_1300$date[1],
#                               end=maxT_0900_1300$date[nrow(maxT_0900_1300)],
#                               hour=9:13)
#maxT_daily <- maxT_0900_1300 %>%
#  group_by(year = format(as.POSIXct(cut(date, breaks = 'year')), '%Y'),
#           month = format(as.POSIXct(cut(date, breaks = 'month')), '%m'),
#           day = format(as.POSIXct(cut(date, breaks = 'day')), '%d'))%>%
#  summarize(maxT_0900_1300 = mean(AirTempC_Max, na.rm = TRUE))

# Add the dates in POSIX format
#date <- paste(maxT_daily$year, maxT_daily$month, maxT_daily$day, sep = '-')
#date <- as.POSIXct(date)
#maxT_daily <- as.data.frame(maxT_daily)
#maxT_daily <- cbind(date, maxT_daily)
#rm(maxT_0900_1300)

# #### Max VPD ####
# # Mean maximum temperature between 0900 and 1300 daily
# maxVPD_0900_1300 <- w_data
# maxVPD_0900_1300 <- selectByDate(maxVPD_0900_1300,start=maxVPD_0900_1300$date[1],
#                                  end=maxVPD_0900_1300$date[nrow(maxVPD_0900_1300)],
#                                  hour=9:13)
# maxVPD_daily <- maxVPD_0900_1300 %>%
#   group_by(year = format(as.POSIXct(cut(date, breaks = 'year')), '%Y'),
#            month = format(as.POSIXct(cut(date, breaks = 'month')), '%m'),
#            day = format(as.POSIXct(cut(date, breaks = 'day')), '%d'))%>%
#   summarize(maxVPD_0900_1300 = max(vpd, na.rm = TRUE))
# 
# # Add the dates in POSIX format
# date <- paste(maxVPD_daily$year, maxVPD_daily$month, maxVPD_daily$day, sep = '-')
# date <- as.POSIXct(date)
# maxVPD_daily <- as.data.frame(maxVPD_daily)
# maxVPD_daily <- cbind(date, maxVPD_daily)
# rm(maxVPD_0900_1300,date)

#### Descriptive statistics ####
#summary(w_data)
#summary(w_data_daily)
#summary(w_data_monthly)

#### Plots ####

# Yearly wind rose
jpeg(filename = 'figs/windRose_year.jpg', width = 10, height = 16, res = 400, 
     units = 'cm')
windRose(w_data, ws = 'WindSpd_ms_Avg',
         wd = 'WindDir_Avg', type = 'year', paddle = FALSE, fontsize = 10)
dev.off()
# Monthly wind rose
jpeg(filename = 'figs/windRose_month.jpg', width = 16, height = 16, res = 400, 
     units = 'cm')
windRose(selectByDate(w_data, 
                      start = w_data$date[1], 
                      end = w_data$date[nrow(w_data)], year = 2011), 
         ws = 'WindSpd_ms_Avg',
         wd = 'WindDir_Avg', type = 'month', paddle = FALSE)
dev.off()

# Plot monthly rainfall

jpeg('figs/rain_monthly_2012.jpg', 
     width = 8, height = 8, unit = 'cm', 
     res = 400)
par(ps = 10, cex = 1, cex.main = 1, mar = c(4,4,0.5,0.5))

format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2012-01-01 00:00:00'
date2 <- '2012-12-31 00:00:00'
plot(rain_monthly$date, rain_monthly$rain_monthly_tot,
     xaxt = 'n',
     ylab = 'Cumulative rain (mm)',
     xlab = 'Date', type = 'o',
     xlim = c(as.POSIXct(date1, format = format_date), 
              as.POSIXct(date2, format = format_date)),
     ylim = c(0,1000))
axis.POSIXct(side = 1, at = rain_monthly$date, 
             labels = format(rain_monthly$date, '%d-%m'))
dev.off()

# Plot yearly rainfall
jpeg('figs/rain_yearly.jpeg', width = 8, height = 8, 
     units = 'cm', res = 400)
par(ps = 10, cex = 1, cex.main = 1, mar = c(4,4,0.5,0.5))
format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2011-01-01 00:00:00'
date2 <- '2013-01-01 00:00:00'
plot(rain_yearly$date, rain_yearly$rain_yearly_tot,
     ylab = 'Cumulative rain (mm)',
     xaxt = 'n',
     xlab = 'Date',
     type = 'o',
     ylim = c(0, 3000))
axis.POSIXct(side = 1, at = rain_yearly$date, 
             labels = format(rain_yearly$date, '%Y'))
dev.off()


# Plot monthly temperature
jpeg('figs/temp_monthly_2011.jpeg', width = 8, height = 8, 
     units = 'cm', res = 400)
par(ps = 10, cex = 1, cex.main = 1, mar = c(4,4,0.5,0.5))
format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2011-01-01 00:00:00'
date2 <- '2011-12-31 00:00:00'
plot(w_data_monthly$date, w_data_monthly$AirTempC_Avg,
     ylab = 'Temperature (C)',
     xaxt = 'n',
     xlab = 'Date',
     type = 'o',
     ylim = c(20, 40))
axis.POSIXct(side = 1, at = w_data_monthly$date, 
             labels = format(w_data_monthly$date, '%Y-%m'))
dev.off()

# Plot yearly temperature
jpeg('figs/temp_yearly.jpeg', width = 8, height = 8, 
     units = 'cm', res = 400)
par(ps = 10, cex = 1, cex.main = 1, mar = c(4,4,0.5,0.5))
format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2011-01-01 00:00:00'
date2 <- '2013-12-31 00:00:00'
plot(w_data_yearly$date, w_data_yearly$AirTempC_Avg,
     ylab = 'Temperature (C)',
     xaxt = 'n',
     xlab = 'Date',
     type = 'o',
     ylim = c(20,40))
axis.POSIXct(side = 1, at = w_data_yearly$date, 
             labels = format(w_data_yearly$date, '%Y'))
dev.off()


# Plot monthly RH
jpeg('figs/RH_monthly_2011.jpeg', width = 8, height = 8, 
     units = 'cm', res = 400)
par(ps = 10, cex = 1, cex.main = 1, mar = c(4,4,0.5,0.5))
format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2011-01-01 00:00:00'
date2 <- '2011-12-31 00:00:00'
plot(w_data_monthly$date, w_data_monthly$RelHum_Avg,
     ylab = 'RH (%)',
     xaxt = 'n',
     xlab = 'Date',
     type = 'o',
     ylim = c(50, 100))
axis.POSIXct(side = 1, at = w_data_monthly$date, 
             labels = format(w_data_monthly$date, '%Y-%m'))
dev.off()

# Plot yearly temperature
jpeg('figs/RH_yearly.jpeg', width = 8, height = 8, 
     units = 'cm', res = 400)
par(ps = 10, cex = 1, cex.main = 1, mar = c(4,4,0.5,0.5))
format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2011-01-01 00:00:00'
date2 <- '2013-12-31 00:00:00'
plot(w_data_yearly$date, w_data_yearly$RelHum_Avg,
     ylab = 'RH (%)',
     xaxt = 'n',
     xlab = 'Date',
     type = 'o',
     ylim = c(50, 100))
axis.POSIXct(side = 1, at = w_data_yearly$date, 
             labels = format(w_data_yearly$date, '%Y'))
dev.off()

# Plot monthly solar radiation 2011
jpeg('figs/solar_monthly_2011.jpeg', width = 8, height = 8, 
     units = 'cm', res = 400)
par(ps = 10, cex = 1, cex.main = 1, mar = c(4,4,0.5,0.5))
format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2011-01-01 00:00:00'
date2 <- '2011-12-31 00:00:00'
plot(w_data_monthly$date, w_data_monthly$SlrRad_W_Avg,
     ylab = 'Solar radiation (W m-2)',
     xaxt = 'n',
     xlab = 'Date',
     type = 'o',
     ylim = c(0, 300))
axis.POSIXct(side = 1, at = w_data_monthly$date, 
             labels = format(w_data_monthly$date, '%Y-%m'))
dev.off()

# Plot yearly solar radiation
jpeg('figs/solar_yearly.jpeg', width = 8, height = 8, 
     units = 'cm', res = 400)
par(ps = 10, cex = 1, cex.main = 1, mar = c(4,4,0.5,0.5))
format_date <- '%Y-%m-%d %H:%M:%S'
# You can specify the start and end dates here to plot
date1 <- '2011-01-01 00:00:00'
date2 <- '2013-12-31 00:00:00'
plot(w_data_yearly$date, w_data_yearly$SlrRad_W_Avg,
     ylab = 'Solar radiation (W m-2)',
     xaxt = 'n',
     xlab = 'Date',
     type = 'o',
     ylim = c(0,300))
axis.POSIXct(side = 1, at = w_data_yearly$date, 
             labels = format(w_data_yearly$date, '%Y'))
dev.off()





# #### Time plots ####
# # Wind speed, wind direction, rain, air temperature
# png(filename = 'figs/met_plot_all.jpg', width = 16, height = 16, res = 400, 
#     units = 'cm')
# timePlot(w_data,#selectByDate(w_data, start = "1/4/2011", end = "30/12/2011"),
#          pollutant = c("WindSpd_ms_Avg",'WindDir_Avg','Rain_mm_Tot','AirTempC_Avg'),
#          avg.time="day",data.thresh = 5,
#          key=FALSE,
#          name.pol=c(expression(paste('WS (m s'^'-1',')')),
#                     expression(paste('WD (',degree,')')),
#                     'Rain (mm)',
#                     expression(paste("T (",degree,'C)')),'RH (%)'
#                     ),
#          date.format="%Y/%m",date.breaks=6,y.relation="free",
#          ylab = "Penor",
#          smooth=F,lwd = 1,col = c("grey30",'grey30','grey30','grey30'),
#          lty = c(1,1,1,1),fontsize=10)
# dev.off()
# 
# # Wind speed, wind direction, rain, air temperature
# png(filename = 'figs/met_plot_all2.jpg', width = 16, height = 16, res = 400, 
#     units = 'cm')
# timePlot(w_data,#selectByDate(w_data, start = "1/4/2011", end = "30/12/2011"),
#          pollutant = c('RelHum_Avg','SlrRad_W_Avg','HeatIndxC_Max','vpd'),
#          avg.time="day",data.thresh = 5,
#          key=FALSE,
#          name.pol=c('RH (%)',
#                     expression(paste('Solar radiation (','W m'^'-1', ')')),
#                     'Heat index (max)',
#                     'VPD'),
#          date.format="%Y/%m",date.breaks=6,y.relation="free",
#          ylab = "Penor",
#          smooth=F,lwd = 1,col = c("grey30",'grey30','grey30','grey30'),
#          lty = c(1,1,1,1),fontsize=10)
# dev.off()
