#### Weather station data analysis ####
# Author: Yusri Yusup
# Date created: 2016-06-22
# Version: 1.0
#


## Preliminaries ####
library(openair)
## Data import ####

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

## Analysis ####
# Averaging
w_data_monthly <- timeAverage(w_data, avg.time = 'month')
w_data_daily <- timeAverage(w_data, avg.time = 'day')

# Wind rose
windRose(w_data, ws = 'WindSpd_ms_Avg', wd = 'WindDir_Avg', paddle = FALSE)

