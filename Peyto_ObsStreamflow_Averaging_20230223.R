#Code to average Peyto streamflow data to daily

setwd('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/18_Streamflow_Data/00_Raw/Peyto')


#Data is in MST timezone

recent_data <- read.csv('Q_SR50_15min_2013_2021.csv')

streamflow_15min <- recent_data$Q[45:311948]


streamflow_daily <- vector()

for (m in 2:((311904/96)-1)) {
  
  streamflow_daily[m+1] <- mean(streamflow_15min[(m*96 + 1):(m*96 + 96)])
  
}

plot(streamflow_daily, type = 'l')


dates <- seq(as.POSIXct('2013-05-01', format = '%Y-%m-%d'), as.POSIXct('2022-03-23 00:00:00'), 'days')


daily_dataframe <- data.frame(dates, streamflow_daily)

