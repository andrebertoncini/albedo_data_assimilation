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



#Control run

control_run <- read.table('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/20_DA_Peyto/00_Runs/Control_2/CRHM_output_control2_20230228.txt')[-c(1, 2),]


streamflow_control <- as.numeric(control_run$V2)/3600


streamflow_ctrl_daily <- vector()

for (m in 0:((52608/24)-1)) {
  
  streamflow_ctrl_daily[m+1] <- mean(streamflow_control[(m*24 + 1):(m*24 + 24)])
  
}

plot(streamflow_ctrl_daily[732:2192], type = 'l', col = 'blue')
lines(streamflow_daily[1615:3075], col = 'black')


#Evaluation

NSE_ctrl = 1 - sum(na.omit(streamflow_daily[1615:3075] - streamflow_ctrl_daily[732:2192])^2)/sum(na.omit(streamflow_daily[1615:3075] - mean(na.omit(streamflow_daily[1615:3075])))^2)

round(NSE_ctrl, 2)


mb_ctrl = mean(streamflow_ctrl_daily[732:2192] - streamflow_daily[1615:3075], na.rm = T)

round(mb_ctrl, 2)


rmse_ctrl = sqrt(mean((streamflow_ctrl_daily[732:2192] - streamflow_daily[1615:3075])^2, na.rm = T))

round(rmse_ctrl, 2)



#Albedo RS Evaluation

athabasca_eval <- read.csv('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/14_Tables/Athabasca_Peyto_Stations_Eval_20230223.csv', sep = ';')

reg_model <- summary(lm(athabasca_eval$obs ~ athabasca_eval$rs))


#Correlation

round(sqrt(reg_model$r.squared), 2)

#Bias

round(mean(athabasca_eval$rs - athabasca_eval$obs), 3)

#RMSE

round(sqrt(mean((athabasca_eval$rs - athabasca_eval$obs)^2)), 3)

#standard error

round(reg_model$sigma, 3)


tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Albedo_Eval_Athabasca_20230206.tiff",
     width = 4.5, height = 4, units = "in", res = 1000, pointsize = 12)

par(mar=c(4,4,1,1))

plot(athabasca_eval$obs, athabasca_eval$rs, xlim = c(0,1), ylim = c(0,1), ylab = 'Remote Sensing Albedo [ ]', xlab = 'Observed Albedo [ ]', pch = 21, bg = 'seagreen')
abline(0, 1, lty = 2)
abline(lm(athabasca_eval$rs ~ athabasca_eval$obs), col = 'red')
legend('topleft', legend = c('Regression Line', '1:1 Line'), col = c('red', 'black'), lty = c(1,2), lwd = c(1,1))

dev.off()



