setwd('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/02_Runs/Run_20')

library(zoo)


streamflow <- matrix(nrow = 52608, ncol = 20)
albedo <- matrix(nrow = 52608, ncol = 20)
swe <- matrix(nrow = 52608, ncol = 20)
z_s <- matrix(nrow = 52608, ncol = 20)


#Ensemble number loop

for (i in 1:20) {
  
  streamflow_vec <- vector()
  albedo_vec <- vector()
  swe_vec <- vector()
  z_s_vec <- vector()
  
  
  #Time step loop
  
  for (j in 1:69) {
    
    if (j > 2) {
    
    crhm_output <- read.csv(paste0('CRHM_output_En', i, '_t', j, '.csv'))[-c(1,2),]
    
    streamflow_vec <- append(streamflow_vec, crhm_output$V2[73:length(crhm_output$V2)])
    albedo_vec <- append(albedo_vec, crhm_output$V3[73:length(crhm_output$V3)])
    swe_vec <- append(swe_vec, crhm_output$V4[73:length(crhm_output$V4)])
    z_s_vec <- append(z_s_vec, crhm_output$V5[73:length(crhm_output$V5)])
    
    } else {
      
      crhm_output <- read.csv(paste0('CRHM_output_En', i, '_t', j, '.csv'))[-c(1,2),]
      
      streamflow_vec <- append(streamflow_vec, crhm_output$V2)
      albedo_vec <- append(albedo_vec, crhm_output$V3)
      swe_vec <- append(swe_vec, crhm_output$V4)
      z_s_vec <- append(z_s_vec, crhm_output$V5)
      
    }
    
  }
  
  
  streamflow[,i] <- as.numeric(streamflow_vec)/3600
  albedo[,i] <- as.numeric(albedo_vec)
  swe[,i] <- as.numeric(swe_vec)
  z_s[,i] <- as.numeric(z_s_vec)
  
}


#Streamflow ensembles

streamflow_mean <- rowMeans(streamflow)


streamflow_mean <- vector()

for (k in 1:52608) {
  
  streamflow_mean[k] <- mean(streamflow[k,])
  
}


streamflow_min <- vector()

for (k in 1:52608) {
  
  streamflow_min[k] <- min(streamflow[k,])
  
}


streamflow_max <- vector()

for (l in 1:52608) {
  
  streamflow_max[l] <- max(streamflow[l,])
  
}


plot(1:52608, streamflow_max, type = 'l', col = 'red')
lines(1:52608, streamflow_mean)
lines(1:52608, streamflow_min, col = 'blue')


#Albedo ensembles at station HRU

albedo_mean <- rowMeans(albedo)


albedo_min <- vector()

for (k in 1:52608) {
  
  albedo_min[k] <- min(albedo[k,])
  
}


albedo_max <- vector()

for (l in 1:52608) {
  
  albedo_max[l] <- max(albedo[l,])
  
}


plot(1:52608, albedo_max, type = 'l', col = 'red', ylim = c(0.15,0.9))
lines(1:52608, albedo_mean)
lines(1:52608, albedo_min, col = 'blue')


#SWE ensembles at station HRU

swe_mean <- rowMeans(swe)


swe_min <- vector()

for (k in 1:52608) {
  
  swe_min[k] <- min(swe[k,])
  
}


swe_max <- vector()

for (l in 1:52608) {
  
  swe_max[l] <- max(swe[l,])
  
}


plot(1:52608, swe_max, type = 'l', col = 'red')
lines(1:52608, swe_mean)
lines(1:52608, swe_min, col = 'blue')


#Snow depth ensembles at station HRU

z_s_mean <- rowMeans(z_s)


z_s_min <- vector()

for (k in 1:52608) {
  
  z_s_min[k] <- min(z_s[k,])
  
}


z_s_max <- vector()

for (l in 1:52608) {
  
  z_s_max[l] <- max(z_s[l,])
  
}


plot(1:52608, z_s_max, type = 'l', col = 'red')
lines(1:52608, z_s_mean)
lines(1:52608, z_s_min, col = 'blue')



#Streamflow daily


streamflow_daily <- vector()

for (m in 2:((52608/24)-1)) {
  
  streamflow_daily[m+1] <- mean(streamflow_mean[(m*24 + 1):(m*24 + 24)])
  
}

streamflow_daily <- c(NA, NA, na.approx(streamflow_daily))

plot(streamflow_daily, type = 'l')



#Control run

control_run <- read.table('C:/users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/02_Runs/Control_9/CRHM_output_control9_20230412.txt')[-c(1, 2),]


streamflow_control <- as.numeric(control_run$V2)/3600


streamflow_ctrl_daily <- vector()

for (m in 0:((52608/24)-1)) {
  
  streamflow_ctrl_daily[m+1] <- mean(streamflow_control[(m*24 + 1):(m*24 + 24)])
  
}



plot(1:(52608/24), streamflow_daily, type = 'l')
lines(1:(52608/24), streamflow_ctrl_daily, col = 'blue')


#Observed daily streamflow

obs_streamflow <- read.table('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/18_Streamflow_Data/00_Raw/Sunwapta_River_07AA007_daily_obs_flow_m3s_1948_2021.txt', sep = '\t', header = T)

dates <- seq(as.POSIXct('1948-01-01', format = '%Y-%m-%d'), as.POSIXct('2022-01-01', format = '%Y-%m-%d'), 'days')

obs_streamflow_daily <- obs_streamflow$daily.flow..m3.s.[24746:26937]

obs_streamflow_daily <- ifelse(is.nan(obs_streamflow_daily), NA, obs_streamflow_daily)


#Evaluation plot

plot(1:1461, streamflow_daily[732:2192], col = 'blue', type = 'l')
lines(1:1461, streamflow_ctrl_daily[732:2192], col = 'red')
lines(1:1461, obs_streamflow_daily[732:2192], col = 'black')


#Evaluation

q_obs_kge <- obs_streamflow_daily[732:2192][which(!is.na(obs_streamflow_daily[732:2192]))]
q_da_kge <- streamflow_daily[732:2192][which(!is.na(obs_streamflow_daily[732:2192]))]
q_ctrl_kge <- streamflow_ctrl_daily[732:2192][which(!is.na(obs_streamflow_daily[732:2192]))]


NSE_DA = 1 - sum((q_obs_kge - q_da_kge)^2)/sum((q_obs_kge - mean(q_obs_kge))^2)

NSE_ctrl = 1 - sum((q_obs_kge - q_ctrl_kge)^2)/sum((q_obs_kge - mean(q_obs_kge))^2)

round(NSE_DA, 2)
round(NSE_ctrl, 2)


mb_DA = mean(q_da_kge - q_obs_kge)

mb_ctrl = mean(q_ctrl_kge - q_obs_kge)

round(mb_DA, 2)
round(mb_ctrl, 2)


rmse_DA = sqrt(mean((q_da_kge - q_obs_kge)^2))

rmse_ctrl = sqrt(mean((q_ctrl_kge - q_obs_kge)^2))

round(rmse_DA, 2)
round(rmse_ctrl, 2)


r_da = summary(lm(q_da_kge ~ q_obs_kge))

kge_da = 1 - sqrt(((sqrt(r_da$r.squared) - 1)^2) + (((sd(q_da_kge)/sd(q_obs_kge)) - 1)^2) + (((mean(q_da_kge)/mean(q_obs_kge)) - 1)^2))


r_ctrl = summary(lm(q_ctrl_kge ~ q_obs_kge))

kge_ctrl = 1 - sqrt(((sqrt(r_ctrl$r.squared) - 1)^2) + (((sd(q_ctrl_kge)/sd(q_obs_kge)) - 1)^2) + (((mean(q_ctrl_kge)/mean(q_obs_kge)) - 1)^2))

round(kge_da, 2)
round(kge_ctrl, 2)


#Obs CV

round(sd(q_obs_kge)/mean(q_obs_kge), 2)


#Plot ensembles

year_17_18 <- streamflow_daily[732:1096]
year_18_19 <- streamflow_daily[1097:1461]
year_19_20 <- c(streamflow_daily[1462:1612], streamflow_daily[1614:1827]) 
year_20_21 <- streamflow_daily[1828:2192]


da_annual_flow <- vector()

for (i in 1:365) {
  
  da_annual_flow[i] <- mean(c(year_17_18[i], year_18_19[i], year_19_20[i], year_20_21[i]), na.rm = T)
  
}


dates_plot <- seq(as.POSIXct('2020-05-01', format = '%Y-%m-%d'), as.POSIXct('2020-09-30', format = '%Y-%m-%d'), 'days')


tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Cumulative_Runoff_20230217.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

par(mar=c(4,4,1,1))

plot(dates_plot, cumsum(((da_annual_flow[213:365]*(24*3600))/2.93e7)*1000), type = 'l', col = 'blue', lwd = 1, ylim = c(0,1800), ylab = 'Cumulative Runoff [mm]', xlab = 'Time [days]')
legend('topleft', legend = c('Observed', 'Control', 'Data Assimilation'), col = c('black', 'red', 'blue'), lty = c(1,1,1), lwd = c(1,1,1))

year_17_18 <- obs_streamflow_daily[732:1096]
year_18_19 <- obs_streamflow_daily[1097:1461]
year_19_20 <- c(obs_streamflow_daily[1462:1612], obs_streamflow_daily[1614:1827]) 
year_20_21 <- obs_streamflow_daily[1828:2192]


obs_annual_flow <- vector()

for (i in 1:365) {
  
  obs_annual_flow[i] <- mean(c(year_17_18[i], year_18_19[i], year_19_20[i], year_20_21[i]), na.rm = T)
  
}


lines(dates_plot, cumsum(((obs_annual_flow[213:365]*(24*3600))/2.93e7)*1000), col = 'black', lwd = 1)


year_17_18 <- streamflow_ctrl_daily[732:1096]
year_18_19 <- streamflow_ctrl_daily[1097:1461]
year_19_20 <- c(streamflow_ctrl_daily[1462:1612], streamflow_ctrl_daily[1614:1827]) 
year_20_21 <- streamflow_ctrl_daily[1828:2192]


ctrl_annual_flow <- vector()

for (i in 1:365) {
  
  ctrl_annual_flow[i] <- mean(c(year_17_18[i], year_18_19[i], year_19_20[i], year_20_21[i]), na.rm = T)
  
}


lines(dates_plot, cumsum(((ctrl_annual_flow[213:365]*(24*3600))/2.93e7)*1000), col = 'red', lwd = 1)

dev.off()


tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Annual_Flow_20230502.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

par(mar=c(4,4.3,1,1))

plot(dates_plot, ctrl_annual_flow[213:365], type = 'l', col = 'firebrick2', lwd = 1, ylim = c(0,14), ylab = expression(paste("Streamflow [", m^3*s^-1, "]")), xlab = 'Time [days]')
legend('topleft', legend = c('Observed', 'Control', 'Data Assimilation'), col = c('black', 'firebrick2', 'royalblue3'), lty = c(1,1,1), lwd = c(1,1,1))

lines(dates_plot, da_annual_flow[213:365], col = 'royalblue3', lwd = 1)

lines(dates_plot, obs_annual_flow[213:365], col = 'black', lwd = 1)

dev.off()


#2017/2018 plots


dates_plot_2018 <- seq(as.POSIXct('2021-05-01', format = '%Y-%m-%d'), as.POSIXct('2021-09-30', format = '%Y-%m-%d'), 'days')


tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Cumulative_Runoff_2021_20230217.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

par(mar=c(4,4,1,1))

plot(dates_plot_2018, cumsum(((streamflow_daily[2040:2192]*(24*3600))/2.93e7)*1000), type = 'l', col = 'blue', lwd = 1, ylim = c(0,1800), ylab = 'Cumulative Runoff [mm]', xlab = 'Time [days]')
legend('topleft', legend = c('Observed', 'Control', 'Data Assimilation'), col = c('black', 'red', 'blue'), lty = c(1,1,1), lwd = c(1,1,1))

lines(dates_plot_2018, cumsum(((obs_streamflow_daily[2040:2192]*(24*3600))/2.93e7)*1000), col = 'black', lwd = 1)

lines(dates_plot_2018, cumsum(((streamflow_ctrl_daily[2040:2192]*(24*3600))/2.93e7)*1000), col = 'red', lwd = 1)

dev.off()



tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Flow_2021_20230419.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

par(mar=c(4,4,1,1))

plot(dates_plot_2018, streamflow_ctrl_daily[2040:2192], type = 'l', col = 'red', lwd = 1, ylim = c(0,14), ylab = 'Streamflow [m3/s]', xlab = 'Time [days]')
legend('topleft', legend = c('Observed', 'Control', 'Data Assimilation'), col = c('black', 'red', 'blue'), lty = c(1,1,1), lwd = c(1,1,1))

lines(dates_plot_2018, streamflow_daily[2040:2192], col = 'blue', lwd = 1)

lines(dates_plot_2018, obs_streamflow_daily[2040:2192], col = 'black', lwd = 1)

dev.off()



#Yearly Statistics

year <- 2021

ys <- ifelse(year == 2018, 732, ifelse(year == 2019, 1097, ifelse(year == 2020, 1462, 1828)))
ye <- ifelse(year == 2018, 1096, ifelse(year == 2019, 1461, ifelse(year == 2020, 1827, 2192)))

#Evaluation

q_obs_kge <- obs_streamflow_daily[ys:ye][which(!is.na(obs_streamflow_daily[ys:ye]))]
q_da_kge <- streamflow_daily[ys:ye][which(!is.na(obs_streamflow_daily[ys:ye]))]
q_ctrl_kge <- streamflow_ctrl_daily[ys:ye][which(!is.na(obs_streamflow_daily[ys:ye]))]


NSE_DA = 1 - sum((q_obs_kge - q_da_kge)^2)/sum((q_obs_kge - mean(q_obs_kge))^2)

NSE_ctrl = 1 - sum((q_obs_kge - q_ctrl_kge)^2)/sum((q_obs_kge - mean(q_obs_kge))^2)

round(NSE_DA, 2)
round(NSE_ctrl, 2)


mb_DA = mean(q_da_kge - q_obs_kge)

mb_ctrl = mean(q_ctrl_kge - q_obs_kge)

round(mb_DA, 2)
round(mb_ctrl, 2)


rmse_DA = sqrt(mean((q_da_kge - q_obs_kge)^2))

rmse_ctrl = sqrt(mean((q_ctrl_kge - q_obs_kge)^2))

round(rmse_DA, 2)
round(rmse_ctrl, 2)


r_da = summary(lm(q_da_kge ~ q_obs_kge))

kge_da = 1 - sqrt(((sqrt(r_da$r.squared) - 1)^2) + (((sd(q_da_kge)/sd(q_obs_kge)) - 1)^2) + (((mean(q_da_kge)/mean(q_obs_kge)) - 1)^2))


r_ctrl = summary(lm(q_ctrl_kge ~ q_obs_kge))

kge_ctrl = 1 - sqrt(((sqrt(r_ctrl$r.squared) - 1)^2) + (((sd(q_ctrl_kge)/sd(q_obs_kge)) - 1)^2) + (((mean(q_ctrl_kge)/mean(q_obs_kge)) - 1)^2))

round(kge_da, 2)
round(kge_ctrl, 2)


#Obs CV

round(sd(q_obs_kge)/mean(q_obs_kge), 2)


#Albedo RS Evaluation

athabasca_eval <- read.csv('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/14_Tables/Athabasca_Stations_Eval_20230416.csv', sep = ';')

reg_model <- summary(lm(athabasca_eval$obs ~ athabasca_eval$rs))


#Correlation

round(sqrt(reg_model$r.squared), 2)

#Bias

round(mean(athabasca_eval$rs - athabasca_eval$obs), 3)

#RMSE

round(sqrt(mean((athabasca_eval$rs - athabasca_eval$obs)^2)), 3)

#standard error

round(reg_model$sigma, 3)


tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Albedo_Eval_Athabasca_20230416.tiff",
     width = 4.5, height = 4, units = "in", res = 1000, pointsize = 12)

par(mar=c(4,4,1,1))

plot(athabasca_eval$obs, athabasca_eval$rs, xlim = c(0,1), ylim = c(0,1), ylab = 'Remote Sensing Albedo [ ]', xlab = 'Observed Albedo [ ]', pch = 21, bg = 'seagreen')
abline(0, 1, lty = 2)
abline(lm(athabasca_eval$rs ~ athabasca_eval$obs), col = 'red')
legend('topleft', legend = c('Regression Line', '1:1 Line'), col = c('red', 'black'), lty = c(1,2), lwd = c(1,1))

dev.off()



#Albedo analysis

dates_full <- seq(as.POSIXct('2015-10-01 01:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2021-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')

dates_hourly <- seq(as.POSIXct('2017-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2021-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')


#Albedo

tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Albedo_20230507.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

plot(dates_hourly, control_run$V3[17544:52607], type = 'l', col = 'firebrick2', ylab = 'Albedo [ ]', xlab = 'Time [hours]', ylim = c(0.15, 0.9))
lines(dates_hourly, albedo_mean[17544:52607], col = 'royalblue3')

dev.off()


#Albedo 2018

dates_hourly_2018 <- seq(as.POSIXct('2017-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2018-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')

tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Albedo_2018_20230419.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

plot(dates_hourly_2018, control_run$V3[17544:26303], type = 'l', col = 'red', ylab = 'Albedo [ ]', xlab = 'Time [hours]', ylim = c(0.15, 0.9))
lines(dates_hourly_2018, albedo_mean[17544:26303], col = 'blue')

dev.off()


#SWE analysis

tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_SWE_20230507.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

plot(dates_hourly, control_run$V4[17544:52607], type = 'l', col = 'firebrick2', ylab = 'SWE [mm]', xlab = 'Time [hours]', ylim = c(0, 200))
lines(dates_hourly, swe_mean[17544:52607], col = 'royalblue3')

dev.off()


#snow depth analysis

tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Z_s_20230419.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

plot(dates_hourly, control_run$V5[17544:52607], type = 'l', col = 'red', ylab = 'Snow Depth [m]', xlab = 'Time [hours]', ylim = c(0, 0.8))
lines(dates_hourly, z_s_mean[17544:52607], col = 'blue')

dev.off()


#Albedo/snow depth plots


#2018

dates_hourly_2018 <- seq(as.POSIXct('2018-05-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2018-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')

tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Zs_Albedo_2018_20230421.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

plot(dates_hourly_2018, control_run$V5[22632:26303], type = 'l', col = 'red', ylab = 'Albedo [ ] and Snow Depth [m]', xlab = 'Time [hours]', ylim = c(0, 0.9), lty = 2)
lines(dates_hourly_2018, z_s_mean[22632:26303], col = 'blue', lty = 2)

lines(dates_hourly_2018, control_run$V3[22632:26303], col = 'red')
lines(dates_hourly_2018, albedo_mean[22632:26303], col = 'blue')

dev.off()


#2019

dates_hourly_2018 <- seq(as.POSIXct('2019-05-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2019-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')

tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Zs_Albedo_2019_20230421.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

plot(dates_hourly_2018, control_run$V5[31392:35063], type = 'l', col = 'red', ylab = 'Albedo [ ] and Snow Depth [m]', xlab = 'Time [hours]', ylim = c(0, 0.9), lty = 2)
lines(dates_hourly_2018, z_s_mean[31392:35063], col = 'blue', lty = 2)

lines(dates_hourly_2018, control_run$V3[31392:35063], col = 'red')
lines(dates_hourly_2018, albedo_mean[31392:35063], col = 'blue')

dev.off()


#2020

dates_hourly_2018 <- seq(as.POSIXct('2020-05-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2020-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')

tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Zs_Albedo_2020_20230421.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

plot(dates_hourly_2018, control_run$V5[40176:43847], type = 'l', col = 'red', ylab = 'Albedo [ ] and Snow Depth [m]', xlab = 'Time [hours]', ylim = c(0, 0.9), lty = 2)
lines(dates_hourly_2018, z_s_mean[40176:43847], col = 'blue', lty = 2)

lines(dates_hourly_2018, control_run$V3[40176:43847], col = 'red')
lines(dates_hourly_2018, albedo_mean[40176:43847], col = 'blue')

dev.off()


#2021

dates_hourly_2018 <- seq(as.POSIXct('2021-05-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2021-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')

tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Atha_Var5_Zs_Albedo_2021_20230421.tiff",
     width = 6, height = 4, units = "in", res = 1000, pointsize = 12)

plot(dates_hourly_2018, control_run$V5[48936:52607], type = 'l', col = 'red', ylab = 'Albedo [ ] and Snow Depth [m]', xlab = 'Time [hours]', ylim = c(0, 0.9), lty = 2)
lines(dates_hourly_2018, z_s_mean[48936:52607], col = 'blue', lty = 2)

lines(dates_hourly_2018, control_run$V3[48936:52607], col = 'red')
lines(dates_hourly_2018, albedo_mean[48936:52607], col = 'blue')

dev.off()



#t-test for different means

year_17_18 <- streamflow_daily[944:1096]
year_18_19 <- streamflow_daily[1309:1461]
year_19_20 <- streamflow_daily[1675:1827] 
year_20_21 <- streamflow_daily[2040:2192]

year_17_18_ctrl <- streamflow_ctrl_daily[944:1096]
year_18_19_ctrl <- streamflow_ctrl_daily[1309:1461]
year_19_20_ctrl <- streamflow_ctrl_daily[1675:1827] 
year_20_21_ctrl <- streamflow_ctrl_daily[2040:2192]

year_17_18_obs <- obs_streamflow_daily[944:1096]
year_18_19_obs <- obs_streamflow_daily[1309:1461]
year_19_20_obs <- obs_streamflow_daily[1675:1827] 
year_20_21_obs <- obs_streamflow_daily[2040:2192]


t.test(year_17_18, year_17_18_ctrl)
t.test(year_17_18_obs, year_17_18_ctrl)
t.test(year_17_18_obs, year_17_18)

t.test(year_18_19, year_18_19_ctrl)
t.test(year_18_19_obs, year_18_19_ctrl)
t.test(year_18_19_obs, year_18_19)

t.test(year_19_20, year_19_20_ctrl)
t.test(year_19_20_obs, year_19_20_ctrl)
t.test(year_19_20_obs, year_19_20)

t.test(year_20_21, year_20_21_ctrl)
t.test(year_20_21_obs, year_20_21_ctrl)
t.test(year_20_21_obs, year_20_21)


total_flow <- c(year_17_18, year_18_19, year_19_20, year_20_21)

total_flow_control <- c(year_17_18_ctrl, year_18_19_ctrl, year_19_20_ctrl, year_20_21_ctrl)

total_flow_obs <- c(year_17_18_obs, year_18_19_obs, year_19_20_obs, year_20_21_obs)

t.test(total_flow, total_flow_control)

t.test(total_flow_obs, total_flow_control)

t.test(total_flow_obs, total_flow)


#Whole period plots

dates_four_year <- seq(as.POSIXct('2017-10-01', format = '%Y-%m-%d'), as.POSIXct('2021-09-30', format = '%Y-%m-%d'), 'days')


tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Four_year_Athabasca_20230507.tiff",
     width = 12, height = 4, units = "in", res = 1000, pointsize = 12)

par(mar=c(4,4.3,1,1))

plot(dates_four_year, streamflow_ctrl_daily[732:2192], type = 'l', col = 'firebrick2', lwd = 1, ylim = c(0,15), ylab = expression(paste("Streamflow [", m^3*s^-1, "]")), xlab = 'Time [days]')
legend('topleft', legend = c('Observed', 'Control', 'Data Assimilation'), col = c('black', 'firebrick2', 'royalblue3'), lty = c(1,1,1), lwd = c(1,1,1))

lines(dates_four_year, streamflow_daily[732:2192], col = 'royalblue3', lwd = 1)

lines(dates_four_year, obs_streamflow_daily[732:2192], col = 'black', lwd = 1)

dev.off()


#Spring/summer averages

round(mean(as.numeric(control_run$V3[which(dates_full == as.POSIXct('2021-05-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct('2021-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'))])), 2)
round(mean(albedo_mean[which(dates_full == as.POSIXct('2021-05-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct('2021-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'))]), 2)

round(mean(as.numeric(control_run$V4[which(dates_full == as.POSIXct('2021-05-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct('2021-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'))])), 1)
round(mean(swe_mean[which(dates_full == as.POSIXct('2021-05-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct('2021-09-30 23:00:00', format = '%Y-%m-%d %H:%M:%S'))]), 1)


#Ensemble analysis

start_date <- '2020-05-01 00:00:00'
end_date <- '2020-09-30 23:00:00'

dates_plot_ensemble <- seq(as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S'), as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S'), 'hours')


tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Athab_Ensemble_SWE_2021_20230524.tiff",
     width = 6, height = 3, units = "in", res = 500, pointsize = 12)

par(mar=c(4,4.3,1,1))


plot(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),1]), type = 'l', col = 'grey', ylim = c(0,360), xlab = 'Time [hours]', ylab = 'SWE [mm]')

lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),2]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),3]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),4]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),5]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),6]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),7]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),8]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),9]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),10]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),11]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),12]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),13]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),14]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),15]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),16]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),17]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),18]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),19]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(swe[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),20]), col = 'grey')


lines(dates_plot_ensemble, as.numeric(control_run$V4[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S'))]), col = 'firebrick2')

lines(dates_plot_ensemble, as.numeric(swe_mean[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S'))]), col = 'royalblue3')


dev.off()



tiff("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/15_PPTs/Plots/Athab_Ensemble_Alb_2020_20230524.tiff",
     width = 6, height = 3, units = "in", res = 500, pointsize = 12)

par(mar=c(4,4.3,1,1))


plot(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),1]), type = 'l', col = 'grey', ylim = c(0,1), xlab = 'Time [hours]', ylab = 'Albedo [ ]')

lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),2]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),3]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),4]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),5]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),6]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),7]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),8]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),9]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),10]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),11]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),12]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),13]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),14]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),15]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),16]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),17]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),18]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),19]), col = 'grey')
lines(dates_plot_ensemble, as.numeric(albedo[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S')),20]), col = 'grey')


lines(dates_plot_ensemble, as.numeric(control_run$V3[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S'))]), col = 'firebrick2')

lines(dates_plot_ensemble, as.numeric(albedo_mean[which(dates_full == as.POSIXct(start_date, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date, format = '%Y-%m-%d %H:%M:%S'))]), col = 'royalblue3')


dev.off()

