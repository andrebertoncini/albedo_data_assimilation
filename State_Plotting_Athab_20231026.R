#Code to plot albedo and SWE states from CRHM outputs for Athabasca Basin

setwd('Path to QFill DA model runs')


library(zoo)

alb_snow <- matrix(nrow = 52608, ncol = 20)
alb_ice <- matrix(nrow = 52608, ncol = 20)
swe_snow <- matrix(nrow = 52608, ncol = 20)
swe_ice <- matrix(nrow = 52608, ncol = 20)
cc_s_snow <- matrix(nrow = 52608, ncol = 20)
cc_s_ice <- matrix(nrow = 52608, ncol = 20)
h2o_snow <- matrix(nrow = 52608, ncol = 20)
h2o_ice <- matrix(nrow = 52608, ncol = 20)
firn_snow <- matrix(nrow = 52608, ncol = 20)
ice_ice <- matrix(nrow = 52608, ncol = 20)



#Ensemble number loop

for (i in 1:20) {
  
  alb_snow_vec <- vector()
  alb_ice_vec <- vector()
  swe_snow_vec <- vector()
  swe_ice_vec <- vector()
  cc_s_snow_vec <- vector()
  cc_s_ice_vec <- vector()
  h2o_snow_vec <- vector()
  h2o_ice_vec <- vector()
  firn_snow_vec <- vector()
  ice_ice_vec <- vector()
  
  
  #Time step loop
  
  for (j in 1:69) {
    
    if (j > 2) {
      
      crhm_output <- read.csv(paste0('CRHM_output_En', i, '_t', j, '.csv'))[-c(1,2),]
      
      alb_snow_vec <- append(alb_snow_vec, crhm_output$V2[73:length(crhm_output$V2)])
      alb_ice_vec <- append(alb_ice_vec, crhm_output$V3[73:length(crhm_output$V3)])
      swe_snow_vec <- append(swe_snow_vec, crhm_output$V4[73:length(crhm_output$V4)])
      swe_ice_vec <- append(swe_ice_vec, crhm_output$V5[73:length(crhm_output$V5)])
      cc_s_snow_vec <- append(cc_s_snow_vec, crhm_output$V6[73:length(crhm_output$V6)])
      cc_s_ice_vec <- append(cc_s_ice_vec, crhm_output$V7[73:length(crhm_output$V7)])
      h2o_snow_vec <- append(h2o_snow_vec, crhm_output$V8[73:length(crhm_output$V8)])
      h2o_ice_vec <- append(h2o_ice_vec, crhm_output$V9[73:length(crhm_output$V9)])
      firn_snow_vec <- append(firn_snow_vec, crhm_output$V10[73:length(crhm_output$V10)])
      ice_ice_vec <- append(ice_ice_vec, crhm_output$V11[73:length(crhm_output$V11)])
      
      
    } else {
      
      crhm_output <- read.csv(paste0('CRHM_output_En', i, '_t', j, '.csv'))[-c(1,2),]
      
      alb_snow_vec <- append(alb_snow_vec, crhm_output$V2)
      alb_ice_vec <- append(alb_ice_vec, crhm_output$V2)
      swe_snow_vec <- append(swe_snow_vec, crhm_output$V2)
      swe_ice_vec <- append(swe_ice_vec, crhm_output$V2)
      cc_s_snow_vec <- append(cc_s_snow_vec, crhm_output$V2)
      cc_s_ice_vec <- append(cc_s_ice_vec, crhm_output$V2)
      h2o_snow_vec <- append(h2o_snow_vec, crhm_output$V2)
      h2o_ice_vec <- append(h2o_ice_vec, crhm_output$V2)
      firn_snow_vec <- append(firn_snow_vec, crhm_output$V2)
      ice_ice_vec <- append(ice_ice_vec, crhm_output$V2)
      
    }
    
  }
  
  
  alb_snow[,i] <- as.numeric(alb_snow_vec)
  alb_ice[,i] <- as.numeric(alb_ice_vec)
  swe_snow[,i] <- as.numeric(swe_snow_vec)
  swe_ice[,i] <- as.numeric(swe_ice_vec)
  cc_s_snow[,i] <- as.numeric(cc_s_snow_vec)
  cc_s_ice[,i] <- as.numeric(cc_s_ice_vec)
  h2o_snow[,i] <- as.numeric(h2o_snow_vec)
  h2o_ice[,i] <- as.numeric(h2o_ice_vec)
  firn_snow[,i] <- as.numeric(firn_snow_vec)
  ice_ice[,i] <- as.numeric(ice_ice_vec)

  
}


#Ensemble means

alb_snow_mean <- rowMeans(alb_snow)
alb_ice_mean <- rowMeans(alb_ice)
swe_snow_mean <- rowMeans(swe_snow)
swe_ice_mean <- rowMeans(swe_ice)
cc_s_snow_mean <- rowMeans(cc_s_snow)
cc_s_ice_mean <- rowMeans(cc_s_ice)
h2o_snow_mean <- rowMeans(h2o_snow)
h2o_ice_mean <- rowMeans(h2o_ice)
firn_snow_mean <- rowMeans(firn_snow)
ice_ice_mean <- rowMeans(ice_ice)


#Add control data

ctrl_file <- read.table("Path to control run outputs file/File.txt")

alb_snow_ctrl <- ctrl_file$V2[-c(1,2)]
alb_ice_ctrl <- ctrl_file$V3[-c(1,2)]
swe_snow_ctrl <- ctrl_file$V4[-c(1,2)]
swe_ice_ctrl <- ctrl_file$V5[-c(1,2)]
cc_s_snow_ctrl <- ctrl_file$V6[-c(1,2)]
cc_s_ice_ctrl <- ctrl_file$V7[-c(1,2)]
h2o_snow_ctrl <- ctrl_file$V8[-c(1,2)]
h2o_ice_ctrl <- ctrl_file$V9[-c(1,2)]
firn_snow_ctrl <- ctrl_file$V10[-c(1,2)]
ice_ice_ctrl <- ctrl_file$V11[-c(1,2)]


#Ploting

dates_full <- seq(as.POSIXct('2015-10-01 01:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2021-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')


start_date_2018 <- '2018-05-01 00:00:00'
end_date_2018 <- '2018-09-30 23:00:00'

start_date_2021 <- '2021-05-01 00:00:00'
end_date_2021 <- '2021-09-30 23:00:00'


dates_plot_2018 <- seq(as.POSIXct("2018-05-01 00:00:00", format = '%Y-%m-%d %H:%M:%S'), as.POSIXct("2018-09-30 23:00:00", format = '%Y-%m-%d %H:%M:%S'), "hours")
dates_plot_2021 <- seq(as.POSIXct("2021-05-01 00:00:00", format = '%Y-%m-%d %H:%M:%S'), as.POSIXct("2021-09-30 23:00:00", format = '%Y-%m-%d %H:%M:%S'), "hours")


tiff("Path to save plot/Albedo and SWE plot filename.tiff", width = 6, height = 4, units = "in", res = 500, pointsize = 12)

par(mfrow=c(2,2))
par(mar=c(4,4,1.5,1.5))
par(mgp=c(2,0.8,0))

plot(dates_plot_2018, alb_snow_ctrl[which(dates_full == as.POSIXct(start_date_2018, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date_2018, format = '%Y-%m-%d %H:%M:%S'))], type = 'l', col = 'red', ylim = c(0.55, 0.95), ylab = "Albedo [ ]", main = "AGRB Snow HRU - 2018", xlab = "Time [hours]")
lines(dates_plot_2018, alb_snow_mean[which(dates_full == as.POSIXct(start_date_2018, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date_2018, format = '%Y-%m-%d %H:%M:%S'))], col = 'blue')
legend('bottomleft', legend = c('DA', 'CTRL'), col = c("blue", "red"), lty = c(1,1), lwd = c(1,1), bty = "n")

plot(dates_plot_2021, alb_snow_ctrl[which(dates_full == as.POSIXct(start_date_2021, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date_2021, format = '%Y-%m-%d %H:%M:%S'))], type = 'l', col = 'red', ylim = c(0.55, 0.95), ylab = "Albedo [ ]", main = "AGRB Snow HRU - 2021", xlab = "Time [hours]")
lines(dates_plot_2021, alb_snow_mean[which(dates_full == as.POSIXct(start_date_2021, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date_2021, format = '%Y-%m-%d %H:%M:%S'))], col = 'blue')

plot(dates_plot_2018, alb_ice_ctrl[which(dates_full == as.POSIXct(start_date_2018, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date_2018, format = '%Y-%m-%d %H:%M:%S'))], type = 'l', col = 'red', ylim = c(0.15, 0.95), ylab = "Albedo [ ]", main = "AGRB Ice HRU - 2018", xlab = "Time [hours]")
lines(dates_plot_2018, alb_ice_mean[which(dates_full == as.POSIXct(start_date_2018, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date_2018, format = '%Y-%m-%d %H:%M:%S'))], col = 'blue')

plot(dates_plot_2021, alb_ice_ctrl[which(dates_full == as.POSIXct(start_date_2021, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date_2021, format = '%Y-%m-%d %H:%M:%S'))], type = 'l', col = 'red', ylim = c(0.15, 0.95), ylab = "Albedo [ ]", main = "AGRB Ice HRU - 2021", xlab = "Time [hours]")
lines(dates_plot_2021, alb_ice_mean[which(dates_full == as.POSIXct(start_date_2021, format = '%Y-%m-%d %H:%M:%S')):which(dates_full == as.POSIXct(end_date_2021, format = '%Y-%m-%d %H:%M:%S'))], col = 'blue')

dev.off()
