#Code to fill streamflow simulations during times the model stopped working due to a data assimilation step for Athabasca Basin

library(lubridate)
library(abind)


setwd('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/02_Runs/Run_20')

exclusion_dates <- c(11:13, 27:29, 31:37, 47:50, 67:73, 90:91)


dates_rs <- read.csv('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/04_RS_Albedo/Dates_Athabasca_20230415.csv', header = F)[-c(exclusion_dates),]

ending_time <- c(as.POSIXct(dates_rs, format = '%Y-%m-%d') + hours(24), as.POSIXct('2021-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'))

ending_time_obs <- c(as.POSIXct(dates_rs, format = '%Y-%m-%d') + hours(96), as.POSIXct('2021-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'))

start_time <- c(as.POSIXct('2015-10-01 01:00:00', format = '%Y-%m-%d %H:%M:%S'), (ending_time + hours(1)))


initial_min_alb <- c(0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.3, 0.3, 0.3, 
                     0.3, 0.55, 0.3, 0.3, 0.3, 0.3, 0.3, 0.17, 0.17, 0.55, 
                     0.55, 0.55, 0.3, 0.55, 0.3, 0.55, 0.55, 0.55, 0.55, 0.17, 
                     0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.55, 0.55, 0.55, 
                     0.3, 0.3, 0.55, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 
                     0.55, 0.55, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.17, 0.3, 
                     0.17, 0.17, 0.17, 0.17, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 
                     0.3, 0.3, 0.3, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 
                     0.17, 0.17, 0.17, 0.17, 0.55, 0.17, 0.17, 0.17, 0.17, 0.17)


#time index

for (j in 2:69) {
  
  states_1 <- readLines(paste0('CRHM_states_En1_t', j-1, '.txt'))
  states_2 <- readLines(paste0('CRHM_states_En2_t', j-1, '.txt'))
  states_3 <- readLines(paste0('CRHM_states_En3_t', j-1, '.txt'))
  states_4 <- readLines(paste0('CRHM_states_En4_t', j-1, '.txt'))
  states_5 <- readLines(paste0('CRHM_states_En5_t', j-1, '.txt'))
  states_6 <- readLines(paste0('CRHM_states_En6_t', j-1, '.txt'))
  states_7 <- readLines(paste0('CRHM_states_En7_t', j-1, '.txt'))
  states_8 <- readLines(paste0('CRHM_states_En8_t', j-1, '.txt'))
  states_9 <- readLines(paste0('CRHM_states_En9_t', j-1, '.txt'))
  states_10 <- readLines(paste0('CRHM_states_En10_t', j-1, '.txt'))
  states_11 <- readLines(paste0('CRHM_states_En11_t', j-1, '.txt'))
  states_12 <- readLines(paste0('CRHM_states_En12_t', j-1, '.txt'))
  states_13 <- readLines(paste0('CRHM_states_En13_t', j-1, '.txt'))
  states_14 <- readLines(paste0('CRHM_states_En14_t', j-1, '.txt'))
  states_15 <- readLines(paste0('CRHM_states_En15_t', j-1, '.txt'))
  states_16 <- readLines(paste0('CRHM_states_En16_t', j-1, '.txt'))
  states_17 <- readLines(paste0('CRHM_states_En17_t', j-1, '.txt'))
  states_18 <- readLines(paste0('CRHM_states_En18_t', j-1, '.txt'))
  states_19 <- readLines(paste0('CRHM_states_En19_t', j-1, '.txt'))
  states_20 <- readLines(paste0('CRHM_states_En20_t', j-1, '.txt'))
  
  #ensemble index
  
  for (i in 1:20) {
    
    
    if (i == 1) {writeLines(states_1, 'states_updated.int')}
    if (i == 2) {writeLines(states_2, 'states_updated.int')}
    if (i == 3) {writeLines(states_3, 'states_updated.int')}
    if (i == 4) {writeLines(states_4, 'states_updated.int')}
    if (i == 6) {writeLines(states_6, 'states_updated.int')}
    if (i == 7) {writeLines(states_7, 'states_updated.int')}
    if (i == 8) {writeLines(states_8, 'states_updated.int')}
    if (i == 9) {writeLines(states_9, 'states_updated.int')}
    if (i == 10) {writeLines(states_10, 'states_updated.int')}
    if (i == 11) {writeLines(states_11, 'states_updated.int')}
    if (i == 12) {writeLines(states_12, 'states_updated.int')}
    if (i == 13) {writeLines(states_13, 'states_updated.int')}
    if (i == 14) {writeLines(states_14, 'states_updated.int')}
    if (i == 15) {writeLines(states_15, 'states_updated.int')}
    if (i == 16) {writeLines(states_16, 'states_updated.int')}
    if (i == 17) {writeLines(states_17, 'states_updated.int')}
    if (i == 18) {writeLines(states_18, 'states_updated.int')}
    if (i == 19) {writeLines(states_19, 'states_updated.int')}
    if (i == 20) {writeLines(states_20, 'states_updated.int')}
    
    
    
    obs <- read.table(paste0('en_', i, '.obs'), sep = '', dec = '.')
    
    obs_dates <- as.POSIXct(paste0(obs$V1, '-', obs$V2, '-', obs$V3, ' ', obs$V4, ':', obs$V5, ':00'), format = '%Y-%m-%d %H:%M:%S')
    
    obs_clipped <- obs[which(start_time[j] == obs_dates):which(ending_time_obs[j] == obs_dates),]
    
    write.table(obs_clipped, file = paste0('en_', i, '_c.obs'), sep = "\t", row.names = F, col.names = F)
    
    obs_header <- readLines('obs_header.txt')
    
    obs_lines <- readLines(paste0('en_', i, '_c.obs'))
    
    obs_combined <- c(obs_header, obs_lines)
    
    writeLines(obs_combined, paste0('en_', i, '_i.obs'))
    
    system(paste0("CRHM.exe Athabasca_DA.prj en_", i, "_i.obs Parameters", i, ".par"))
    
    
    crhm_output <- read.table('CRHM_output_2022204.txt')
    
    
    write.csv(crhm_output, paste0('CRHM_output_En', i, '_t', j, '.csv'))
    
    plot(crhm_output$V2[-c(1,2)], type = 'l')
    
    
  }
  
  
  
  Albedo_cur <- matrix(nrow = 20, ncol = 90)
  
  Albedo_cur[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'albedo_Richard Albedo')+1):(which(states_1 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'albedo_Richard Albedo')+1):(which(states_2 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'albedo_Richard Albedo')+1):(which(states_3 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'albedo_Richard Albedo')+1):(which(states_4 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'albedo_Richard Albedo')+1):(which(states_5 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'albedo_Richard Albedo')+1):(which(states_6 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'albedo_Richard Albedo')+1):(which(states_7 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'albedo_Richard Albedo')+1):(which(states_8 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'albedo_Richard Albedo')+1):(which(states_9 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'albedo_Richard Albedo')+1):(which(states_10 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'albedo_Richard Albedo')+1):(which(states_11 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'albedo_Richard Albedo')+1):(which(states_12 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'albedo_Richard Albedo')+1):(which(states_13 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'albedo_Richard Albedo')+1):(which(states_14 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'albedo_Richard Albedo')+1):(which(states_15 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'albedo_Richard Albedo')+1):(which(states_16 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'albedo_Richard Albedo')+1):(which(states_17 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'albedo_Richard Albedo')+1):(which(states_18 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'albedo_Richard Albedo')+1):(which(states_19 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo_cur[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'albedo_Richard Albedo')+1):(which(states_20 == 'albedo_Richard Albedo')+9)]), ' ')))
  
  
  
  #Retrieve previous albedos
  
  Albedo_p <- array(data = NA, dim = c(20,90,68))
  
  
  if (j > 1) {
    
    for (t in 1:(j-1)) {
      
      states_p_1 <- readLines(paste0('CRHM_states_En1_t', t, '.txt'))
      states_p_2 <- readLines(paste0('CRHM_states_En2_t', t, '.txt'))
      states_p_3 <- readLines(paste0('CRHM_states_En3_t', t, '.txt'))
      states_p_4 <- readLines(paste0('CRHM_states_En4_t', t, '.txt'))
      states_p_5 <- readLines(paste0('CRHM_states_En5_t', t, '.txt'))
      states_p_6 <- readLines(paste0('CRHM_states_En6_t', t, '.txt'))
      states_p_7 <- readLines(paste0('CRHM_states_En7_t', t, '.txt'))
      states_p_8 <- readLines(paste0('CRHM_states_En8_t', t, '.txt'))
      states_p_9 <- readLines(paste0('CRHM_states_En9_t', t, '.txt'))
      states_p_10 <- readLines(paste0('CRHM_states_En10_t', t, '.txt'))
      states_p_11 <- readLines(paste0('CRHM_states_En11_t', t, '.txt'))
      states_p_12 <- readLines(paste0('CRHM_states_En12_t', t, '.txt'))
      states_p_13 <- readLines(paste0('CRHM_states_En13_t', t, '.txt'))
      states_p_14 <- readLines(paste0('CRHM_states_En14_t', t, '.txt'))
      states_p_15 <- readLines(paste0('CRHM_states_En15_t', t, '.txt'))
      states_p_16 <- readLines(paste0('CRHM_states_En16_t', t, '.txt'))
      states_p_17 <- readLines(paste0('CRHM_states_En17_t', t, '.txt'))
      states_p_18 <- readLines(paste0('CRHM_states_En18_t', t, '.txt'))
      states_p_19 <- readLines(paste0('CRHM_states_En19_t', t, '.txt'))
      states_p_20 <- readLines(paste0('CRHM_states_En20_t', t, '.txt'))
      
      
      Albedo_p[1,,t] <- as.numeric(unlist(strsplit(c(states_p_1[(which(states_p_1 == 'albedo_Richard Albedo')+1):(which(states_p_1 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[2,,t] <- as.numeric(unlist(strsplit(c(states_p_2[(which(states_p_2 == 'albedo_Richard Albedo')+1):(which(states_p_2 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[3,,t] <- as.numeric(unlist(strsplit(c(states_p_3[(which(states_p_3 == 'albedo_Richard Albedo')+1):(which(states_p_3 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[4,,t] <- as.numeric(unlist(strsplit(c(states_p_4[(which(states_p_4 == 'albedo_Richard Albedo')+1):(which(states_p_4 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[5,,t] <- as.numeric(unlist(strsplit(c(states_p_5[(which(states_p_5 == 'albedo_Richard Albedo')+1):(which(states_p_5 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[6,,t] <- as.numeric(unlist(strsplit(c(states_p_6[(which(states_p_6 == 'albedo_Richard Albedo')+1):(which(states_p_6 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[7,,t] <- as.numeric(unlist(strsplit(c(states_p_7[(which(states_p_7 == 'albedo_Richard Albedo')+1):(which(states_p_7 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[8,,t] <- as.numeric(unlist(strsplit(c(states_p_8[(which(states_p_8 == 'albedo_Richard Albedo')+1):(which(states_p_8 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[9,,t] <- as.numeric(unlist(strsplit(c(states_p_9[(which(states_p_9 == 'albedo_Richard Albedo')+1):(which(states_p_9 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[10,,t] <- as.numeric(unlist(strsplit(c(states_p_10[(which(states_p_10 == 'albedo_Richard Albedo')+1):(which(states_p_10 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[11,,t] <- as.numeric(unlist(strsplit(c(states_p_11[(which(states_p_11 == 'albedo_Richard Albedo')+1):(which(states_p_11 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[12,,t] <- as.numeric(unlist(strsplit(c(states_p_12[(which(states_p_12 == 'albedo_Richard Albedo')+1):(which(states_p_12 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[13,,t] <- as.numeric(unlist(strsplit(c(states_p_13[(which(states_p_13 == 'albedo_Richard Albedo')+1):(which(states_p_13 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[14,,t] <- as.numeric(unlist(strsplit(c(states_p_14[(which(states_p_14 == 'albedo_Richard Albedo')+1):(which(states_p_14 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[15,,t] <- as.numeric(unlist(strsplit(c(states_p_15[(which(states_p_15 == 'albedo_Richard Albedo')+1):(which(states_p_15 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[16,,t] <- as.numeric(unlist(strsplit(c(states_p_16[(which(states_p_16 == 'albedo_Richard Albedo')+1):(which(states_p_16 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[17,,t] <- as.numeric(unlist(strsplit(c(states_p_17[(which(states_p_17 == 'albedo_Richard Albedo')+1):(which(states_p_17 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[18,,t] <- as.numeric(unlist(strsplit(c(states_p_18[(which(states_p_18 == 'albedo_Richard Albedo')+1):(which(states_p_18 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[19,,t] <- as.numeric(unlist(strsplit(c(states_p_19[(which(states_p_19 == 'albedo_Richard Albedo')+1):(which(states_p_19 == 'albedo_Richard Albedo')+9)]), ' ')))
      Albedo_p[20,,t] <- as.numeric(unlist(strsplit(c(states_p_20[(which(states_p_20 == 'albedo_Richard Albedo')+1):(which(states_p_20 == 'albedo_Richard Albedo')+9)]), ' ')))
      
      
    }
    
    
    albedo_min_vec <- vector()
    
    for (u in which(initial_min_alb == 0.3)) {
      
      albedo_min_vec[u] <- Albedo_p[1,u,rev(tail(which(Albedo_p[1,u,] < 0.3)))[1]]
      
    }
    
    for (u in which(initial_min_alb == 0.55)) {
      
      albedo_min_vec[u] <- Albedo_p[1,u,rev(tail(which(Albedo_p[1,u,] < 0.55)))[1]]
      
    }
    
    
  } else {
    
    albedo_min_vec <- initial_min_alb
    
  }
  
  
  #Change minimum albedo parameter
  
  for (s in 1:20) {
    
    initial_min_alb_final <- ifelse(initial_min_alb == 0.17, 0.17, Albedo_cur[s,])
    
    initial_min_alb_final[which(initial_min_alb == 0.3)] <- ifelse(initial_min_alb_final[which(initial_min_alb == 0.3)] < 0.3, initial_min_alb_final[which(initial_min_alb == 0.3)], albedo_min_vec[which(initial_min_alb == 0.3)])
    
    initial_min_alb_final[which(initial_min_alb == 0.55)] <- ifelse(initial_min_alb_final[which(initial_min_alb == 0.55)] < 0.55, initial_min_alb_final[which(initial_min_alb == 0.55)], albedo_min_vec[which(initial_min_alb == 0.55)])
    
    initial_min_alb_final <- ifelse(is.na(initial_min_alb_final), initial_min_alb, initial_min_alb_final)
    
    
    parameters <- readLines(paste0('Parameters', s, '.par'))
    
    parameters[24] <- paste(as.character(initial_min_alb_final[1:10]), collapse = ' ')
    parameters[25] <- paste(as.character(initial_min_alb_final[11:20]), collapse = ' ')
    parameters[26] <- paste(as.character(initial_min_alb_final[21:30]), collapse = ' ')
    parameters[27] <- paste(as.character(initial_min_alb_final[31:40]), collapse = ' ')
    parameters[28] <- paste(as.character(initial_min_alb_final[41:50]), collapse = ' ')
    parameters[29] <- paste(as.character(initial_min_alb_final[51:60]), collapse = ' ')
    parameters[30] <- paste(as.character(initial_min_alb_final[61:70]), collapse = ' ')
    parameters[31] <- paste(as.character(initial_min_alb_final[71:80]), collapse = ' ')
    parameters[32] <- paste(as.character(initial_min_alb_final[81:90]), collapse = ' ')
    
    
    parameters[522] <- paste(as.character(initial_min_alb_final[1:10]), collapse = ' ')
    parameters[523] <- paste(as.character(initial_min_alb_final[11:20]), collapse = ' ')
    parameters[524] <- paste(as.character(initial_min_alb_final[21:30]), collapse = ' ')
    parameters[525] <- paste(as.character(initial_min_alb_final[31:40]), collapse = ' ')
    parameters[526] <- paste(as.character(initial_min_alb_final[41:50]), collapse = ' ')
    parameters[527] <- paste(as.character(initial_min_alb_final[51:60]), collapse = ' ')
    parameters[528] <- paste(as.character(initial_min_alb_final[61:70]), collapse = ' ')
    parameters[529] <- paste(as.character(initial_min_alb_final[71:80]), collapse = ' ')
    parameters[530] <- paste(as.character(initial_min_alb_final[81:90]), collapse = ' ')
    
    
    parameters[754] <- paste(as.character(initial_min_alb_final[1:10]), collapse = ' ')
    parameters[755] <- paste(as.character(initial_min_alb_final[11:20]), collapse = ' ')
    parameters[756] <- paste(as.character(initial_min_alb_final[21:30]), collapse = ' ')
    parameters[757] <- paste(as.character(initial_min_alb_final[31:40]), collapse = ' ')
    parameters[758] <- paste(as.character(initial_min_alb_final[41:50]), collapse = ' ')
    parameters[759] <- paste(as.character(initial_min_alb_final[51:60]), collapse = ' ')
    parameters[760] <- paste(as.character(initial_min_alb_final[61:70]), collapse = ' ')
    parameters[761] <- paste(as.character(initial_min_alb_final[71:80]), collapse = ' ')
    parameters[762] <- paste(as.character(initial_min_alb_final[81:90]), collapse = ' ')
    
    
    writeLines(parameters, paste0('Parameters', s, '.par'))
    
  }
  
  
}


