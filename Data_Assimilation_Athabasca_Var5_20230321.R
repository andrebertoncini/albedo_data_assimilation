#Code to perform albedo data assimilation with the CRHM model for Athabasca Basin

library(lubridate)
library(abind)


setwd('Path to model DA runs')

exclusion_dates <- c(11:13, 27:29, 31:37, 47:50, 67:73, 90:91) #dates outside the May to September period


dates_rs <- read.csv('Path to remote sensing dates/Dates_Athabasca.csv', header = F)[-c(exclusion_dates),]

ending_time <- c(as.POSIXct(dates_rs, format = '%Y-%m-%d') + hours(24), as.POSIXct('2021-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'))

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

j = 1

for (j in 2:69) {
  
  
  #ensemble index
  
  for (i in 1:20) {
    
    
    if (j > 1) {
      
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
      
    }
    
    
    obs <- read.table(paste0('en_', i, '.obs'), sep = '', dec = '.')
    
    obs_dates <- as.POSIXct(paste0(obs$V1, '-', obs$V2, '-', obs$V3, ' ', obs$V4, ':', obs$V5, ':00'), format = '%Y-%m-%d %H:%M:%S')
    
    obs_clipped <- obs[which(start_time[j] == obs_dates):which(ending_time[j] == obs_dates),]
    
    write.table(obs_clipped, file = paste0('en_', i, '_c.obs'), sep = "\t", row.names = F, col.names = F)
    
    obs_header <- readLines('obs_header.txt')
    
    obs_lines <- readLines(paste0('en_', i, '_c.obs'))
    
    obs_combined <- c(obs_header, obs_lines)
    
    writeLines(obs_combined, paste0('en_', i, '_i.obs'))
    
    system(paste0("CRHM.exe Athabasca_DA.prj en_", i, "_i.obs Parameters", i, ".par"))
    
    
    crhm_output <- read.table('CRHM_output_2022204.txt')
    
    states <- readLines('States_File.int')
    
    
    write.csv(crhm_output, paste0('CRHM_output_En', i, '_t', j, '.csv'))
    
    writeLines(states, paste0('CRHM_states_En', i, '_t', j, '.txt'))
    
    plot(crhm_output$V2[-c(1,2)], type = 'l')
    
    
  }
  
  
  ###############################################################
  #Only run this when all the ensembles of a time step are done!#
  ###############################################################
  
  
  states_1 <- readLines(paste0('CRHM_states_En1_t', j, '.txt'))
  states_2 <- readLines(paste0('CRHM_states_En2_t', j, '.txt'))
  states_3 <- readLines(paste0('CRHM_states_En3_t', j, '.txt'))
  states_4 <- readLines(paste0('CRHM_states_En4_t', j, '.txt'))
  states_5 <- readLines(paste0('CRHM_states_En5_t', j, '.txt'))
  states_6 <- readLines(paste0('CRHM_states_En6_t', j, '.txt'))
  states_7 <- readLines(paste0('CRHM_states_En7_t', j, '.txt'))
  states_8 <- readLines(paste0('CRHM_states_En8_t', j, '.txt'))
  states_9 <- readLines(paste0('CRHM_states_En9_t', j, '.txt'))
  states_10 <- readLines(paste0('CRHM_states_En10_t', j, '.txt'))
  states_11 <- readLines(paste0('CRHM_states_En11_t', j, '.txt'))
  states_12 <- readLines(paste0('CRHM_states_En12_t', j, '.txt'))
  states_13 <- readLines(paste0('CRHM_states_En13_t', j, '.txt'))
  states_14 <- readLines(paste0('CRHM_states_En14_t', j, '.txt'))
  states_15 <- readLines(paste0('CRHM_states_En15_t', j, '.txt'))
  states_16 <- readLines(paste0('CRHM_states_En16_t', j, '.txt'))
  states_17 <- readLines(paste0('CRHM_states_En17_t', j, '.txt'))
  states_18 <- readLines(paste0('CRHM_states_En18_t', j, '.txt'))
  states_19 <- readLines(paste0('CRHM_states_En19_t', j, '.txt'))
  states_20 <- readLines(paste0('CRHM_states_En20_t', j, '.txt'))
  
  
  #Input states matrices
  
  SWE <- matrix(nrow = 20, ncol = 90)
  T_s <- matrix(nrow = 20, ncol = 90)
  T_s_0 <- matrix(nrow = 20, ncol = 90)
  cc_s <- matrix(nrow = 20, ncol = 90)
  cc_s_0 <- matrix(nrow = 20, ncol = 90)
  cc_s_l <- matrix(nrow = 20, ncol = 90)
  h2o <- matrix(nrow = 20, ncol = 90)
  h2o_sat <- matrix(nrow = 20, ncol = 90)
  layer_count <- matrix(nrow = 20, ncol = 90)
  m_s_0 <- matrix(nrow = 20, ncol = 90)
  m_s_l <- matrix(nrow = 20, ncol = 90)
  rho <- matrix(nrow = 20, ncol = 90)
  snowcover <- matrix(nrow = 20, ncol = 90)
  z_s <- matrix(nrow = 20, ncol = 90)
  z_s_0 <- matrix(nrow = 20, ncol = 90)
  z_s_l <- matrix(nrow = 20, ncol = 90)
  Albedo <- matrix(nrow = 20, ncol = 90)
  firn <- matrix(nrow = 20, ncol = 90)
  firn_depth <- matrix(nrow = 20, ncol = 90)
  firn_h <- matrix(nrow = 20, ncol = 900)
  firn_dens <- matrix(nrow = 20, ncol = 900)
  ice <- matrix(nrow = 20, ncol = 90)
  nfirn <- matrix(nrow = 20, ncol = 90)
  
  
  #Create vector of states
  
  SWE[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM SWE')+1):(which(states_1 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM SWE')+1):(which(states_2 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM SWE')+1):(which(states_3 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM SWE')+1):(which(states_4 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM SWE')+1):(which(states_5 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM SWE')+1):(which(states_6 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM SWE')+1):(which(states_7 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM SWE')+1):(which(states_8 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM SWE')+1):(which(states_9 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM SWE')+1):(which(states_10 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM SWE')+1):(which(states_11 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM SWE')+1):(which(states_12 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM SWE')+1):(which(states_13 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM SWE')+1):(which(states_14 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM SWE')+1):(which(states_15 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM SWE')+1):(which(states_16 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM SWE')+1):(which(states_17 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM SWE')+1):(which(states_18 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM SWE')+1):(which(states_19 == 'SnobalCRHM SWE')+9)]), ' ')))
  SWE[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM SWE')+1):(which(states_20 == 'SnobalCRHM SWE')+9)]), ' ')))
  
  
  T_s[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM T_s')+1):(which(states_1 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM T_s')+1):(which(states_2 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM T_s')+1):(which(states_3 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM T_s')+1):(which(states_4 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM T_s')+1):(which(states_5 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM T_s')+1):(which(states_6 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM T_s')+1):(which(states_7 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM T_s')+1):(which(states_8 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM T_s')+1):(which(states_9 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM T_s')+1):(which(states_10 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM T_s')+1):(which(states_11 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM T_s')+1):(which(states_12 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM T_s')+1):(which(states_13 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM T_s')+1):(which(states_14 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM T_s')+1):(which(states_15 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM T_s')+1):(which(states_16 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM T_s')+1):(which(states_17 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM T_s')+1):(which(states_18 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM T_s')+1):(which(states_19 == 'SnobalCRHM T_s')+9)]), ' ')))
  T_s[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM T_s')+1):(which(states_20 == 'SnobalCRHM T_s')+9)]), ' ')))
  
  
  T_s_0[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM T_s_0')+1):(which(states_1 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM T_s_0')+1):(which(states_2 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM T_s_0')+1):(which(states_3 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM T_s_0')+1):(which(states_4 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM T_s_0')+1):(which(states_5 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM T_s_0')+1):(which(states_6 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM T_s_0')+1):(which(states_7 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM T_s_0')+1):(which(states_8 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM T_s_0')+1):(which(states_9 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM T_s_0')+1):(which(states_10 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM T_s_0')+1):(which(states_11 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM T_s_0')+1):(which(states_12 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM T_s_0')+1):(which(states_13 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM T_s_0')+1):(which(states_14 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM T_s_0')+1):(which(states_15 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM T_s_0')+1):(which(states_16 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM T_s_0')+1):(which(states_17 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM T_s_0')+1):(which(states_18 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM T_s_0')+1):(which(states_19 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  T_s_0[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM T_s_0')+1):(which(states_20 == 'SnobalCRHM T_s_0')+9)]), ' ')))
  
  
  cc_s[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM cc_s')+1):(which(states_1 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM cc_s')+1):(which(states_2 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM cc_s')+1):(which(states_3 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM cc_s')+1):(which(states_4 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM cc_s')+1):(which(states_5 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM cc_s')+1):(which(states_6 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM cc_s')+1):(which(states_7 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM cc_s')+1):(which(states_8 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM cc_s')+1):(which(states_9 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM cc_s')+1):(which(states_10 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM cc_s')+1):(which(states_11 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM cc_s')+1):(which(states_12 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM cc_s')+1):(which(states_13 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM cc_s')+1):(which(states_14 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM cc_s')+1):(which(states_15 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM cc_s')+1):(which(states_16 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM cc_s')+1):(which(states_17 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM cc_s')+1):(which(states_18 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM cc_s')+1):(which(states_19 == 'SnobalCRHM cc_s')+9)]), ' ')))
  cc_s[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM cc_s')+1):(which(states_20 == 'SnobalCRHM cc_s')+9)]), ' ')))
  
  
  cc_s_0[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM cc_s_0')+1):(which(states_1 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM cc_s_0')+1):(which(states_2 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM cc_s_0')+1):(which(states_3 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM cc_s_0')+1):(which(states_4 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM cc_s_0')+1):(which(states_5 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM cc_s_0')+1):(which(states_6 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM cc_s_0')+1):(which(states_7 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM cc_s_0')+1):(which(states_8 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM cc_s_0')+1):(which(states_9 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM cc_s_0')+1):(which(states_10 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM cc_s_0')+1):(which(states_11 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM cc_s_0')+1):(which(states_12 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM cc_s_0')+1):(which(states_13 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM cc_s_0')+1):(which(states_14 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM cc_s_0')+1):(which(states_15 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM cc_s_0')+1):(which(states_16 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM cc_s_0')+1):(which(states_17 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM cc_s_0')+1):(which(states_18 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM cc_s_0')+1):(which(states_19 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  cc_s_0[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM cc_s_0')+1):(which(states_20 == 'SnobalCRHM cc_s_0')+9)]), ' ')))
  
  
  cc_s_l[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM cc_s_l')+1):(which(states_1 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM cc_s_l')+1):(which(states_2 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM cc_s_l')+1):(which(states_3 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM cc_s_l')+1):(which(states_4 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM cc_s_l')+1):(which(states_5 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM cc_s_l')+1):(which(states_6 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM cc_s_l')+1):(which(states_7 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM cc_s_l')+1):(which(states_8 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM cc_s_l')+1):(which(states_9 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM cc_s_l')+1):(which(states_10 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM cc_s_l')+1):(which(states_11 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM cc_s_l')+1):(which(states_12 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM cc_s_l')+1):(which(states_13 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM cc_s_l')+1):(which(states_14 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM cc_s_l')+1):(which(states_15 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM cc_s_l')+1):(which(states_16 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM cc_s_l')+1):(which(states_17 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM cc_s_l')+1):(which(states_18 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM cc_s_l')+1):(which(states_19 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  cc_s_l[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM cc_s_l')+1):(which(states_20 == 'SnobalCRHM cc_s_l')+9)]), ' ')))
  
  
  h2o[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM h2o')+1):(which(states_1 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM h2o')+1):(which(states_2 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM h2o')+1):(which(states_3 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM h2o')+1):(which(states_4 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM h2o')+1):(which(states_5 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM h2o')+1):(which(states_6 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM h2o')+1):(which(states_7 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM h2o')+1):(which(states_8 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM h2o')+1):(which(states_9 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM h2o')+1):(which(states_10 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM h2o')+1):(which(states_11 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM h2o')+1):(which(states_12 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM h2o')+1):(which(states_13 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM h2o')+1):(which(states_14 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM h2o')+1):(which(states_15 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM h2o')+1):(which(states_16 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM h2o')+1):(which(states_17 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM h2o')+1):(which(states_18 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM h2o')+1):(which(states_19 == 'SnobalCRHM h2o')+9)]), ' ')))
  h2o[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM h2o')+1):(which(states_20 == 'SnobalCRHM h2o')+9)]), ' ')))
  
  
  h2o_sat[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM h2o_sat')+1):(which(states_1 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM h2o_sat')+1):(which(states_2 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM h2o_sat')+1):(which(states_3 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM h2o_sat')+1):(which(states_4 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM h2o_sat')+1):(which(states_5 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM h2o_sat')+1):(which(states_6 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM h2o_sat')+1):(which(states_7 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM h2o_sat')+1):(which(states_8 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM h2o_sat')+1):(which(states_9 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM h2o_sat')+1):(which(states_10 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM h2o_sat')+1):(which(states_11 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM h2o_sat')+1):(which(states_12 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM h2o_sat')+1):(which(states_13 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM h2o_sat')+1):(which(states_14 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM h2o_sat')+1):(which(states_15 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM h2o_sat')+1):(which(states_16 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM h2o_sat')+1):(which(states_17 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM h2o_sat')+1):(which(states_18 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM h2o_sat')+1):(which(states_19 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  h2o_sat[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM h2o_sat')+1):(which(states_20 == 'SnobalCRHM h2o_sat')+9)]), ' ')))
  
  
  layer_count[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM layer_count')+1):(which(states_1 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM layer_count')+1):(which(states_2 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM layer_count')+1):(which(states_3 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM layer_count')+1):(which(states_4 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM layer_count')+1):(which(states_5 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM layer_count')+1):(which(states_6 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM layer_count')+1):(which(states_7 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM layer_count')+1):(which(states_8 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM layer_count')+1):(which(states_9 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM layer_count')+1):(which(states_10 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM layer_count')+1):(which(states_11 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM layer_count')+1):(which(states_12 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM layer_count')+1):(which(states_13 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM layer_count')+1):(which(states_14 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM layer_count')+1):(which(states_15 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM layer_count')+1):(which(states_16 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM layer_count')+1):(which(states_17 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM layer_count')+1):(which(states_18 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM layer_count')+1):(which(states_19 == 'SnobalCRHM layer_count')+9)]), ' ')))
  layer_count[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM layer_count')+1):(which(states_20 == 'SnobalCRHM layer_count')+9)]), ' ')))
  
  
  m_s_0[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM m_s_0')+1):(which(states_1 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM m_s_0')+1):(which(states_2 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM m_s_0')+1):(which(states_3 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM m_s_0')+1):(which(states_4 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM m_s_0')+1):(which(states_5 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM m_s_0')+1):(which(states_6 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM m_s_0')+1):(which(states_7 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM m_s_0')+1):(which(states_8 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM m_s_0')+1):(which(states_9 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM m_s_0')+1):(which(states_10 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM m_s_0')+1):(which(states_11 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM m_s_0')+1):(which(states_12 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM m_s_0')+1):(which(states_13 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM m_s_0')+1):(which(states_14 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM m_s_0')+1):(which(states_15 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM m_s_0')+1):(which(states_16 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM m_s_0')+1):(which(states_17 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM m_s_0')+1):(which(states_18 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM m_s_0')+1):(which(states_19 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  m_s_0[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM m_s_0')+1):(which(states_20 == 'SnobalCRHM m_s_0')+9)]), ' ')))
  
  
  m_s_l[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM m_s_l')+1):(which(states_1 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM m_s_l')+1):(which(states_2 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM m_s_l')+1):(which(states_3 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM m_s_l')+1):(which(states_4 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM m_s_l')+1):(which(states_5 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM m_s_l')+1):(which(states_6 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM m_s_l')+1):(which(states_7 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM m_s_l')+1):(which(states_8 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM m_s_l')+1):(which(states_9 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM m_s_l')+1):(which(states_10 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM m_s_l')+1):(which(states_11 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM m_s_l')+1):(which(states_12 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM m_s_l')+1):(which(states_13 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM m_s_l')+1):(which(states_14 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM m_s_l')+1):(which(states_15 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM m_s_l')+1):(which(states_16 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM m_s_l')+1):(which(states_17 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM m_s_l')+1):(which(states_18 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM m_s_l')+1):(which(states_19 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  m_s_l[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM m_s_l')+1):(which(states_20 == 'SnobalCRHM m_s_l')+9)]), ' ')))
  
  
  rho[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM rho')+1):(which(states_1 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM rho')+1):(which(states_2 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM rho')+1):(which(states_3 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM rho')+1):(which(states_4 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM rho')+1):(which(states_5 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM rho')+1):(which(states_6 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM rho')+1):(which(states_7 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM rho')+1):(which(states_8 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM rho')+1):(which(states_9 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM rho')+1):(which(states_10 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM rho')+1):(which(states_11 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM rho')+1):(which(states_12 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM rho')+1):(which(states_13 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM rho')+1):(which(states_14 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM rho')+1):(which(states_15 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM rho')+1):(which(states_16 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM rho')+1):(which(states_17 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM rho')+1):(which(states_18 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM rho')+1):(which(states_19 == 'SnobalCRHM rho')+9)]), ' ')))
  rho[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM rho')+1):(which(states_20 == 'SnobalCRHM rho')+9)]), ' ')))
  
  
  snowcover[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM snowcover')+1):(which(states_1 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM snowcover')+1):(which(states_2 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM snowcover')+1):(which(states_3 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM snowcover')+1):(which(states_4 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM snowcover')+1):(which(states_5 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM snowcover')+1):(which(states_6 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM snowcover')+1):(which(states_7 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM snowcover')+1):(which(states_8 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM snowcover')+1):(which(states_9 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM snowcover')+1):(which(states_10 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM snowcover')+1):(which(states_11 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM snowcover')+1):(which(states_12 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM snowcover')+1):(which(states_13 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM snowcover')+1):(which(states_14 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM snowcover')+1):(which(states_15 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM snowcover')+1):(which(states_16 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM snowcover')+1):(which(states_17 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM snowcover')+1):(which(states_18 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM snowcover')+1):(which(states_19 == 'SnobalCRHM snowcover')+9)]), ' ')))
  snowcover[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM snowcover')+1):(which(states_20 == 'SnobalCRHM snowcover')+9)]), ' ')))
  
  
  z_s[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM z_s')+1):(which(states_1 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM z_s')+1):(which(states_2 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM z_s')+1):(which(states_3 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM z_s')+1):(which(states_4 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM z_s')+1):(which(states_5 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM z_s')+1):(which(states_6 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM z_s')+1):(which(states_7 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM z_s')+1):(which(states_8 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM z_s')+1):(which(states_9 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM z_s')+1):(which(states_10 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM z_s')+1):(which(states_11 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM z_s')+1):(which(states_12 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM z_s')+1):(which(states_13 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM z_s')+1):(which(states_14 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM z_s')+1):(which(states_15 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM z_s')+1):(which(states_16 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM z_s')+1):(which(states_17 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM z_s')+1):(which(states_18 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM z_s')+1):(which(states_19 == 'SnobalCRHM z_s')+9)]), ' ')))
  z_s[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM z_s')+1):(which(states_20 == 'SnobalCRHM z_s')+9)]), ' ')))
  
  
  z_s_0[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM z_s_0')+1):(which(states_1 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM z_s_0')+1):(which(states_2 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM z_s_0')+1):(which(states_3 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM z_s_0')+1):(which(states_4 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM z_s_0')+1):(which(states_5 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM z_s_0')+1):(which(states_6 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM z_s_0')+1):(which(states_7 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM z_s_0')+1):(which(states_8 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM z_s_0')+1):(which(states_9 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM z_s_0')+1):(which(states_10 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM z_s_0')+1):(which(states_11 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM z_s_0')+1):(which(states_12 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM z_s_0')+1):(which(states_13 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM z_s_0')+1):(which(states_14 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM z_s_0')+1):(which(states_15 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM z_s_0')+1):(which(states_16 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM z_s_0')+1):(which(states_17 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM z_s_0')+1):(which(states_18 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM z_s_0')+1):(which(states_19 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  z_s_0[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM z_s_0')+1):(which(states_20 == 'SnobalCRHM z_s_0')+9)]), ' ')))
  
  
  z_s_l[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM z_s_l')+1):(which(states_1 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM z_s_l')+1):(which(states_2 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM z_s_l')+1):(which(states_3 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM z_s_l')+1):(which(states_4 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM z_s_l')+1):(which(states_5 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM z_s_l')+1):(which(states_6 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM z_s_l')+1):(which(states_7 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM z_s_l')+1):(which(states_8 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM z_s_l')+1):(which(states_9 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM z_s_l')+1):(which(states_10 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM z_s_l')+1):(which(states_11 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM z_s_l')+1):(which(states_12 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM z_s_l')+1):(which(states_13 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM z_s_l')+1):(which(states_14 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM z_s_l')+1):(which(states_15 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM z_s_l')+1):(which(states_16 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM z_s_l')+1):(which(states_17 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM z_s_l')+1):(which(states_18 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM z_s_l')+1):(which(states_19 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  z_s_l[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM z_s_l')+1):(which(states_20 == 'SnobalCRHM z_s_l')+9)]), ' ')))
  
  
  Albedo[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'albedo_Richard Albedo')+1):(which(states_1 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'albedo_Richard Albedo')+1):(which(states_2 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'albedo_Richard Albedo')+1):(which(states_3 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'albedo_Richard Albedo')+1):(which(states_4 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'albedo_Richard Albedo')+1):(which(states_5 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'albedo_Richard Albedo')+1):(which(states_6 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'albedo_Richard Albedo')+1):(which(states_7 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'albedo_Richard Albedo')+1):(which(states_8 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'albedo_Richard Albedo')+1):(which(states_9 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'albedo_Richard Albedo')+1):(which(states_10 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'albedo_Richard Albedo')+1):(which(states_11 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'albedo_Richard Albedo')+1):(which(states_12 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'albedo_Richard Albedo')+1):(which(states_13 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'albedo_Richard Albedo')+1):(which(states_14 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'albedo_Richard Albedo')+1):(which(states_15 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'albedo_Richard Albedo')+1):(which(states_16 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'albedo_Richard Albedo')+1):(which(states_17 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'albedo_Richard Albedo')+1):(which(states_18 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'albedo_Richard Albedo')+1):(which(states_19 == 'albedo_Richard Albedo')+9)]), ' ')))
  Albedo[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'albedo_Richard Albedo')+1):(which(states_20 == 'albedo_Richard Albedo')+9)]), ' ')))
  
  
  firn[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier firn')+1):(which(states_1 == 'glacier firn')+9)]), ' ')))
  firn[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier firn')+1):(which(states_2 == 'glacier firn')+9)]), ' ')))
  firn[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier firn')+1):(which(states_3 == 'glacier firn')+9)]), ' ')))
  firn[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier firn')+1):(which(states_4 == 'glacier firn')+9)]), ' ')))
  firn[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier firn')+1):(which(states_5 == 'glacier firn')+9)]), ' ')))
  firn[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier firn')+1):(which(states_6 == 'glacier firn')+9)]), ' ')))
  firn[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier firn')+1):(which(states_7 == 'glacier firn')+9)]), ' ')))
  firn[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier firn')+1):(which(states_8 == 'glacier firn')+9)]), ' ')))
  firn[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier firn')+1):(which(states_9 == 'glacier firn')+9)]), ' ')))
  firn[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier firn')+1):(which(states_10 == 'glacier firn')+9)]), ' ')))
  firn[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier firn')+1):(which(states_11 == 'glacier firn')+9)]), ' ')))
  firn[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier firn')+1):(which(states_12 == 'glacier firn')+9)]), ' ')))
  firn[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier firn')+1):(which(states_13 == 'glacier firn')+9)]), ' ')))
  firn[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier firn')+1):(which(states_14 == 'glacier firn')+9)]), ' ')))
  firn[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier firn')+1):(which(states_15 == 'glacier firn')+9)]), ' ')))
  firn[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier firn')+1):(which(states_16 == 'glacier firn')+9)]), ' ')))
  firn[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier firn')+1):(which(states_17 == 'glacier firn')+9)]), ' ')))
  firn[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier firn')+1):(which(states_18 == 'glacier firn')+9)]), ' ')))
  firn[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier firn')+1):(which(states_19 == 'glacier firn')+9)]), ' ')))
  firn[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier firn')+1):(which(states_20 == 'glacier firn')+9)]), ' ')))
  
  
  firn_depth[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier firn_depth')+1):(which(states_1 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier firn_depth')+1):(which(states_2 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier firn_depth')+1):(which(states_3 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier firn_depth')+1):(which(states_4 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier firn_depth')+1):(which(states_5 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier firn_depth')+1):(which(states_6 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier firn_depth')+1):(which(states_7 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier firn_depth')+1):(which(states_8 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier firn_depth')+1):(which(states_9 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier firn_depth')+1):(which(states_10 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier firn_depth')+1):(which(states_11 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier firn_depth')+1):(which(states_12 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier firn_depth')+1):(which(states_13 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier firn_depth')+1):(which(states_14 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier firn_depth')+1):(which(states_15 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier firn_depth')+1):(which(states_16 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier firn_depth')+1):(which(states_17 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier firn_depth')+1):(which(states_18 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier firn_depth')+1):(which(states_19 == 'glacier firn_depth')+9)]), ' ')))
  firn_depth[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier firn_depth')+1):(which(states_20 == 'glacier firn_depth')+9)]), ' ')))
  
  
  firn_h[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier firn_h')+1):(which(states_1 == 'glacier firn_h')+90)]), ' ')))
  firn_h[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier firn_h')+1):(which(states_2 == 'glacier firn_h')+90)]), ' ')))
  firn_h[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier firn_h')+1):(which(states_3 == 'glacier firn_h')+90)]), ' ')))
  firn_h[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier firn_h')+1):(which(states_4 == 'glacier firn_h')+90)]), ' ')))
  firn_h[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier firn_h')+1):(which(states_5 == 'glacier firn_h')+90)]), ' ')))
  firn_h[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier firn_h')+1):(which(states_6 == 'glacier firn_h')+90)]), ' ')))
  firn_h[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier firn_h')+1):(which(states_7 == 'glacier firn_h')+90)]), ' ')))
  firn_h[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier firn_h')+1):(which(states_8 == 'glacier firn_h')+90)]), ' ')))
  firn_h[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier firn_h')+1):(which(states_9 == 'glacier firn_h')+90)]), ' ')))
  firn_h[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier firn_h')+1):(which(states_10 == 'glacier firn_h')+90)]), ' ')))
  firn_h[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier firn_h')+1):(which(states_11 == 'glacier firn_h')+90)]), ' ')))
  firn_h[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier firn_h')+1):(which(states_12 == 'glacier firn_h')+90)]), ' ')))
  firn_h[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier firn_h')+1):(which(states_13 == 'glacier firn_h')+90)]), ' ')))
  firn_h[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier firn_h')+1):(which(states_14 == 'glacier firn_h')+90)]), ' ')))
  firn_h[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier firn_h')+1):(which(states_15 == 'glacier firn_h')+90)]), ' ')))
  firn_h[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier firn_h')+1):(which(states_16 == 'glacier firn_h')+90)]), ' ')))
  firn_h[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier firn_h')+1):(which(states_17 == 'glacier firn_h')+90)]), ' ')))
  firn_h[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier firn_h')+1):(which(states_18 == 'glacier firn_h')+90)]), ' ')))
  firn_h[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier firn_h')+1):(which(states_19 == 'glacier firn_h')+90)]), ' ')))
  firn_h[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier firn_h')+1):(which(states_20 == 'glacier firn_h')+90)]), ' ')))
  
  
  firn_dens[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier firn_dens')+1):(which(states_1 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier firn_dens')+1):(which(states_2 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier firn_dens')+1):(which(states_3 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier firn_dens')+1):(which(states_4 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier firn_dens')+1):(which(states_5 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier firn_dens')+1):(which(states_6 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier firn_dens')+1):(which(states_7 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier firn_dens')+1):(which(states_8 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier firn_dens')+1):(which(states_9 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier firn_dens')+1):(which(states_10 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier firn_dens')+1):(which(states_11 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier firn_dens')+1):(which(states_12 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier firn_dens')+1):(which(states_13 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier firn_dens')+1):(which(states_14 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier firn_dens')+1):(which(states_15 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier firn_dens')+1):(which(states_16 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier firn_dens')+1):(which(states_17 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier firn_dens')+1):(which(states_18 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier firn_dens')+1):(which(states_19 == 'glacier firn_dens')+90)]), ' ')))
  firn_dens[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier firn_dens')+1):(which(states_20 == 'glacier firn_dens')+90)]), ' ')))
  
  
  ice[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier ice')+1):(which(states_1 == 'glacier ice')+9)]), ' ')))
  ice[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier ice')+1):(which(states_2 == 'glacier ice')+9)]), ' ')))
  ice[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier ice')+1):(which(states_3 == 'glacier ice')+9)]), ' ')))
  ice[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier ice')+1):(which(states_4 == 'glacier ice')+9)]), ' ')))
  ice[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier ice')+1):(which(states_5 == 'glacier ice')+9)]), ' ')))
  ice[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier ice')+1):(which(states_6 == 'glacier ice')+9)]), ' ')))
  ice[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier ice')+1):(which(states_7 == 'glacier ice')+9)]), ' ')))
  ice[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier ice')+1):(which(states_8 == 'glacier ice')+9)]), ' ')))
  ice[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier ice')+1):(which(states_9 == 'glacier ice')+9)]), ' ')))
  ice[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier ice')+1):(which(states_10 == 'glacier ice')+9)]), ' ')))
  ice[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier ice')+1):(which(states_11 == 'glacier ice')+9)]), ' ')))
  ice[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier ice')+1):(which(states_12 == 'glacier ice')+9)]), ' ')))
  ice[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier ice')+1):(which(states_13 == 'glacier ice')+9)]), ' ')))
  ice[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier ice')+1):(which(states_14 == 'glacier ice')+9)]), ' ')))
  ice[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier ice')+1):(which(states_15 == 'glacier ice')+9)]), ' ')))
  ice[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier ice')+1):(which(states_16 == 'glacier ice')+9)]), ' ')))
  ice[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier ice')+1):(which(states_17 == 'glacier ice')+9)]), ' ')))
  ice[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier ice')+1):(which(states_18 == 'glacier ice')+9)]), ' ')))
  ice[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier ice')+1):(which(states_19 == 'glacier ice')+9)]), ' ')))
  ice[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier ice')+1):(which(states_20 == 'glacier ice')+9)]), ' ')))
  
  
  nfirn[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier nfirn')+1):(which(states_1 == 'glacier nfirn')+9)]), ' ')))
  nfirn[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier nfirn')+1):(which(states_2 == 'glacier nfirn')+9)]), ' ')))
  nfirn[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier nfirn')+1):(which(states_3 == 'glacier nfirn')+9)]), ' ')))
  nfirn[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier nfirn')+1):(which(states_4 == 'glacier nfirn')+9)]), ' ')))
  nfirn[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier nfirn')+1):(which(states_5 == 'glacier nfirn')+9)]), ' ')))
  nfirn[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier nfirn')+1):(which(states_6 == 'glacier nfirn')+9)]), ' ')))
  nfirn[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier nfirn')+1):(which(states_7 == 'glacier nfirn')+9)]), ' ')))
  nfirn[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier nfirn')+1):(which(states_8 == 'glacier nfirn')+9)]), ' ')))
  nfirn[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier nfirn')+1):(which(states_9 == 'glacier nfirn')+9)]), ' ')))
  nfirn[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier nfirn')+1):(which(states_10 == 'glacier nfirn')+9)]), ' ')))
  nfirn[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier nfirn')+1):(which(states_11 == 'glacier nfirn')+9)]), ' ')))
  nfirn[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier nfirn')+1):(which(states_12 == 'glacier nfirn')+9)]), ' ')))
  nfirn[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier nfirn')+1):(which(states_13 == 'glacier nfirn')+9)]), ' ')))
  nfirn[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier nfirn')+1):(which(states_14 == 'glacier nfirn')+9)]), ' ')))
  nfirn[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier nfirn')+1):(which(states_15 == 'glacier nfirn')+9)]), ' ')))
  nfirn[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier nfirn')+1):(which(states_16 == 'glacier nfirn')+9)]), ' ')))
  nfirn[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier nfirn')+1):(which(states_17 == 'glacier nfirn')+9)]), ' ')))
  nfirn[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier nfirn')+1):(which(states_18 == 'glacier nfirn')+9)]), ' ')))
  nfirn[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier nfirn')+1):(which(states_19 == 'glacier nfirn')+9)]), ' ')))
  nfirn[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier nfirn')+1):(which(states_20 == 'glacier nfirn')+9)]), ' ')))
  
  
  #Create states data frame
  
  states_array <- array(data = NA, dim = c(20, 90, 7))
  
  
  states_array[,,1] <- Albedo
  states_array[,,2] <- SWE
  states_array[,,3] <- cc_s
  states_array[,,4] <- h2o
  states_array[,,5] <- z_s
  states_array[,,6] <- firn
  states_array[,,7] <- ice
  
  
  #Calculate observation error
  
  athabasca_eval <- read.csv('Path to albedo remote sensing evaluation data/File.csv', sep = ';')
  
  reg_model <- summary(lm(athabasca_eval$obs ~ athabasca_eval$rs))
  
  R = round(reg_model$sigma, 2)
  
  albedo_rs <- read.csv('Path to remote sensing albedo/HRU_Snow_Albedo_Athabasca.csv', header = F)[-c(exclusion_dates),]
  
  albedo_rs <- ifelse(as.matrix(albedo_rs) == -9999, NA, as.matrix(albedo_rs)) 
  
  albedo_obs_pert0 <- rnorm(20, 1, R)
  
  
  updated_states_array <- array(data = NA, dim = c(20,90,7))
  
  for (m in 1:90) {
    
    albedo_obs_pert <- albedo_obs_pert0*albedo_rs[j,m]
    
    #Calculate Kalman gain
    
    state_mean <- colMeans(states_array[,m,])
    
    state_pert <- t(as.matrix(rbind(states_array[,m,][1,] - state_mean, states_array[,m,][2,] - state_mean, states_array[,m,][3,] - state_mean,
                                    states_array[,m,][4,] - state_mean, states_array[,m,][5,] - state_mean, states_array[,m,][6,] - state_mean,
                                    states_array[,m,][7,] - state_mean, states_array[,m,][8,] - state_mean, states_array[,m,][9,] - state_mean,
                                    states_array[,m,][10,] - state_mean, states_array[,m,][11,] - state_mean, states_array[,m,][12,] - state_mean,
                                    states_array[,m,][13,] - state_mean, states_array[,m,][14,] - state_mean, states_array[,m,][15,] - state_mean,
                                    states_array[,m,][16,] - state_mean, states_array[,m,][17,] - state_mean, states_array[,m,][18,] - state_mean,
                                    states_array[,m,][19,] - state_mean, states_array[,m,][20,] - state_mean)))
    
    
    P_matrix <- (state_pert %*% t(state_pert))/2
    
    H_matrix <- matrix(c(1,0,0,0,0,0,0), nrow = 1, ncol = 7)
    
    
    K = as.vector((P_matrix %*% t(H_matrix)))/as.vector(((H_matrix %*% P_matrix %*% t(H_matrix)) + R))
    
    
    updated_states <- matrix(nrow = 20, ncol = 7)
    
    updated_states[1,] = states_array[1,m,] + K*(albedo_obs_pert[1] - states_array[1,m,1])
    updated_states[2,] = states_array[2,m,] + K*(albedo_obs_pert[2] - states_array[2,m,1])
    updated_states[3,] = states_array[3,m,] + K*(albedo_obs_pert[3] - states_array[3,m,1])
    updated_states[4,] = states_array[4,m,] + K*(albedo_obs_pert[4] - states_array[4,m,1])
    updated_states[5,] = states_array[5,m,] + K*(albedo_obs_pert[5] - states_array[5,m,1])
    updated_states[6,] = states_array[6,m,] + K*(albedo_obs_pert[6] - states_array[6,m,1])
    updated_states[7,] = states_array[7,m,] + K*(albedo_obs_pert[7] - states_array[7,m,1])
    updated_states[8,] = states_array[8,m,] + K*(albedo_obs_pert[8] - states_array[8,m,1])
    updated_states[9,] = states_array[9,m,] + K*(albedo_obs_pert[9] - states_array[9,m,1])
    updated_states[10,] = states_array[10,m,] + K*(albedo_obs_pert[10] - states_array[10,m,1])
    updated_states[11,] = states_array[11,m,] + K*(albedo_obs_pert[11] - states_array[11,m,1])
    updated_states[12,] = states_array[12,m,] + K*(albedo_obs_pert[12] - states_array[12,m,1])
    updated_states[13,] = states_array[13,m,] + K*(albedo_obs_pert[13] - states_array[13,m,1])
    updated_states[14,] = states_array[14,m,] + K*(albedo_obs_pert[14] - states_array[14,m,1])
    updated_states[15,] = states_array[15,m,] + K*(albedo_obs_pert[15] - states_array[15,m,1])
    updated_states[16,] = states_array[16,m,] + K*(albedo_obs_pert[16] - states_array[16,m,1])
    updated_states[17,] = states_array[17,m,] + K*(albedo_obs_pert[17] - states_array[17,m,1])
    updated_states[18,] = states_array[18,m,] + K*(albedo_obs_pert[18] - states_array[18,m,1])
    updated_states[19,] = states_array[19,m,] + K*(albedo_obs_pert[19] - states_array[19,m,1])
    updated_states[20,] = states_array[20,m,] + K*(albedo_obs_pert[20] - states_array[20,m,1])
    
    
    updated_states[,1] <- ifelse(updated_states[,1] > 1, 1, ifelse(updated_states[,1] < 0, 0, updated_states[,1]))
    updated_states[,2] <- ifelse(updated_states[,2] < 0, 0, updated_states[,2])
    updated_states[,3] <- ifelse(updated_states[,3] > 0, 0, updated_states[,3])
    updated_states[,4] <- ifelse(updated_states[,4] < 0, 0, updated_states[,4])
    updated_states[,5] <- ifelse(updated_states[,5] < 0, 0, updated_states[,5])
    updated_states[,6] <- ifelse(updated_states[,6] < 0, 0, updated_states[,6])
    updated_states[,7] <- ifelse(updated_states[,7] < 0, 0, updated_states[,7])
    
    updated_states_array[,m,] <-  updated_states
    
  }
  
  
  
  states_array_final <- ifelse(is.na(updated_states_array), states_array, updated_states_array)
  
  
  #SWE
  
  states_array_final[,,2] <- ifelse(states_array_final[,,5] <= 0, 0, states_array_final[,,2])
  
  #Snowcover
  
  states_array_final <- abind(states_array_final, snowcover)
  states_array_final[,,8] <- ifelse(states_array_final[,,5] > 0, 1, 0)
  
  #Layer count
  
  states_array_final <- abind(states_array_final, layer_count)
  states_array_final[,,9] <- ifelse(states_array_final[,,5] > 0.1, 2, 1)
  
  #Rho
  
  states_array_final <- abind(states_array_final, rho)
  states_array_final[,,10] <- ifelse(states_array_final[,,2] > 0, states_array_final[,,2]/states_array_final[,,5], 250)
  states_array_final[,,10] <- ifelse(is.infinite(states_array_final[,,10]), 0, states_array_final[,,10])
  
  #Active layer depth
  
  states_array_final <- abind(states_array_final, states_array_final[,,5])
  states_array_final[,,11] <- ifelse(states_array_final[,,11] > 0.1, 0.1, states_array_final[,,11])
  
  #Lower layer depth
  
  states_array_final <- abind(states_array_final, (states_array_final[,,5] - states_array_final[,,11]))
  states_array_final[,,12] <- ifelse(states_array_final[,,5] <= 0, 0, states_array_final[,,12])
  
  #Active layer specific mass
  
  states_array_final <- abind(states_array_final, (states_array_final[,,11]*states_array_final[,,10]))
  
  #Lower layer specific mass
  
  states_array_final <- abind(states_array_final, (states_array_final[,,12]*states_array_final[,,10]))
  
  #Snow surface temperature
  
  states_array_final <- abind(states_array_final, T_s)
  states_array_final[,,15] <- ifelse(states_array_final[,,12] > 0, states_array_final[,,15], -10)
  states_array_final[,,15] <- ifelse(states_array_final[,,15] < -10, -10, states_array_final[,,15])
  
  #Active layer temperature
  
  states_array_final <- abind(states_array_final, states_array_final[,,15])
  
  #Active layer cold content
  
  states_array_final <- abind(states_array_final, (2102*1000*(states_array_final[,,15]-0)*(states_array_final[,,13]/1000)))
  states_array_final[,,17] <- ifelse(states_array_final[,,17] > 0, 0, states_array_final[,,17])
  
  #Lower layer cold content
  
  states_array_final <- abind(states_array_final, (states_array_final[,,3] - states_array_final[,,17]))
  states_array_final[,,18] <- ifelse(states_array_final[,,18] > 0, 0, states_array_final[,,18])
  
  #% of liquid H2O saturation
  
  states_array_final <- abind(states_array_final, matrix(rep(0.0001, 1800), nrow = 20, ncol = 90))
  
  
  #Set some SNOBAL states to zero
  
  states_array_final[,,3] <- ifelse(states_array_final[,,5] == 0, 0, states_array[,,3])
  states_array_final[,,17] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,17])
  states_array_final[,,18] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,18])
  states_array_final[,,4] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,4])
  states_array_final[,,19] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,19])
  states_array_final[,,9] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,9])
  states_array_final[,,13] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,13])
  states_array_final[,,14] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,14])
  states_array_final[,,10] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,10])
  states_array_final[,,11] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,11])
  states_array_final[,,12] <- ifelse(states_array_final[,,5] == 0, 0, states_array_final[,,12])
  
  
  #Update glacier states
  
  
  firn_dens_i <- ifelse(firn_dens == 0, NA, firn_dens)                 
  
  states_array_final <- abind(states_array_final, matrix(rep(NA, 1800), nrow = 20, ncol = 90))
  states_array_final <- abind(states_array_final, matrix(rep(NA, 1800), nrow = 20, ncol = 90))
  
  firn_h_matrix <- matrix(nrow = 20, ncol = 900)
  
  
  for (q in 1:20) {
    
    mean_firn_dens <- vector()
    
    for (o in 0:89) {
      
      mean_firn_dens[o+1] <- mean(c(firn_dens_i[q,1+o], firn_dens_i[q,91+o], firn_dens_i[q,181+o], firn_dens_i[q,271+o], firn_dens_i[q,361+o], 
                                    firn_dens_i[q,451+o], firn_dens_i[q,541+o], firn_dens_i[q,631+o], firn_dens_i[q,721+o], firn_dens_i[q,811+o]), na.rm = T)
      
    }
    
    
    mean_firn_dens <- ifelse(is.nan(mean_firn_dens), 0, mean_firn_dens)
    
    
    firn_h_updt <- (states_array_final[q,,6]/mean_firn_dens)
    
    firn_h_updt <- ifelse(is.nan(firn_h_updt), 0, firn_h_updt)
    
    firn_h_updt <- ifelse(is.infinite(firn_h_updt), 0, firn_h_updt)
    
    firn_h_updt_floor <- floor(firn_h_updt)
    
    firn_h_updt_floor <- ifelse(firn_h_updt_floor == 10, 9, firn_h_updt_floor)
    
    firn_h_updt <- ifelse(firn_h_updt > 10, 10, firn_h_updt)
    
    
    firn_h_final <- c(firn_h_updt - firn_h_updt_floor,
                      
                      ifelse(firn_h_updt_floor > 0, 1, 0),
                      
                      ifelse(ifelse((firn_h_updt_floor - 1) < 0, 0, (firn_h_updt_floor - 1)) > 0, 1, 0),
                      
                      ifelse(ifelse((firn_h_updt_floor - 2) < 0, 0, (firn_h_updt_floor - 2)) > 0, 1, 0),
                      
                      ifelse(ifelse((firn_h_updt_floor - 3) < 0, 0, (firn_h_updt_floor - 3)) > 0, 1, 0),
                      
                      ifelse(ifelse((firn_h_updt_floor - 4) < 0, 0, (firn_h_updt_floor - 4)) > 0, 1, 0),
                      
                      ifelse(ifelse((firn_h_updt_floor - 5) < 0, 0, (firn_h_updt_floor - 5)) > 0, 1, 0),
                      
                      ifelse(ifelse((firn_h_updt_floor - 6) < 0, 0, (firn_h_updt_floor - 6)) > 0, 1, 0),
                      
                      ifelse(ifelse((firn_h_updt_floor - 7) < 0, 0, (firn_h_updt_floor - 7)) > 0, 1, 0),
                      
                      ifelse(ifelse((firn_h_updt_floor - 8) < 0, 0, (firn_h_updt_floor - 8)) > 0, 1, 0))*1000
    
    
    firn_h_final <- ifelse(firn_h_final < 0, 1000, firn_h_final)
    
    
    nfirn_updt <- ifelse(firn_h_updt_floor > 0, firn_h_updt_floor + 1, 0)
    nfirn_updt <- ifelse(nfirn_updt > 10, 10, nfirn_updt)
    
    
    firn_depth_updt <- vector()
    
    for (p in 0:89) {
      
      firn_depth_updt[p+1] <- (firn_h_final[1+p] + firn_h_final[91+p] + firn_h_final[181+p] + firn_h_final[271+p] + firn_h_final[361+p] + 
                                 firn_h_final[451+p] + firn_h_final[541+p] + firn_h_final[631+p] + firn_h_final[721+p] + firn_h_final[811+p])
      
    }
    
    
    states_array_final[q,,20] <- nfirn_updt
    
    states_array_final[q,,21] <- firn_depth_updt
    
    firn_h_matrix[q,] <- firn_h_final
    
  }
  
  
  
  for (n in 0:8) {
    
    states_1[670+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_1[340+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_1[384+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_1[417+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_1[505+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_1[861+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_1[1056+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_1[494+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_1[450+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_1[483+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_1[516+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_1[527+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_1[461+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_1[472+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_1[351+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_1[362+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_1[395+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_1[406+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_1[428+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_1[1067+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_1[861+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_2[670+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_2[340+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_2[384+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_2[417+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_2[505+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_2[861+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_2[1056+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_2[494+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_2[450+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_2[483+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_2[516+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_2[527+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_2[461+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_2[472+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_2[351+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_2[362+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_2[395+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_2[406+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_2[428+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_2[1067+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_2[861+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_3[670+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_3[340+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_3[384+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_3[417+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_3[505+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_3[861+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_3[1056+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_3[494+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_3[450+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_3[483+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_3[516+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_3[527+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_3[461+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_3[472+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_3[351+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_3[362+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_3[395+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_3[406+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_3[428+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_3[1067+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_3[861+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_4[670+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_4[340+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_4[384+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_4[417+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_4[505+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_4[861+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_4[1056+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_4[494+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_4[450+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_4[483+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_4[516+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_4[527+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_4[461+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_4[472+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_4[351+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_4[362+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_4[395+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_4[406+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_4[428+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_4[1067+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_4[861+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_5[670+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_5[340+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_5[384+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_5[417+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_5[505+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_5[861+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_5[1056+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_5[494+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_5[450+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_5[483+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_5[516+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_5[527+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_5[461+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_5[472+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_5[351+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_5[362+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_5[395+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_5[406+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_5[428+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_5[1067+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_5[861+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_6[670+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_6[340+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_6[384+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_6[417+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_6[505+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_6[861+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_6[1056+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_6[494+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_6[450+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_6[483+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_6[516+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_6[527+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_6[461+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_6[472+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_6[351+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_6[362+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_6[395+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_6[406+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_6[428+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_6[1067+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_6[861+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_7[670+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_7[340+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_7[384+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_7[417+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_7[505+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_7[861+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_7[1056+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_7[494+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_7[450+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_7[483+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_7[516+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_7[527+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_7[461+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_7[472+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_7[351+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_7[362+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_7[395+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_7[406+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_7[428+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_7[1067+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_7[861+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_8[670+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_8[340+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_8[384+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_8[417+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_8[505+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_8[861+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_8[1056+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_8[494+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_8[450+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_8[483+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_8[516+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_8[527+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_8[461+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_8[472+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_8[351+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_8[362+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_8[395+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_8[406+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_8[428+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_8[1067+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_8[861+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_9[670+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_9[340+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_9[384+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_9[417+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_9[505+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_9[861+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_9[1056+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_9[494+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_9[450+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_9[483+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_9[516+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_9[527+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_9[461+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_9[472+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_9[351+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_9[362+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_9[395+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_9[406+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_9[428+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_9[1067+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_9[861+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_10[670+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_10[340+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_10[384+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_10[417+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_10[505+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_10[861+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_10[1056+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_10[494+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_10[450+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_10[483+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_10[516+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_10[527+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_10[461+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_10[472+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_10[351+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_10[362+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_10[395+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_10[406+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_10[428+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_10[1067+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_10[861+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_11[670+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_11[340+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_11[384+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_11[417+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_11[505+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_11[861+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_11[1056+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_11[494+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_11[450+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_11[483+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_11[516+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_11[527+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_11[461+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_11[472+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_11[351+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_11[362+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_11[395+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_11[406+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_11[428+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_11[1067+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_11[861+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_12[670+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_12[340+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_12[384+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_12[417+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_12[505+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_12[861+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_12[1056+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_12[494+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_12[450+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_12[483+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_12[516+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_12[527+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_12[461+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_12[472+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_12[351+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_12[362+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_12[395+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_12[406+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_12[428+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_12[1067+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_12[861+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_13[670+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_13[340+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_13[384+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_13[417+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_13[505+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_13[861+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_13[1056+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_13[494+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_13[450+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_13[483+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_13[516+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_13[527+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_13[461+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_13[472+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_13[351+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_13[362+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_13[395+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_13[406+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_13[428+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_13[1067+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_13[861+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_14[670+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_14[340+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_14[384+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_14[417+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_14[505+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_14[861+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_14[1056+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_14[494+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_14[450+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_14[483+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_14[516+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_14[527+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_14[461+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_14[472+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_14[351+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_14[362+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_14[395+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_14[406+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_14[428+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_14[1067+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_14[861+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_15[670+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_15[340+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_15[384+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_15[417+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_15[505+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_15[861+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_15[1056+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_15[494+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_15[450+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_15[483+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_15[516+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_15[527+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_15[461+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_15[472+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_15[351+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_15[362+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_15[395+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_15[406+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_15[428+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_15[1067+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_15[861+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_16[670+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_16[340+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_16[384+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_16[417+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_16[505+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_16[861+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_16[1056+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_16[494+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_16[450+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_16[483+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_16[516+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_16[527+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_16[461+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_16[472+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_16[351+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_16[362+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_16[395+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_16[406+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_16[428+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_16[1067+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_16[861+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_17[670+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_17[340+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_17[384+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_17[417+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_17[505+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_17[861+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_17[1056+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_17[494+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_17[450+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_17[483+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_17[516+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_17[527+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_17[461+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_17[472+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_17[351+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_17[362+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_17[395+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_17[406+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_17[428+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_17[1067+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_17[861+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_18[670+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_18[340+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_18[384+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_18[417+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_18[505+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_18[861+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_18[1056+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_18[494+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_18[450+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_18[483+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_18[516+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_18[527+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_18[461+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_18[472+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_18[351+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_18[362+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_18[395+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_18[406+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_18[428+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_18[1067+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_18[861+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_19[670+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_19[340+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_19[384+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_19[417+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_19[505+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_19[861+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_19[1056+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_19[494+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_19[450+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_19[483+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_19[516+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_19[527+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_19[461+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_19[472+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_19[351+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_19[362+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_19[395+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_19[406+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_19[428+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_19[1067+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_19[861+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
    states_20[670+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),1]), collapse = ' ')
    states_20[340+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),2]), collapse = ' ')
    states_20[384+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),3]), collapse = ' ')
    states_20[417+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),4]), collapse = ' ')
    states_20[505+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),5]), collapse = ' ')
    states_20[861+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),6]), collapse = ' ')
    states_20[1056+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),7]), collapse = ' ')
    states_20[494+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),8]), collapse = ' ')
    states_20[450+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),9]), collapse = ' ')
    states_20[483+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),10]), collapse = ' ')
    states_20[516+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),11]), collapse = ' ')
    states_20[527+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),12]), collapse = ' ')
    states_20[461+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),13]), collapse = ' ')
    states_20[472+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),14]), collapse = ' ')
    states_20[351+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),15]), collapse = ' ')
    states_20[362+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),16]), collapse = ' ')
    states_20[395+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),17]), collapse = ' ')
    states_20[406+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),18]), collapse = ' ')
    states_20[428+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),19]), collapse = ' ')
    states_20[1067+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),20]), collapse = ' ')
    states_20[861+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),21]), collapse = ' ')
    
  }
  
  
  
  firn_dens_updt <- t(matrix(rep(c(rep(500,90), rep(520,90), rep(550,90), rep(580,90), rep(610,90), rep(640,90), rep(660, 90), rep(680,90), rep(700,180)), 20), nrow = 900, ncol = 20))
  
  firn_dens_updt <- ifelse(firn_h_matrix > 0, firn_dens_updt, 0)
  
  
  for (r in 0:89) {
    
    states_1[872+r] <- paste(as.character(firn_h_matrix[1,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_2[872+r] <- paste(as.character(firn_h_matrix[2,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_3[872+r] <- paste(as.character(firn_h_matrix[3,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_4[872+r] <- paste(as.character(firn_h_matrix[4,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_5[872+r] <- paste(as.character(firn_h_matrix[5,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_6[872+r] <- paste(as.character(firn_h_matrix[6,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_7[872+r] <- paste(as.character(firn_h_matrix[7,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_8[872+r] <- paste(as.character(firn_h_matrix[8,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_9[872+r] <- paste(as.character(firn_h_matrix[9,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_10[872+r] <- paste(as.character(firn_h_matrix[10,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_11[872+r] <- paste(as.character(firn_h_matrix[11,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_12[872+r] <- paste(as.character(firn_h_matrix[12,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_13[872+r] <- paste(as.character(firn_h_matrix[13,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_14[872+r] <- paste(as.character(firn_h_matrix[14,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_15[872+r] <- paste(as.character(firn_h_matrix[15,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_16[872+r] <- paste(as.character(firn_h_matrix[16,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_17[872+r] <- paste(as.character(firn_h_matrix[17,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_18[872+r] <- paste(as.character(firn_h_matrix[18,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_19[872+r] <- paste(as.character(firn_h_matrix[19,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_20[872+r] <- paste(as.character(firn_h_matrix[20,((10*r)+1):((10*r)+10)]), collapse = ' ')
    
    
    states_1[769+r] <- paste(as.character(firn_dens_updt[1,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_2[769+r] <- paste(as.character(firn_dens_updt[2,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_3[769+r] <- paste(as.character(firn_dens_updt[3,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_4[769+r] <- paste(as.character(firn_dens_updt[4,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_5[769+r] <- paste(as.character(firn_dens_updt[5,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_6[769+r] <- paste(as.character(firn_dens_updt[6,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_7[769+r] <- paste(as.character(firn_dens_updt[7,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_8[769+r] <- paste(as.character(firn_dens_updt[8,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_9[769+r] <- paste(as.character(firn_dens_updt[9,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_10[769+r] <- paste(as.character(firn_dens_updt[10,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_11[769+r] <- paste(as.character(firn_dens_updt[11,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_12[769+r] <- paste(as.character(firn_dens_updt[12,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_13[769+r] <- paste(as.character(firn_dens_updt[13,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_14[769+r] <- paste(as.character(firn_dens_updt[14,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_15[769+r] <- paste(as.character(firn_dens_updt[15,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_16[769+r] <- paste(as.character(firn_dens_updt[16,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_17[769+r] <- paste(as.character(firn_dens_updt[17,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_18[769+r] <- paste(as.character(firn_dens_updt[18,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_19[769+r] <- paste(as.character(firn_dens_updt[19,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_20[769+r] <- paste(as.character(firn_dens_updt[20,((10*r)+1):((10*r)+10)]), collapse = ' ')
    
  }
  
  
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
    
    initial_min_alb_final <- ifelse(initial_min_alb == 0.17, 0.17, states_array_final[s,,1])
    
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

