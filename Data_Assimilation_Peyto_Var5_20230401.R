#Code to perform albedo data assimilation with the CRHM model for Peyto Basin

library(lubridate)
library(abind)


setwd('Path to model DA runs')


dates_rs <- read.csv('Path to remote sensing dates/Dates_Peyto.csv', header = F)[-c(4,14,15,25,36,37),] # -c() excludes dates outside the May to September period

ending_time <- c(as.POSIXct(dates_rs, format = '%Y-%m-%d') + hours(24), as.POSIXct('2021-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'))

start_time <- c(as.POSIXct('2015-10-01 01:00:00', format = '%Y-%m-%d %H:%M:%S'), (ending_time + hours(1)))


initial_min_alb <- c(0.55, 0.55, 0.17, 0.55, 0.3, 0.55, 0.3, 0.3, 0.3, 0.3, 
                     0.3, 0.3, 0.17, 0.3, 0.3, 0.3, 0.3, 0.17, 0.55, 0.55, 
                     0.55, 0.55, 0.3, 0.3, 0.3, 0.3, 0.17, 0.55, 0.55, 0.3, 
                     0.3, 0.3, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.3, 
                     0.17, 0.3, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 
                     0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.55, 
                     0.17, 0.17, 0.17, 0.17, 0.17)


#time index

j = 1

for (j in 2:34) {
  
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
    
    system(paste0("CRHM.exe Peyto_DA.prj en_", i, "_i.obs Parameters", i, ".par"))
    
    
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
  
  SWE <- matrix(nrow = 20, ncol = 65)
  T_s <- matrix(nrow = 20, ncol = 65)
  T_s_0 <- matrix(nrow = 20, ncol = 65)
  cc_s <- matrix(nrow = 20, ncol = 65)
  cc_s_0 <- matrix(nrow = 20, ncol = 65)
  cc_s_l <- matrix(nrow = 20, ncol = 65)
  h2o <- matrix(nrow = 20, ncol = 65)
  h2o_sat <- matrix(nrow = 20, ncol = 65)
  layer_count <- matrix(nrow = 20, ncol = 65)
  m_s_0 <- matrix(nrow = 20, ncol = 65)
  m_s_l <- matrix(nrow = 20, ncol = 65)
  rho <- matrix(nrow = 20, ncol = 65)
  snowcover <- matrix(nrow = 20, ncol = 65)
  z_s <- matrix(nrow = 20, ncol = 65)
  z_s_0 <- matrix(nrow = 20, ncol = 65)
  z_s_l <- matrix(nrow = 20, ncol = 65)
  Albedo <- matrix(nrow = 20, ncol = 65)
  firn <- matrix(nrow = 20, ncol = 65)
  firn_depth <- matrix(nrow = 20, ncol = 65)
  firn_h <- matrix(nrow = 20, ncol = 650)
  firn_dens <- matrix(nrow = 20, ncol = 650)
  ice <- matrix(nrow = 20, ncol = 65)
  nfirn <- matrix(nrow = 20, ncol = 65)
  
  
  #Create vector of states
  
  SWE[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM SWE')+1):(which(states_1 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM SWE')+1):(which(states_2 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM SWE')+1):(which(states_3 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM SWE')+1):(which(states_4 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM SWE')+1):(which(states_5 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM SWE')+1):(which(states_6 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM SWE')+1):(which(states_7 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM SWE')+1):(which(states_8 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM SWE')+1):(which(states_9 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM SWE')+1):(which(states_10 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM SWE')+1):(which(states_11 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM SWE')+1):(which(states_12 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM SWE')+1):(which(states_13 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM SWE')+1):(which(states_14 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM SWE')+1):(which(states_15 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM SWE')+1):(which(states_16 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM SWE')+1):(which(states_17 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM SWE')+1):(which(states_18 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM SWE')+1):(which(states_19 == 'SnobalCRHM SWE')+7)]), ' ')))
  SWE[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM SWE')+1):(which(states_20 == 'SnobalCRHM SWE')+7)]), ' ')))
  
  
  T_s[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM T_s')+1):(which(states_1 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM T_s')+1):(which(states_2 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM T_s')+1):(which(states_3 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM T_s')+1):(which(states_4 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM T_s')+1):(which(states_5 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM T_s')+1):(which(states_6 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM T_s')+1):(which(states_7 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM T_s')+1):(which(states_8 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM T_s')+1):(which(states_9 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM T_s')+1):(which(states_10 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM T_s')+1):(which(states_11 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM T_s')+1):(which(states_12 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM T_s')+1):(which(states_13 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM T_s')+1):(which(states_14 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM T_s')+1):(which(states_15 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM T_s')+1):(which(states_16 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM T_s')+1):(which(states_17 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM T_s')+1):(which(states_18 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM T_s')+1):(which(states_19 == 'SnobalCRHM T_s')+7)]), ' ')))
  T_s[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM T_s')+1):(which(states_20 == 'SnobalCRHM T_s')+7)]), ' ')))
  
  
  T_s_0[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM T_s_0')+1):(which(states_1 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM T_s_0')+1):(which(states_2 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM T_s_0')+1):(which(states_3 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM T_s_0')+1):(which(states_4 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM T_s_0')+1):(which(states_5 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM T_s_0')+1):(which(states_6 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM T_s_0')+1):(which(states_7 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM T_s_0')+1):(which(states_8 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM T_s_0')+1):(which(states_9 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM T_s_0')+1):(which(states_10 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM T_s_0')+1):(which(states_11 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM T_s_0')+1):(which(states_12 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM T_s_0')+1):(which(states_13 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM T_s_0')+1):(which(states_14 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM T_s_0')+1):(which(states_15 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM T_s_0')+1):(which(states_16 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM T_s_0')+1):(which(states_17 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM T_s_0')+1):(which(states_18 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM T_s_0')+1):(which(states_19 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  T_s_0[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM T_s_0')+1):(which(states_20 == 'SnobalCRHM T_s_0')+7)]), ' ')))
  
  
  cc_s[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM cc_s')+1):(which(states_1 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM cc_s')+1):(which(states_2 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM cc_s')+1):(which(states_3 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM cc_s')+1):(which(states_4 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM cc_s')+1):(which(states_5 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM cc_s')+1):(which(states_6 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM cc_s')+1):(which(states_7 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM cc_s')+1):(which(states_8 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM cc_s')+1):(which(states_9 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM cc_s')+1):(which(states_10 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM cc_s')+1):(which(states_11 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM cc_s')+1):(which(states_12 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM cc_s')+1):(which(states_13 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM cc_s')+1):(which(states_14 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM cc_s')+1):(which(states_15 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM cc_s')+1):(which(states_16 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM cc_s')+1):(which(states_17 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM cc_s')+1):(which(states_18 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM cc_s')+1):(which(states_19 == 'SnobalCRHM cc_s')+7)]), ' ')))
  cc_s[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM cc_s')+1):(which(states_20 == 'SnobalCRHM cc_s')+7)]), ' ')))
  
  
  cc_s_0[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM cc_s_0')+1):(which(states_1 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM cc_s_0')+1):(which(states_2 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM cc_s_0')+1):(which(states_3 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM cc_s_0')+1):(which(states_4 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM cc_s_0')+1):(which(states_5 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM cc_s_0')+1):(which(states_6 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM cc_s_0')+1):(which(states_7 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM cc_s_0')+1):(which(states_8 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM cc_s_0')+1):(which(states_9 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM cc_s_0')+1):(which(states_10 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM cc_s_0')+1):(which(states_11 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM cc_s_0')+1):(which(states_12 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM cc_s_0')+1):(which(states_13 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM cc_s_0')+1):(which(states_14 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM cc_s_0')+1):(which(states_15 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM cc_s_0')+1):(which(states_16 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM cc_s_0')+1):(which(states_17 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM cc_s_0')+1):(which(states_18 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM cc_s_0')+1):(which(states_19 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  cc_s_0[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM cc_s_0')+1):(which(states_20 == 'SnobalCRHM cc_s_0')+7)]), ' ')))
  
  
  cc_s_l[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM cc_s_l')+1):(which(states_1 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM cc_s_l')+1):(which(states_2 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM cc_s_l')+1):(which(states_3 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM cc_s_l')+1):(which(states_4 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM cc_s_l')+1):(which(states_5 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM cc_s_l')+1):(which(states_6 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM cc_s_l')+1):(which(states_7 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM cc_s_l')+1):(which(states_8 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM cc_s_l')+1):(which(states_9 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM cc_s_l')+1):(which(states_10 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM cc_s_l')+1):(which(states_11 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM cc_s_l')+1):(which(states_12 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM cc_s_l')+1):(which(states_13 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM cc_s_l')+1):(which(states_14 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM cc_s_l')+1):(which(states_15 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM cc_s_l')+1):(which(states_16 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM cc_s_l')+1):(which(states_17 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM cc_s_l')+1):(which(states_18 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM cc_s_l')+1):(which(states_19 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  cc_s_l[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM cc_s_l')+1):(which(states_20 == 'SnobalCRHM cc_s_l')+7)]), ' ')))
  
  
  h2o[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM h2o')+1):(which(states_1 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM h2o')+1):(which(states_2 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM h2o')+1):(which(states_3 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM h2o')+1):(which(states_4 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM h2o')+1):(which(states_5 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM h2o')+1):(which(states_6 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM h2o')+1):(which(states_7 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM h2o')+1):(which(states_8 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM h2o')+1):(which(states_9 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM h2o')+1):(which(states_10 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM h2o')+1):(which(states_11 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM h2o')+1):(which(states_12 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM h2o')+1):(which(states_13 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM h2o')+1):(which(states_14 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM h2o')+1):(which(states_15 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM h2o')+1):(which(states_16 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM h2o')+1):(which(states_17 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM h2o')+1):(which(states_18 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM h2o')+1):(which(states_19 == 'SnobalCRHM h2o')+7)]), ' ')))
  h2o[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM h2o')+1):(which(states_20 == 'SnobalCRHM h2o')+7)]), ' ')))
  
  
  h2o_sat[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM h2o_sat')+1):(which(states_1 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM h2o_sat')+1):(which(states_2 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM h2o_sat')+1):(which(states_3 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM h2o_sat')+1):(which(states_4 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM h2o_sat')+1):(which(states_5 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM h2o_sat')+1):(which(states_6 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM h2o_sat')+1):(which(states_7 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM h2o_sat')+1):(which(states_8 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM h2o_sat')+1):(which(states_9 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM h2o_sat')+1):(which(states_10 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM h2o_sat')+1):(which(states_11 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM h2o_sat')+1):(which(states_12 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM h2o_sat')+1):(which(states_13 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM h2o_sat')+1):(which(states_14 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM h2o_sat')+1):(which(states_15 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM h2o_sat')+1):(which(states_16 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM h2o_sat')+1):(which(states_17 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM h2o_sat')+1):(which(states_18 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM h2o_sat')+1):(which(states_19 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  h2o_sat[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM h2o_sat')+1):(which(states_20 == 'SnobalCRHM h2o_sat')+7)]), ' ')))
  
  
  layer_count[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM layer_count')+1):(which(states_1 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM layer_count')+1):(which(states_2 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM layer_count')+1):(which(states_3 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM layer_count')+1):(which(states_4 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM layer_count')+1):(which(states_5 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM layer_count')+1):(which(states_6 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM layer_count')+1):(which(states_7 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM layer_count')+1):(which(states_8 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM layer_count')+1):(which(states_9 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM layer_count')+1):(which(states_10 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM layer_count')+1):(which(states_11 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM layer_count')+1):(which(states_12 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM layer_count')+1):(which(states_13 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM layer_count')+1):(which(states_14 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM layer_count')+1):(which(states_15 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM layer_count')+1):(which(states_16 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM layer_count')+1):(which(states_17 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM layer_count')+1):(which(states_18 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM layer_count')+1):(which(states_19 == 'SnobalCRHM layer_count')+7)]), ' ')))
  layer_count[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM layer_count')+1):(which(states_20 == 'SnobalCRHM layer_count')+7)]), ' ')))
  
  
  m_s_0[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM m_s_0')+1):(which(states_1 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM m_s_0')+1):(which(states_2 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM m_s_0')+1):(which(states_3 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM m_s_0')+1):(which(states_4 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM m_s_0')+1):(which(states_5 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM m_s_0')+1):(which(states_6 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM m_s_0')+1):(which(states_7 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM m_s_0')+1):(which(states_8 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM m_s_0')+1):(which(states_9 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM m_s_0')+1):(which(states_10 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM m_s_0')+1):(which(states_11 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM m_s_0')+1):(which(states_12 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM m_s_0')+1):(which(states_13 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM m_s_0')+1):(which(states_14 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM m_s_0')+1):(which(states_15 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM m_s_0')+1):(which(states_16 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM m_s_0')+1):(which(states_17 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM m_s_0')+1):(which(states_18 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM m_s_0')+1):(which(states_19 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  m_s_0[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM m_s_0')+1):(which(states_20 == 'SnobalCRHM m_s_0')+7)]), ' ')))
  
  
  m_s_l[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM m_s_l')+1):(which(states_1 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM m_s_l')+1):(which(states_2 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM m_s_l')+1):(which(states_3 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM m_s_l')+1):(which(states_4 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM m_s_l')+1):(which(states_5 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM m_s_l')+1):(which(states_6 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM m_s_l')+1):(which(states_7 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM m_s_l')+1):(which(states_8 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM m_s_l')+1):(which(states_9 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM m_s_l')+1):(which(states_10 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM m_s_l')+1):(which(states_11 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM m_s_l')+1):(which(states_12 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM m_s_l')+1):(which(states_13 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM m_s_l')+1):(which(states_14 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM m_s_l')+1):(which(states_15 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM m_s_l')+1):(which(states_16 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM m_s_l')+1):(which(states_17 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM m_s_l')+1):(which(states_18 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM m_s_l')+1):(which(states_19 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  m_s_l[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM m_s_l')+1):(which(states_20 == 'SnobalCRHM m_s_l')+7)]), ' ')))
  
  
  rho[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM rho')+1):(which(states_1 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM rho')+1):(which(states_2 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM rho')+1):(which(states_3 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM rho')+1):(which(states_4 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM rho')+1):(which(states_5 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM rho')+1):(which(states_6 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM rho')+1):(which(states_7 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM rho')+1):(which(states_8 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM rho')+1):(which(states_9 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM rho')+1):(which(states_10 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM rho')+1):(which(states_11 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM rho')+1):(which(states_12 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM rho')+1):(which(states_13 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM rho')+1):(which(states_14 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM rho')+1):(which(states_15 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM rho')+1):(which(states_16 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM rho')+1):(which(states_17 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM rho')+1):(which(states_18 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM rho')+1):(which(states_19 == 'SnobalCRHM rho')+7)]), ' ')))
  rho[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM rho')+1):(which(states_20 == 'SnobalCRHM rho')+7)]), ' ')))
  
  
  snowcover[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM snowcover')+1):(which(states_1 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM snowcover')+1):(which(states_2 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM snowcover')+1):(which(states_3 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM snowcover')+1):(which(states_4 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM snowcover')+1):(which(states_5 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM snowcover')+1):(which(states_6 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM snowcover')+1):(which(states_7 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM snowcover')+1):(which(states_8 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM snowcover')+1):(which(states_9 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM snowcover')+1):(which(states_10 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM snowcover')+1):(which(states_11 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM snowcover')+1):(which(states_12 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM snowcover')+1):(which(states_13 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM snowcover')+1):(which(states_14 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM snowcover')+1):(which(states_15 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM snowcover')+1):(which(states_16 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM snowcover')+1):(which(states_17 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM snowcover')+1):(which(states_18 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM snowcover')+1):(which(states_19 == 'SnobalCRHM snowcover')+7)]), ' ')))
  snowcover[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM snowcover')+1):(which(states_20 == 'SnobalCRHM snowcover')+7)]), ' ')))
  
  
  z_s[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM z_s')+1):(which(states_1 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM z_s')+1):(which(states_2 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM z_s')+1):(which(states_3 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM z_s')+1):(which(states_4 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM z_s')+1):(which(states_5 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM z_s')+1):(which(states_6 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM z_s')+1):(which(states_7 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM z_s')+1):(which(states_8 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM z_s')+1):(which(states_9 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM z_s')+1):(which(states_10 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM z_s')+1):(which(states_11 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM z_s')+1):(which(states_12 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM z_s')+1):(which(states_13 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM z_s')+1):(which(states_14 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM z_s')+1):(which(states_15 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM z_s')+1):(which(states_16 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM z_s')+1):(which(states_17 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM z_s')+1):(which(states_18 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM z_s')+1):(which(states_19 == 'SnobalCRHM z_s')+7)]), ' ')))
  z_s[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM z_s')+1):(which(states_20 == 'SnobalCRHM z_s')+7)]), ' ')))
  
  
  z_s_0[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM z_s_0')+1):(which(states_1 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM z_s_0')+1):(which(states_2 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM z_s_0')+1):(which(states_3 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM z_s_0')+1):(which(states_4 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM z_s_0')+1):(which(states_5 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM z_s_0')+1):(which(states_6 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM z_s_0')+1):(which(states_7 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM z_s_0')+1):(which(states_8 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM z_s_0')+1):(which(states_9 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM z_s_0')+1):(which(states_10 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM z_s_0')+1):(which(states_11 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM z_s_0')+1):(which(states_12 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM z_s_0')+1):(which(states_13 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM z_s_0')+1):(which(states_14 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM z_s_0')+1):(which(states_15 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM z_s_0')+1):(which(states_16 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM z_s_0')+1):(which(states_17 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM z_s_0')+1):(which(states_18 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM z_s_0')+1):(which(states_19 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  z_s_0[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM z_s_0')+1):(which(states_20 == 'SnobalCRHM z_s_0')+7)]), ' ')))
  
  
  z_s_l[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'SnobalCRHM z_s_l')+1):(which(states_1 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'SnobalCRHM z_s_l')+1):(which(states_2 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'SnobalCRHM z_s_l')+1):(which(states_3 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'SnobalCRHM z_s_l')+1):(which(states_4 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'SnobalCRHM z_s_l')+1):(which(states_5 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'SnobalCRHM z_s_l')+1):(which(states_6 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'SnobalCRHM z_s_l')+1):(which(states_7 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'SnobalCRHM z_s_l')+1):(which(states_8 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'SnobalCRHM z_s_l')+1):(which(states_9 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'SnobalCRHM z_s_l')+1):(which(states_10 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'SnobalCRHM z_s_l')+1):(which(states_11 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'SnobalCRHM z_s_l')+1):(which(states_12 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'SnobalCRHM z_s_l')+1):(which(states_13 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'SnobalCRHM z_s_l')+1):(which(states_14 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'SnobalCRHM z_s_l')+1):(which(states_15 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'SnobalCRHM z_s_l')+1):(which(states_16 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'SnobalCRHM z_s_l')+1):(which(states_17 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'SnobalCRHM z_s_l')+1):(which(states_18 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'SnobalCRHM z_s_l')+1):(which(states_19 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  z_s_l[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'SnobalCRHM z_s_l')+1):(which(states_20 == 'SnobalCRHM z_s_l')+7)]), ' ')))
  
  
  Albedo[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'albedo_Richard Albedo')+1):(which(states_1 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'albedo_Richard Albedo')+1):(which(states_2 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'albedo_Richard Albedo')+1):(which(states_3 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'albedo_Richard Albedo')+1):(which(states_4 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'albedo_Richard Albedo')+1):(which(states_5 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'albedo_Richard Albedo')+1):(which(states_6 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'albedo_Richard Albedo')+1):(which(states_7 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'albedo_Richard Albedo')+1):(which(states_8 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'albedo_Richard Albedo')+1):(which(states_9 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'albedo_Richard Albedo')+1):(which(states_10 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'albedo_Richard Albedo')+1):(which(states_11 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'albedo_Richard Albedo')+1):(which(states_12 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'albedo_Richard Albedo')+1):(which(states_13 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'albedo_Richard Albedo')+1):(which(states_14 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'albedo_Richard Albedo')+1):(which(states_15 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'albedo_Richard Albedo')+1):(which(states_16 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'albedo_Richard Albedo')+1):(which(states_17 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'albedo_Richard Albedo')+1):(which(states_18 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'albedo_Richard Albedo')+1):(which(states_19 == 'albedo_Richard Albedo')+7)]), ' ')))
  Albedo[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'albedo_Richard Albedo')+1):(which(states_20 == 'albedo_Richard Albedo')+7)]), ' ')))
  
  
  firn[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier firn')+1):(which(states_1 == 'glacier firn')+7)]), ' ')))
  firn[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier firn')+1):(which(states_2 == 'glacier firn')+7)]), ' ')))
  firn[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier firn')+1):(which(states_3 == 'glacier firn')+7)]), ' ')))
  firn[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier firn')+1):(which(states_4 == 'glacier firn')+7)]), ' ')))
  firn[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier firn')+1):(which(states_5 == 'glacier firn')+7)]), ' ')))
  firn[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier firn')+1):(which(states_6 == 'glacier firn')+7)]), ' ')))
  firn[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier firn')+1):(which(states_7 == 'glacier firn')+7)]), ' ')))
  firn[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier firn')+1):(which(states_8 == 'glacier firn')+7)]), ' ')))
  firn[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier firn')+1):(which(states_9 == 'glacier firn')+7)]), ' ')))
  firn[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier firn')+1):(which(states_10 == 'glacier firn')+7)]), ' ')))
  firn[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier firn')+1):(which(states_11 == 'glacier firn')+7)]), ' ')))
  firn[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier firn')+1):(which(states_12 == 'glacier firn')+7)]), ' ')))
  firn[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier firn')+1):(which(states_13 == 'glacier firn')+7)]), ' ')))
  firn[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier firn')+1):(which(states_14 == 'glacier firn')+7)]), ' ')))
  firn[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier firn')+1):(which(states_15 == 'glacier firn')+7)]), ' ')))
  firn[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier firn')+1):(which(states_16 == 'glacier firn')+7)]), ' ')))
  firn[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier firn')+1):(which(states_17 == 'glacier firn')+7)]), ' ')))
  firn[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier firn')+1):(which(states_18 == 'glacier firn')+7)]), ' ')))
  firn[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier firn')+1):(which(states_19 == 'glacier firn')+7)]), ' ')))
  firn[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier firn')+1):(which(states_20 == 'glacier firn')+7)]), ' ')))
  
  
  firn_depth[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier firn_depth')+1):(which(states_1 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier firn_depth')+1):(which(states_2 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier firn_depth')+1):(which(states_3 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier firn_depth')+1):(which(states_4 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier firn_depth')+1):(which(states_5 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier firn_depth')+1):(which(states_6 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier firn_depth')+1):(which(states_7 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier firn_depth')+1):(which(states_8 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier firn_depth')+1):(which(states_9 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier firn_depth')+1):(which(states_10 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier firn_depth')+1):(which(states_11 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier firn_depth')+1):(which(states_12 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier firn_depth')+1):(which(states_13 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier firn_depth')+1):(which(states_14 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier firn_depth')+1):(which(states_15 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier firn_depth')+1):(which(states_16 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier firn_depth')+1):(which(states_17 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier firn_depth')+1):(which(states_18 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier firn_depth')+1):(which(states_19 == 'glacier firn_depth')+7)]), ' ')))
  firn_depth[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier firn_depth')+1):(which(states_20 == 'glacier firn_depth')+7)]), ' ')))
  
  
  firn_h[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier firn_h')+1):(which(states_1 == 'glacier firn_h')+70)]), ' ')))
  firn_h[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier firn_h')+1):(which(states_2 == 'glacier firn_h')+70)]), ' ')))
  firn_h[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier firn_h')+1):(which(states_3 == 'glacier firn_h')+70)]), ' ')))
  firn_h[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier firn_h')+1):(which(states_4 == 'glacier firn_h')+70)]), ' ')))
  firn_h[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier firn_h')+1):(which(states_5 == 'glacier firn_h')+70)]), ' ')))
  firn_h[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier firn_h')+1):(which(states_6 == 'glacier firn_h')+70)]), ' ')))
  firn_h[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier firn_h')+1):(which(states_7 == 'glacier firn_h')+70)]), ' ')))
  firn_h[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier firn_h')+1):(which(states_8 == 'glacier firn_h')+70)]), ' ')))
  firn_h[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier firn_h')+1):(which(states_9 == 'glacier firn_h')+70)]), ' ')))
  firn_h[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier firn_h')+1):(which(states_10 == 'glacier firn_h')+70)]), ' ')))
  firn_h[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier firn_h')+1):(which(states_11 == 'glacier firn_h')+70)]), ' ')))
  firn_h[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier firn_h')+1):(which(states_12 == 'glacier firn_h')+70)]), ' ')))
  firn_h[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier firn_h')+1):(which(states_13 == 'glacier firn_h')+70)]), ' ')))
  firn_h[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier firn_h')+1):(which(states_14 == 'glacier firn_h')+70)]), ' ')))
  firn_h[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier firn_h')+1):(which(states_15 == 'glacier firn_h')+70)]), ' ')))
  firn_h[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier firn_h')+1):(which(states_16 == 'glacier firn_h')+70)]), ' ')))
  firn_h[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier firn_h')+1):(which(states_17 == 'glacier firn_h')+70)]), ' ')))
  firn_h[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier firn_h')+1):(which(states_18 == 'glacier firn_h')+70)]), ' ')))
  firn_h[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier firn_h')+1):(which(states_19 == 'glacier firn_h')+70)]), ' ')))
  firn_h[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier firn_h')+1):(which(states_20 == 'glacier firn_h')+70)]), ' ')))
  
  
  firn_dens[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier firn_dens')+1):(which(states_1 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier firn_dens')+1):(which(states_2 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier firn_dens')+1):(which(states_3 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier firn_dens')+1):(which(states_4 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier firn_dens')+1):(which(states_5 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier firn_dens')+1):(which(states_6 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier firn_dens')+1):(which(states_7 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier firn_dens')+1):(which(states_8 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier firn_dens')+1):(which(states_9 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier firn_dens')+1):(which(states_10 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier firn_dens')+1):(which(states_11 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier firn_dens')+1):(which(states_12 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier firn_dens')+1):(which(states_13 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier firn_dens')+1):(which(states_14 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier firn_dens')+1):(which(states_15 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier firn_dens')+1):(which(states_16 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier firn_dens')+1):(which(states_17 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier firn_dens')+1):(which(states_18 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier firn_dens')+1):(which(states_19 == 'glacier firn_dens')+70)]), ' ')))
  firn_dens[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier firn_dens')+1):(which(states_20 == 'glacier firn_dens')+70)]), ' ')))
  
  
  ice[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier ice')+1):(which(states_1 == 'glacier ice')+7)]), ' ')))
  ice[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier ice')+1):(which(states_2 == 'glacier ice')+7)]), ' ')))
  ice[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier ice')+1):(which(states_3 == 'glacier ice')+7)]), ' ')))
  ice[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier ice')+1):(which(states_4 == 'glacier ice')+7)]), ' ')))
  ice[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier ice')+1):(which(states_5 == 'glacier ice')+7)]), ' ')))
  ice[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier ice')+1):(which(states_6 == 'glacier ice')+7)]), ' ')))
  ice[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier ice')+1):(which(states_7 == 'glacier ice')+7)]), ' ')))
  ice[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier ice')+1):(which(states_8 == 'glacier ice')+7)]), ' ')))
  ice[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier ice')+1):(which(states_9 == 'glacier ice')+7)]), ' ')))
  ice[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier ice')+1):(which(states_10 == 'glacier ice')+7)]), ' ')))
  ice[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier ice')+1):(which(states_11 == 'glacier ice')+7)]), ' ')))
  ice[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier ice')+1):(which(states_12 == 'glacier ice')+7)]), ' ')))
  ice[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier ice')+1):(which(states_13 == 'glacier ice')+7)]), ' ')))
  ice[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier ice')+1):(which(states_14 == 'glacier ice')+7)]), ' ')))
  ice[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier ice')+1):(which(states_15 == 'glacier ice')+7)]), ' ')))
  ice[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier ice')+1):(which(states_16 == 'glacier ice')+7)]), ' ')))
  ice[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier ice')+1):(which(states_17 == 'glacier ice')+7)]), ' ')))
  ice[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier ice')+1):(which(states_18 == 'glacier ice')+7)]), ' ')))
  ice[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier ice')+1):(which(states_19 == 'glacier ice')+7)]), ' ')))
  ice[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier ice')+1):(which(states_20 == 'glacier ice')+7)]), ' ')))
  
  
  nfirn[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'glacier nfirn')+1):(which(states_1 == 'glacier nfirn')+7)]), ' ')))
  nfirn[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'glacier nfirn')+1):(which(states_2 == 'glacier nfirn')+7)]), ' ')))
  nfirn[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'glacier nfirn')+1):(which(states_3 == 'glacier nfirn')+7)]), ' ')))
  nfirn[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'glacier nfirn')+1):(which(states_4 == 'glacier nfirn')+7)]), ' ')))
  nfirn[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'glacier nfirn')+1):(which(states_5 == 'glacier nfirn')+7)]), ' ')))
  nfirn[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'glacier nfirn')+1):(which(states_6 == 'glacier nfirn')+7)]), ' ')))
  nfirn[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'glacier nfirn')+1):(which(states_7 == 'glacier nfirn')+7)]), ' ')))
  nfirn[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'glacier nfirn')+1):(which(states_8 == 'glacier nfirn')+7)]), ' ')))
  nfirn[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'glacier nfirn')+1):(which(states_9 == 'glacier nfirn')+7)]), ' ')))
  nfirn[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'glacier nfirn')+1):(which(states_10 == 'glacier nfirn')+7)]), ' ')))
  nfirn[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'glacier nfirn')+1):(which(states_11 == 'glacier nfirn')+7)]), ' ')))
  nfirn[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'glacier nfirn')+1):(which(states_12 == 'glacier nfirn')+7)]), ' ')))
  nfirn[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'glacier nfirn')+1):(which(states_13 == 'glacier nfirn')+7)]), ' ')))
  nfirn[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'glacier nfirn')+1):(which(states_14 == 'glacier nfirn')+7)]), ' ')))
  nfirn[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'glacier nfirn')+1):(which(states_15 == 'glacier nfirn')+7)]), ' ')))
  nfirn[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'glacier nfirn')+1):(which(states_16 == 'glacier nfirn')+7)]), ' ')))
  nfirn[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'glacier nfirn')+1):(which(states_17 == 'glacier nfirn')+7)]), ' ')))
  nfirn[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'glacier nfirn')+1):(which(states_18 == 'glacier nfirn')+7)]), ' ')))
  nfirn[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'glacier nfirn')+1):(which(states_19 == 'glacier nfirn')+7)]), ' ')))
  nfirn[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'glacier nfirn')+1):(which(states_20 == 'glacier nfirn')+7)]), ' ')))
  
  
  #Create states data frame
  
  states_array <- array(data = NA, dim = c(20, 65, 7))
  
  
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
  
  albedo_rs <- read.csv('Path to remote sensing albedo/HRU_Snow_Albedo_Peyto.csv', header = F)
  
  albedo_rs <- ifelse(as.matrix(albedo_rs) == -9999, NA, as.matrix(albedo_rs))
  
  albedo_rs <- albedo_rs[-c(4,14,15,25,36,37),]
  
  albedo_obs_pert0 <- rnorm(20, 1, R)
  
  
  updated_states_array <- array(data = NA, dim = c(20,65,7))
  
  for (m in 1:65) {
    
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
  
  states_array_final <- abind(states_array_final, matrix(rep(0.0001, 1300), nrow = 20, ncol = 65))
  
  
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
  
  states_array_final <- abind(states_array_final, matrix(rep(NA, 1300), nrow = 20, ncol = 65))
  states_array_final <- abind(states_array_final, matrix(rep(NA, 1300), nrow = 20, ncol = 65))
  
  firn_h_matrix <- matrix(nrow = 20, ncol = 650)
  
  
  for (q in 1:20) {
    
    mean_firn_dens <- vector()
    
    for (o in 0:64) {
      
      mean_firn_dens[o+1] <- mean(c(firn_dens_i[q,1+o], firn_dens_i[q,66+o], firn_dens_i[q,131+o], firn_dens_i[q,196+o], firn_dens_i[q,261+o], 
                                    firn_dens_i[q,326+o], firn_dens_i[q,391+o], firn_dens_i[q,456+o], firn_dens_i[q,521+o], firn_dens_i[q,586+o]), na.rm = T)
      
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
    
    for (p in 0:64) {
      
      firn_depth_updt[p+1] <- (firn_h_final[1+p] + firn_h_final[66+p] + firn_h_final[131+p] + firn_h_final[196+p] + firn_h_final[261+p] + 
                                 firn_h_final[326+p] + firn_h_final[391+p] + firn_h_final[456+p] + firn_h_final[521+p] + firn_h_final[586+p])
      
    }
    
    
    states_array_final[q,,20] <- nfirn_updt
    
    states_array_final[q,,21] <- firn_depth_updt
    
    firn_h_matrix[q,] <- firn_h_final
    
  }
  
  
  
  for (n in 0:6) {
    
    if (n == 6) {
      
      states_1[514+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_1[244+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_1[280+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_1[307+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_1[379+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_1[703+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_1[856+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_1[370+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_1[334+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_1[361+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_1[388+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_1[397+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_1[343+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_1[352+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_1[253+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_1[262+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_1[289+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_1[298+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_1[316+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_1[865+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_1[703+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_2[514+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_2[244+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_2[280+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_2[307+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_2[379+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_2[703+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_2[856+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_2[370+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_2[334+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_2[361+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_2[388+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_2[397+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_2[343+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_2[352+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_2[253+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_2[262+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_2[289+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_2[298+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_2[316+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_2[865+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_2[703+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_3[514+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_3[244+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_3[280+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_3[307+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_3[379+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_3[703+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_3[856+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_3[370+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_3[334+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_3[361+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_3[388+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_3[397+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_3[343+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_3[352+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_3[253+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_3[262+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_3[289+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_3[298+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_3[316+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_3[865+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_3[703+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_4[514+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_4[244+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_4[280+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_4[307+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_4[379+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_4[703+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_4[856+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_4[370+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_4[334+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_4[361+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_4[388+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_4[397+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_4[343+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_4[352+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_4[253+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_4[262+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_4[289+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_4[298+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_4[316+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_4[865+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_4[703+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_5[514+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_5[244+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_5[280+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_5[307+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_5[379+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_5[703+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_5[856+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_5[370+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_5[334+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_5[361+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_5[388+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_5[397+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_5[343+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_5[352+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_5[253+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_5[262+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_5[289+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_5[298+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_5[316+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_5[865+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_5[703+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_6[514+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_6[244+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_6[280+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_6[307+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_6[379+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_6[703+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_6[856+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_6[370+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_6[334+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_6[361+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_6[388+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_6[397+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_6[343+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_6[352+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_6[253+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_6[262+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_6[289+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_6[298+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_6[316+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_6[865+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_6[703+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_7[514+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_7[244+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_7[280+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_7[307+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_7[379+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_7[703+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_7[856+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_7[370+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_7[334+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_7[361+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_7[388+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_7[397+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_7[343+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_7[352+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_7[253+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_7[262+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_7[289+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_7[298+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_7[316+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_7[865+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_7[703+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_8[514+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_8[244+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_8[280+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_8[307+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_8[379+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_8[703+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_8[856+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_8[370+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_8[334+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_8[361+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_8[388+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_8[397+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_8[343+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_8[352+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_8[253+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_8[262+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_8[289+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_8[298+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_8[316+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_8[865+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_8[703+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_9[514+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_9[244+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_9[280+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_9[307+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_9[379+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_9[703+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_9[856+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_9[370+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_9[334+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_9[361+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_9[388+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_9[397+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_9[343+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_9[352+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_9[253+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_9[262+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_9[289+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_9[298+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_9[316+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_9[865+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_9[703+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_10[514+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_10[244+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_10[280+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_10[307+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_10[379+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_10[703+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_10[856+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_10[370+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_10[334+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_10[361+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_10[388+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_10[397+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_10[343+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_10[352+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_10[253+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_10[262+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_10[289+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_10[298+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_10[316+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_10[865+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_10[703+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_11[514+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_11[244+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_11[280+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_11[307+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_11[379+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_11[703+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_11[856+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_11[370+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_11[334+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_11[361+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_11[388+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_11[397+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_11[343+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_11[352+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_11[253+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_11[262+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_11[289+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_11[298+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_11[316+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_11[865+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_11[703+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_12[514+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_12[244+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_12[280+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_12[307+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_12[379+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_12[703+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_12[856+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_12[370+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_12[334+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_12[361+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_12[388+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_12[397+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_12[343+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_12[352+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_12[253+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_12[262+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_12[289+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_12[298+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_12[316+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_12[865+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_12[703+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_13[514+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_13[244+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_13[280+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_13[307+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_13[379+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_13[703+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_13[856+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_13[370+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_13[334+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_13[361+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_13[388+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_13[397+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_13[343+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_13[352+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_13[253+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_13[262+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_13[289+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_13[298+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_13[316+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_13[865+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_13[703+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_14[514+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_14[244+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_14[280+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_14[307+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_14[379+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_14[703+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_14[856+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_14[370+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_14[334+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_14[361+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_14[388+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_14[397+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_14[343+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_14[352+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_14[253+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_14[262+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_14[289+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_14[298+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_14[316+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_14[865+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_14[703+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_15[514+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_15[244+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_15[280+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_15[307+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_15[379+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_15[703+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_15[856+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_15[370+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_15[334+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_15[361+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_15[388+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_15[397+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_15[343+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_15[352+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_15[253+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_15[262+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_15[289+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_15[298+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_15[316+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_15[865+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_15[703+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_16[514+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_16[244+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_16[280+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_16[307+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_16[379+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_16[703+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_16[856+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_16[370+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_16[334+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_16[361+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_16[388+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_16[397+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_16[343+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_16[352+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_16[253+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_16[262+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_16[289+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_16[298+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_16[316+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_16[865+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_16[703+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_17[514+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_17[244+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_17[280+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_17[307+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_17[379+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_17[703+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_17[856+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_17[370+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_17[334+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_17[361+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_17[388+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_17[397+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_17[343+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_17[352+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_17[253+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_17[262+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_17[289+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_17[298+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_17[316+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_17[865+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_17[703+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_18[514+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_18[244+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_18[280+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_18[307+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_18[379+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_18[703+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_18[856+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_18[370+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_18[334+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_18[361+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_18[388+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_18[397+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_18[343+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_18[352+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_18[253+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_18[262+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_18[289+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_18[298+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_18[316+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_18[865+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_18[703+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_19[514+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_19[244+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_19[280+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_19[307+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_19[379+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_19[703+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_19[856+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_19[370+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_19[334+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_19[361+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_19[388+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_19[397+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_19[343+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_19[352+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_19[253+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_19[262+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_19[289+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_19[298+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_19[316+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_19[865+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_19[703+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      states_20[514+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),1]), collapse = ' ')
      states_20[244+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),2]), collapse = ' ')
      states_20[280+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),3]), collapse = ' ')
      states_20[307+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),4]), collapse = ' ')
      states_20[379+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),5]), collapse = ' ')
      states_20[703+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),6]), collapse = ' ')
      states_20[856+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),7]), collapse = ' ')
      states_20[370+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),8]), collapse = ' ')
      states_20[334+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),9]), collapse = ' ')
      states_20[361+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),10]), collapse = ' ')
      states_20[388+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),11]), collapse = ' ')
      states_20[397+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),12]), collapse = ' ')
      states_20[343+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),13]), collapse = ' ')
      states_20[352+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),14]), collapse = ' ')
      states_20[253+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),15]), collapse = ' ')
      states_20[262+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),16]), collapse = ' ')
      states_20[289+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),17]), collapse = ' ')
      states_20[298+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),18]), collapse = ' ')
      states_20[316+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),19]), collapse = ' ')
      states_20[865+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),20]), collapse = ' ')
      states_20[703+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+5),21]), collapse = ' ')
      
      
    } else {
      
      states_1[514+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_1[244+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_1[280+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_1[307+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_1[379+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_1[703+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_1[856+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_1[370+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_1[334+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_1[361+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_1[388+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_1[397+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_1[343+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_1[352+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_1[253+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_1[262+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_1[289+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_1[298+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_1[316+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_1[865+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_1[703+n] <- paste(as.character(states_array_final[1,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_2[514+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_2[244+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_2[280+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_2[307+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_2[379+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_2[703+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_2[856+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_2[370+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_2[334+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_2[361+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_2[388+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_2[397+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_2[343+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_2[352+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_2[253+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_2[262+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_2[289+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_2[298+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_2[316+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_2[865+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_2[703+n] <- paste(as.character(states_array_final[2,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_3[514+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_3[244+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_3[280+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_3[307+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_3[379+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_3[703+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_3[856+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_3[370+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_3[334+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_3[361+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_3[388+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_3[397+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_3[343+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_3[352+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_3[253+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_3[262+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_3[289+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_3[298+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_3[316+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_3[865+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_3[703+n] <- paste(as.character(states_array_final[3,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_4[514+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_4[244+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_4[280+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_4[307+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_4[379+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_4[703+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_4[856+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_4[370+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_4[334+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_4[361+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_4[388+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_4[397+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_4[343+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_4[352+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_4[253+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_4[262+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_4[289+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_4[298+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_4[316+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_4[865+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_4[703+n] <- paste(as.character(states_array_final[4,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_5[514+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_5[244+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_5[280+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_5[307+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_5[379+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_5[703+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_5[856+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_5[370+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_5[334+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_5[361+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_5[388+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_5[397+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_5[343+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_5[352+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_5[253+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_5[262+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_5[289+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_5[298+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_5[316+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_5[865+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_5[703+n] <- paste(as.character(states_array_final[5,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_6[514+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_6[244+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_6[280+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_6[307+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_6[379+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_6[703+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_6[856+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_6[370+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_6[334+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_6[361+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_6[388+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_6[397+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_6[343+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_6[352+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_6[253+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_6[262+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_6[289+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_6[298+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_6[316+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_6[865+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_6[703+n] <- paste(as.character(states_array_final[6,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_7[514+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_7[244+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_7[280+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_7[307+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_7[379+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_7[703+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_7[856+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_7[370+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_7[334+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_7[361+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_7[388+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_7[397+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_7[343+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_7[352+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_7[253+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_7[262+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_7[289+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_7[298+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_7[316+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_7[865+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_7[703+n] <- paste(as.character(states_array_final[7,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_8[514+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_8[244+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_8[280+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_8[307+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_8[379+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_8[703+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_8[856+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_8[370+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_8[334+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_8[361+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_8[388+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_8[397+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_8[343+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_8[352+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_8[253+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_8[262+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_8[289+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_8[298+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_8[316+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_8[865+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_8[703+n] <- paste(as.character(states_array_final[8,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_9[514+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_9[244+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_9[280+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_9[307+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_9[379+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_9[703+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_9[856+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_9[370+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_9[334+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_9[361+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_9[388+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_9[397+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_9[343+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_9[352+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_9[253+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_9[262+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_9[289+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_9[298+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_9[316+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_9[865+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_9[703+n] <- paste(as.character(states_array_final[9,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_10[514+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_10[244+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_10[280+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_10[307+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_10[379+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_10[703+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_10[856+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_10[370+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_10[334+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_10[361+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_10[388+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_10[397+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_10[343+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_10[352+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_10[253+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_10[262+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_10[289+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_10[298+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_10[316+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_10[865+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_10[703+n] <- paste(as.character(states_array_final[10,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_11[514+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_11[244+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_11[280+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_11[307+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_11[379+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_11[703+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_11[856+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_11[370+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_11[334+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_11[361+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_11[388+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_11[397+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_11[343+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_11[352+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_11[253+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_11[262+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_11[289+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_11[298+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_11[316+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_11[865+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_11[703+n] <- paste(as.character(states_array_final[11,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_12[514+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_12[244+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_12[280+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_12[307+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_12[379+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_12[703+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_12[856+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_12[370+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_12[334+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_12[361+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_12[388+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_12[397+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_12[343+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_12[352+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_12[253+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_12[262+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_12[289+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_12[298+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_12[316+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_12[865+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_12[703+n] <- paste(as.character(states_array_final[12,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_13[514+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_13[244+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_13[280+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_13[307+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_13[379+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_13[703+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_13[856+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_13[370+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_13[334+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_13[361+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_13[388+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_13[397+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_13[343+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_13[352+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_13[253+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_13[262+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_13[289+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_13[298+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_13[316+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_13[865+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_13[703+n] <- paste(as.character(states_array_final[13,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_14[514+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_14[244+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_14[280+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_14[307+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_14[379+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_14[703+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_14[856+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_14[370+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_14[334+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_14[361+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_14[388+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_14[397+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_14[343+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_14[352+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_14[253+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_14[262+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_14[289+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_14[298+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_14[316+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_14[865+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_14[703+n] <- paste(as.character(states_array_final[14,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_15[514+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_15[244+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_15[280+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_15[307+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_15[379+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_15[703+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_15[856+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_15[370+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_15[334+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_15[361+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_15[388+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_15[397+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_15[343+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_15[352+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_15[253+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_15[262+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_15[289+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_15[298+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_15[316+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_15[865+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_15[703+n] <- paste(as.character(states_array_final[15,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_16[514+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_16[244+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_16[280+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_16[307+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_16[379+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_16[703+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_16[856+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_16[370+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_16[334+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_16[361+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_16[388+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_16[397+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_16[343+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_16[352+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_16[253+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_16[262+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_16[289+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_16[298+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_16[316+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_16[865+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_16[703+n] <- paste(as.character(states_array_final[16,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_17[514+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_17[244+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_17[280+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_17[307+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_17[379+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_17[703+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_17[856+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_17[370+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_17[334+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_17[361+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_17[388+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_17[397+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_17[343+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_17[352+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_17[253+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_17[262+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_17[289+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_17[298+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_17[316+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_17[865+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_17[703+n] <- paste(as.character(states_array_final[17,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_18[514+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_18[244+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_18[280+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_18[307+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_18[379+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_18[703+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_18[856+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_18[370+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_18[334+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_18[361+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_18[388+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_18[397+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_18[343+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_18[352+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_18[253+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_18[262+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_18[289+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_18[298+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_18[316+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_18[865+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_18[703+n] <- paste(as.character(states_array_final[18,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_19[514+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_19[244+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_19[280+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_19[307+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_19[379+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_19[703+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_19[856+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_19[370+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_19[334+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_19[361+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_19[388+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_19[397+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_19[343+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_19[352+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_19[253+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_19[262+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_19[289+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_19[298+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_19[316+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_19[865+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_19[703+n] <- paste(as.character(states_array_final[19,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
      states_20[514+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),1]), collapse = ' ')
      states_20[244+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),2]), collapse = ' ')
      states_20[280+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),3]), collapse = ' ')
      states_20[307+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),4]), collapse = ' ')
      states_20[379+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),5]), collapse = ' ')
      states_20[703+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),6]), collapse = ' ')
      states_20[856+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),7]), collapse = ' ')
      states_20[370+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),8]), collapse = ' ')
      states_20[334+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),9]), collapse = ' ')
      states_20[361+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),10]), collapse = ' ')
      states_20[388+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),11]), collapse = ' ')
      states_20[397+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),12]), collapse = ' ')
      states_20[343+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),13]), collapse = ' ')
      states_20[352+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),14]), collapse = ' ')
      states_20[253+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),15]), collapse = ' ')
      states_20[262+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),16]), collapse = ' ')
      states_20[289+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),17]), collapse = ' ')
      states_20[298+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),18]), collapse = ' ')
      states_20[316+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),19]), collapse = ' ')
      states_20[865+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),20]), collapse = ' ')
      states_20[703+n] <- paste(as.character(states_array_final[20,((10*n)+1):((10*n)+10),21]), collapse = ' ')
      
    }
    
  }
  
  
  
  firn_dens_updt <- t(matrix(rep(c(rep(450,65), rep(550,65), rep(620,65), rep(670,65), rep(710,65), rep(740,65), rep(755, 65), rep(780,65), rep(800,65), rep(820,65)), 20), nrow = 650, ncol = 20))
  
  firn_dens_updt <- ifelse(firn_h_matrix > 0, firn_dens_updt, 0)
  
  
  for (r in 0:64) {
    
    states_1[712+r] <- paste(as.character(firn_h_matrix[1,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_2[712+r] <- paste(as.character(firn_h_matrix[2,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_3[712+r] <- paste(as.character(firn_h_matrix[3,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_4[712+r] <- paste(as.character(firn_h_matrix[4,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_5[712+r] <- paste(as.character(firn_h_matrix[5,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_6[712+r] <- paste(as.character(firn_h_matrix[6,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_7[712+r] <- paste(as.character(firn_h_matrix[7,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_8[712+r] <- paste(as.character(firn_h_matrix[8,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_9[712+r] <- paste(as.character(firn_h_matrix[9,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_10[712+r] <- paste(as.character(firn_h_matrix[10,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_11[712+r] <- paste(as.character(firn_h_matrix[11,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_12[712+r] <- paste(as.character(firn_h_matrix[12,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_13[712+r] <- paste(as.character(firn_h_matrix[13,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_14[712+r] <- paste(as.character(firn_h_matrix[14,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_15[712+r] <- paste(as.character(firn_h_matrix[15,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_16[712+r] <- paste(as.character(firn_h_matrix[16,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_17[712+r] <- paste(as.character(firn_h_matrix[17,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_18[712+r] <- paste(as.character(firn_h_matrix[18,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_19[712+r] <- paste(as.character(firn_h_matrix[19,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_20[712+r] <- paste(as.character(firn_h_matrix[20,((10*r)+1):((10*r)+10)]), collapse = ' ')
    
    
    states_1[631+r] <- paste(as.character(firn_dens_updt[1,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_2[631+r] <- paste(as.character(firn_dens_updt[2,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_3[631+r] <- paste(as.character(firn_dens_updt[3,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_4[631+r] <- paste(as.character(firn_dens_updt[4,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_5[631+r] <- paste(as.character(firn_dens_updt[5,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_6[631+r] <- paste(as.character(firn_dens_updt[6,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_7[631+r] <- paste(as.character(firn_dens_updt[7,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_8[631+r] <- paste(as.character(firn_dens_updt[8,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_9[631+r] <- paste(as.character(firn_dens_updt[9,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_10[631+r] <- paste(as.character(firn_dens_updt[10,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_11[631+r] <- paste(as.character(firn_dens_updt[11,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_12[631+r] <- paste(as.character(firn_dens_updt[12,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_13[631+r] <- paste(as.character(firn_dens_updt[13,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_14[631+r] <- paste(as.character(firn_dens_updt[14,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_15[631+r] <- paste(as.character(firn_dens_updt[15,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_16[631+r] <- paste(as.character(firn_dens_updt[16,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_17[631+r] <- paste(as.character(firn_dens_updt[17,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_18[631+r] <- paste(as.character(firn_dens_updt[18,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_19[631+r] <- paste(as.character(firn_dens_updt[19,((10*r)+1):((10*r)+10)]), collapse = ' ')
    states_20[631+r] <- paste(as.character(firn_dens_updt[20,((10*r)+1):((10*r)+10)]), collapse = ' ')
    
  }
  
  
  #Retrieve previous albedos
  
  Albedo_p <- array(data = NA, dim = c(20,65,33))
  
  
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
      
      
      Albedo_p[1,,t] <- as.numeric(unlist(strsplit(c(states_p_1[(which(states_p_1 == 'albedo_Richard Albedo')+1):(which(states_p_1 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[2,,t] <- as.numeric(unlist(strsplit(c(states_p_2[(which(states_p_2 == 'albedo_Richard Albedo')+1):(which(states_p_2 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[3,,t] <- as.numeric(unlist(strsplit(c(states_p_3[(which(states_p_3 == 'albedo_Richard Albedo')+1):(which(states_p_3 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[4,,t] <- as.numeric(unlist(strsplit(c(states_p_4[(which(states_p_4 == 'albedo_Richard Albedo')+1):(which(states_p_4 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[5,,t] <- as.numeric(unlist(strsplit(c(states_p_5[(which(states_p_5 == 'albedo_Richard Albedo')+1):(which(states_p_5 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[6,,t] <- as.numeric(unlist(strsplit(c(states_p_6[(which(states_p_6 == 'albedo_Richard Albedo')+1):(which(states_p_6 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[7,,t] <- as.numeric(unlist(strsplit(c(states_p_7[(which(states_p_7 == 'albedo_Richard Albedo')+1):(which(states_p_7 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[8,,t] <- as.numeric(unlist(strsplit(c(states_p_8[(which(states_p_8 == 'albedo_Richard Albedo')+1):(which(states_p_8 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[9,,t] <- as.numeric(unlist(strsplit(c(states_p_9[(which(states_p_9 == 'albedo_Richard Albedo')+1):(which(states_p_9 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[10,,t] <- as.numeric(unlist(strsplit(c(states_p_10[(which(states_p_10 == 'albedo_Richard Albedo')+1):(which(states_p_10 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[11,,t] <- as.numeric(unlist(strsplit(c(states_p_11[(which(states_p_11 == 'albedo_Richard Albedo')+1):(which(states_p_11 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[12,,t] <- as.numeric(unlist(strsplit(c(states_p_12[(which(states_p_12 == 'albedo_Richard Albedo')+1):(which(states_p_12 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[13,,t] <- as.numeric(unlist(strsplit(c(states_p_13[(which(states_p_13 == 'albedo_Richard Albedo')+1):(which(states_p_13 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[14,,t] <- as.numeric(unlist(strsplit(c(states_p_14[(which(states_p_14 == 'albedo_Richard Albedo')+1):(which(states_p_14 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[15,,t] <- as.numeric(unlist(strsplit(c(states_p_15[(which(states_p_15 == 'albedo_Richard Albedo')+1):(which(states_p_15 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[16,,t] <- as.numeric(unlist(strsplit(c(states_p_16[(which(states_p_16 == 'albedo_Richard Albedo')+1):(which(states_p_16 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[17,,t] <- as.numeric(unlist(strsplit(c(states_p_17[(which(states_p_17 == 'albedo_Richard Albedo')+1):(which(states_p_17 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[18,,t] <- as.numeric(unlist(strsplit(c(states_p_18[(which(states_p_18 == 'albedo_Richard Albedo')+1):(which(states_p_18 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[19,,t] <- as.numeric(unlist(strsplit(c(states_p_19[(which(states_p_19 == 'albedo_Richard Albedo')+1):(which(states_p_19 == 'albedo_Richard Albedo')+7)]), ' ')))
      Albedo_p[20,,t] <- as.numeric(unlist(strsplit(c(states_p_20[(which(states_p_20 == 'albedo_Richard Albedo')+1):(which(states_p_20 == 'albedo_Richard Albedo')+7)]), ' ')))
      
      
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
    
    parameters[20] <- paste(as.character(initial_min_alb_final[1:10]), collapse = ' ')
    parameters[21] <- paste(as.character(initial_min_alb_final[11:20]), collapse = ' ')
    parameters[22] <- paste(as.character(initial_min_alb_final[21:30]), collapse = ' ')
    parameters[23] <- paste(as.character(initial_min_alb_final[31:40]), collapse = ' ')
    parameters[24] <- paste(as.character(initial_min_alb_final[41:50]), collapse = ' ')
    parameters[25] <- paste(as.character(initial_min_alb_final[51:60]), collapse = ' ')
    parameters[26] <- paste(as.character(initial_min_alb_final[61:65]), collapse = ' ')
    
    
    parameters[482] <- paste(as.character(initial_min_alb_final[1:10]), collapse = ' ')
    parameters[483] <- paste(as.character(initial_min_alb_final[11:20]), collapse = ' ')
    parameters[484] <- paste(as.character(initial_min_alb_final[21:30]), collapse = ' ')
    parameters[485] <- paste(as.character(initial_min_alb_final[31:40]), collapse = ' ')
    parameters[486] <- paste(as.character(initial_min_alb_final[41:50]), collapse = ' ')
    parameters[487] <- paste(as.character(initial_min_alb_final[51:60]), collapse = ' ')
    parameters[488] <- paste(as.character(initial_min_alb_final[61:65]), collapse = ' ')
    
    
    parameters[664] <- paste(as.character(initial_min_alb_final[1:10]), collapse = ' ')
    parameters[665] <- paste(as.character(initial_min_alb_final[11:20]), collapse = ' ')
    parameters[666] <- paste(as.character(initial_min_alb_final[21:30]), collapse = ' ')
    parameters[667] <- paste(as.character(initial_min_alb_final[31:40]), collapse = ' ')
    parameters[668] <- paste(as.character(initial_min_alb_final[41:50]), collapse = ' ')
    parameters[669] <- paste(as.character(initial_min_alb_final[51:60]), collapse = ' ')
    parameters[670] <- paste(as.character(initial_min_alb_final[61:65]), collapse = ' ')
    
    
    writeLines(parameters, paste0('Parameters', s, '.par'))
    
  }
  
  
}

