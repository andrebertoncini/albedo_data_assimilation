#Code to plot states from CRHM outputs for Athabasca Basin

setwd("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/02_Runs/Run_20")


alb_ice <- matrix(nrow = 20, ncol = 68)
alb_snow <- matrix(nrow = 20, ncol = 68)
cc_s_ice <- matrix(nrow = 20, ncol = 68)
cc_s_snow <- matrix(nrow = 20, ncol = 68)
h2o_ice <- matrix(nrow = 20, ncol = 68)
h2o_snow <- matrix(nrow = 20, ncol = 68)
z_s_ice <- matrix(nrow = 20, ncol = 68)
z_s_snow <- matrix(nrow = 20, ncol = 68)
firn_ice <- matrix(nrow = 20, ncol = 68)
firn_snow <- matrix(nrow = 20, ncol = 68)
ice_ice <- matrix(nrow = 20, ncol = 68)
ice_snow <- matrix(nrow = 20, ncol = 68)
swe_ice <- matrix(nrow = 20, ncol = 68)
swe_snow <- matrix(nrow = 20, ncol = 68)


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


albedo <- matrix(nrow = 20, ncol = 90)

albedo[1,] <- as.numeric(unlist(strsplit(c(states_1[(which(states_1 == 'albedo_Richard Albedo')+1):(which(states_1 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[2,] <- as.numeric(unlist(strsplit(c(states_2[(which(states_2 == 'albedo_Richard Albedo')+1):(which(states_2 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[3,] <- as.numeric(unlist(strsplit(c(states_3[(which(states_3 == 'albedo_Richard Albedo')+1):(which(states_3 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[4,] <- as.numeric(unlist(strsplit(c(states_4[(which(states_4 == 'albedo_Richard Albedo')+1):(which(states_4 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[5,] <- as.numeric(unlist(strsplit(c(states_5[(which(states_5 == 'albedo_Richard Albedo')+1):(which(states_5 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[6,] <- as.numeric(unlist(strsplit(c(states_6[(which(states_6 == 'albedo_Richard Albedo')+1):(which(states_6 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[7,] <- as.numeric(unlist(strsplit(c(states_7[(which(states_7 == 'albedo_Richard Albedo')+1):(which(states_7 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[8,] <- as.numeric(unlist(strsplit(c(states_8[(which(states_8 == 'albedo_Richard Albedo')+1):(which(states_8 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[9,] <- as.numeric(unlist(strsplit(c(states_9[(which(states_9 == 'albedo_Richard Albedo')+1):(which(states_9 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[10,] <- as.numeric(unlist(strsplit(c(states_10[(which(states_10 == 'albedo_Richard Albedo')+1):(which(states_10 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[11,] <- as.numeric(unlist(strsplit(c(states_11[(which(states_11 == 'albedo_Richard Albedo')+1):(which(states_11 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[12,] <- as.numeric(unlist(strsplit(c(states_12[(which(states_12 == 'albedo_Richard Albedo')+1):(which(states_12 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[13,] <- as.numeric(unlist(strsplit(c(states_13[(which(states_13 == 'albedo_Richard Albedo')+1):(which(states_13 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[14,] <- as.numeric(unlist(strsplit(c(states_14[(which(states_14 == 'albedo_Richard Albedo')+1):(which(states_14 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[15,] <- as.numeric(unlist(strsplit(c(states_15[(which(states_15 == 'albedo_Richard Albedo')+1):(which(states_15 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[16,] <- as.numeric(unlist(strsplit(c(states_16[(which(states_16 == 'albedo_Richard Albedo')+1):(which(states_16 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[17,] <- as.numeric(unlist(strsplit(c(states_17[(which(states_17 == 'albedo_Richard Albedo')+1):(which(states_17 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[18,] <- as.numeric(unlist(strsplit(c(states_18[(which(states_18 == 'albedo_Richard Albedo')+1):(which(states_18 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[19,] <- as.numeric(unlist(strsplit(c(states_19[(which(states_19 == 'albedo_Richard Albedo')+1):(which(states_19 == 'albedo_Richard Albedo')+9)]), ' ')))
albedo[20,] <- as.numeric(unlist(strsplit(c(states_20[(which(states_20 == 'albedo_Richard Albedo')+1):(which(states_20 == 'albedo_Richard Albedo')+9)]), ' ')))


alb_ice[,j-1] <- albedo[,15]
alb_snow[,j-1] <- albedo[,2]


cc_s <- matrix(nrow = 20, ncol = 90)

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

cc_s_ice[,j-1] <- cc_s[,15]
cc_s_snow[,j-1] <- cc_s[,2]


h2o <- matrix(nrow = 20, ncol = 90)

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

h2o_ice[,j-1] <- h2o[,15]
h2o_snow[,j-1] <- h2o[,2]


z_s <- matrix(nrow = 20, ncol = 90)

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

z_s_ice[,j-1] <- z_s[,15]
z_s_snow[,j-1] <- z_s[,2]


firn <- matrix(nrow = 20, ncol = 90)

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

firn_ice[,j-1] <- firn[,15]
firn_snow[,j-1] <- firn[,2]


ice <- matrix(nrow = 20, ncol = 90)

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

ice_ice[,j-1] <- ice[,15]
ice_snow[,j-1] <- ice[,2]


SWE <- matrix(nrow = 20, ncol = 90)

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

swe_ice[,j-1] <- SWE[,15]
swe_snow[,j-1] <- SWE[,2]

}


par(mfrow=c(5,2))
par(mar=c(3,3,1,1))

plot(colMeans(alb_ice), type = 'l', col = "blue", ylim = c(0.15, 0.9))
lines(alb_ctrl_ice, col = "red")

plot(colMeans(alb_snow), type = 'l', col = "blue", ylim = c(0.50, 0.9))
lines(alb_ctrl_snow, col = "red")

plot(colMeans(swe_ice), type = 'l', col = "blue", ylim = c(0, 120))
lines(swe_ctrl_ice, col = "red")

plot(colMeans(swe_snow), type = 'l', col = "blue", ylim = c(0, 1400))
lines(swe_ctrl_snow, col = "red")

plot(colMeans(cc_s_ice), type = 'l', col = "blue", ylim = c(-1.2e6, 0))
lines(cc_s_ctrl_ice, col = "red")

plot(colMeans(cc_s_snow), type = 'l', col = "blue")
lines(cc_s_ctrl_snow, col = "red")

plot(colMeans(h2o_ice), type = 'l', col = "blue")
lines(h2o_ctrl_ice, col = "red")

plot(colMeans(h2o_snow), type = 'l', col = "blue")
lines(h2o_ctrl_snow, col = "red")

plot(colMeans(ice_ice)-colMeans(ice_ice)[1], type = 'l', col = "blue")
lines(ice_ctrl_ice - ice_ctrl_ice[1], col = "red")

plot(colMeans(firn_snow-colMeans(firn_snow)[1]), type = 'l', col = "blue", ylim = c(-3500, 320))
lines(firn_ctrl_snow - firn_ctrl_snow[1], col = "red")


#Read control

ctrl_file <- read.table("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/06_StatePlots/Control_9/CRHM_output_2022204.txt")

exclusion_dates <- c(11:13, 27:29, 31:37, 47:50, 67:73, 90:91)

dates_rs <- read.csv('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/04_RS_Albedo/Dates_Athabasca_20230415.csv', header = F)[-c(exclusion_dates),]

dates_hourly <- as.POSIXct(paste0(dates_rs, " 01:00:00"), format = '%Y-%m-%d %H:%M:%S')

dates_full <- seq(as.POSIXct('2015-10-01 01:00:00', format = '%Y-%m-%d %H:%M:%S'), as.POSIXct('2021-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 'hours')


alb_ctrl_snow <- as.numeric(ctrl_file$V2[-c(1,2)])[which(dates_full %in% dates_hourly)]
alb_ctrl_ice <- as.numeric(ctrl_file$V3[-c(1,2)])[which(dates_full %in% dates_hourly)]
swe_ctrl_snow <- as.numeric(ctrl_file$V4[-c(1,2)])[which(dates_full %in% dates_hourly)]
swe_ctrl_ice <- as.numeric(ctrl_file$V5[-c(1,2)])[which(dates_full %in% dates_hourly)]
cc_s_ctrl_snow <- as.numeric(ctrl_file$V6[-c(1,2)])[which(dates_full %in% dates_hourly)]
cc_s_ctrl_ice <- as.numeric(ctrl_file$V7[-c(1,2)])[which(dates_full %in% dates_hourly)]
h2o_ctrl_snow <- as.numeric(ctrl_file$V8[-c(1,2)])[which(dates_full %in% dates_hourly)]
h2o_ctrl_ice <- as.numeric(ctrl_file$V9[-c(1,2)])[which(dates_full %in% dates_hourly)]
firn_ctrl_snow <- as.numeric(ctrl_file$V10[-c(1,2)])[which(dates_full %in% dates_hourly)]
ice_ctrl_ice <- as.numeric(ctrl_file$V11[-c(1,2)])[which(dates_full %in% dates_hourly)]



#Generate hourly states

library(lubridate)
library(abind)


setwd('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/06_StatePlots/Run_20')

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



#Plotting

setwd('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/06_StatePlots/Run_20')

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


#Streamflow ensembles

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

ctrl_file <- read.table("C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/06_StatePlots/Control_9/CRHM_output_2022204.txt")

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


tiff("C:/Users/alb818/Dropbox/PHD/THESIS/04_DATA_ASSIMILATION/01_Plots_20231026/Athabasca_SnowIce_20231028.tiff",
     width = 6, height = 4, units = "in", res = 500, pointsize = 12)

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
