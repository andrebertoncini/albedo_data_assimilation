library(MASS)

setwd('C:/Users/alb818/Dropbox/PHD/DATA_ASSIMILATION/17_DA_Athabasca/05_Obs_Ensemble_20230205')

orig_obs <- read.table('AthabascaGlacierMoraine_1Oct2014_30Sept2021_merge_hourly_t,rh,u,Qsi,Qli,p.obs', sep = '', dec = '.', skip = 9)


ta_orig <- orig_obs$V6[8760:61368]
rh_orig <- orig_obs$V7[8760:61368]
u_orig <- orig_obs$V8[8760:61368]
sw_orig <- orig_obs$V9[8760:61368]
lw_orig <- orig_obs$V10[8760:61368]
p_orig <- orig_obs$V11[8760:61368]

cor_matrix <- matrix(c(1, round(cor(ta_orig, rh_orig),2), round(cor(ta_orig, sw_orig),2), round(cor(ta_orig, lw_orig),2), round(cor(ta_orig, p_orig),2),
                       round(cor(rh_orig, ta_orig),2), 1, round(cor(rh_orig, sw_orig),2), round(cor(rh_orig, lw_orig),2), round(cor(rh_orig, p_orig),2),
                       round(cor(sw_orig, ta_orig),2), round(cor(sw_orig, rh_orig),2), 1, round(cor(sw_orig, lw_orig),2), round(cor(sw_orig, p_orig),2),
                       round(cor(lw_orig, ta_orig),2), round(cor(lw_orig, rh_orig),2), round(cor(lw_orig, sw_orig),2), 1, round(cor(lw_orig, p_orig),2),
                       round(cor(p_orig, ta_orig),2), round(cor(p_orig, rh_orig),2), round(cor(p_orig, sw_orig),2), round(cor(p_orig, lw_orig),2), 1), ncol = 5, nrow = 5)   


multivariate_dists <- mvrnorm(n = 20, mu = c(0, 0, 0, 0, 0), Sigma = cor_matrix)

cdf_1 <- pnorm(multivariate_dists[,1], 0, 1)
cdf_2 <- pnorm(multivariate_dists[,2], 0, 1)
cdf_3 <- pnorm(multivariate_dists[,3], 0, 1)
cdf_4 <- pnorm(multivariate_dists[,4], 0, 1)
cdf_5 <- pnorm(multivariate_dists[,5], 0, 1)

ta_pert <- 2.5*qunif(cdf_1, min = -1, max = 1)
rh_pert <- 5*qunif(cdf_2, min = -1, max = 1)
sw_pert <- 1 + (0.15*qunif(cdf_3, min = -1, max = 1))
lw_pert <- 25*qunif(cdf_4, min = -1, max = 1)
p_pert <- 1 + (0.5*qunif(cdf_5, min = -1, max = 1))


ta_1 <- ta_orig + ta_pert[1]
ta_2 <- ta_orig + ta_pert[2]
ta_3 <- ta_orig + ta_pert[3]
ta_4 <- ta_orig + ta_pert[4]
ta_5 <- ta_orig + ta_pert[5]
ta_6 <- ta_orig + ta_pert[6]
ta_7 <- ta_orig + ta_pert[7]
ta_8 <- ta_orig + ta_pert[8]
ta_9 <- ta_orig + ta_pert[9]
ta_10 <- ta_orig + ta_pert[10]
ta_11 <- ta_orig + ta_pert[11]
ta_12 <- ta_orig + ta_pert[12]
ta_13 <- ta_orig + ta_pert[13]
ta_14 <- ta_orig + ta_pert[14]
ta_15 <- ta_orig + ta_pert[15]
ta_16 <- ta_orig + ta_pert[16]
ta_17 <- ta_orig + ta_pert[17]
ta_18 <- ta_orig + ta_pert[18]
ta_19 <- ta_orig + ta_pert[19]
ta_20 <- ta_orig + ta_pert[20]


rh_1 <- ifelse(rh_orig + rh_pert[1] > 100, 100, ifelse(rh_orig + rh_pert[1] < 10, 10, rh_orig + rh_pert[1]))
rh_2 <- ifelse(rh_orig + rh_pert[2] > 100, 100, ifelse(rh_orig + rh_pert[2] < 10, 10, rh_orig + rh_pert[2]))
rh_3 <- ifelse(rh_orig + rh_pert[3] > 100, 100, ifelse(rh_orig + rh_pert[3] < 10, 10, rh_orig + rh_pert[3]))
rh_4 <- ifelse(rh_orig + rh_pert[4] > 100, 100, ifelse(rh_orig + rh_pert[4] < 10, 10, rh_orig + rh_pert[4]))
rh_5 <- ifelse(rh_orig + rh_pert[5] > 100, 100, ifelse(rh_orig + rh_pert[5] < 10, 10, rh_orig + rh_pert[5]))
rh_6 <- ifelse(rh_orig + rh_pert[6] > 100, 100, ifelse(rh_orig + rh_pert[6] < 10, 10, rh_orig + rh_pert[6]))
rh_7 <- ifelse(rh_orig + rh_pert[7] > 100, 100, ifelse(rh_orig + rh_pert[7] < 10, 10, rh_orig + rh_pert[7]))
rh_8 <- ifelse(rh_orig + rh_pert[8] > 100, 100, ifelse(rh_orig + rh_pert[8] < 10, 10, rh_orig + rh_pert[8]))
rh_9 <- ifelse(rh_orig + rh_pert[9] > 100, 100, ifelse(rh_orig + rh_pert[9] < 10, 10, rh_orig + rh_pert[9]))
rh_10 <- ifelse(rh_orig + rh_pert[10] > 100, 100, ifelse(rh_orig + rh_pert[10] < 10, 10, rh_orig + rh_pert[10]))
rh_11 <- ifelse(rh_orig + rh_pert[11] > 100, 100, ifelse(rh_orig + rh_pert[11] < 10, 10, rh_orig + rh_pert[11]))
rh_12 <- ifelse(rh_orig + rh_pert[12] > 100, 100, ifelse(rh_orig + rh_pert[12] < 10, 10, rh_orig + rh_pert[12]))
rh_13 <- ifelse(rh_orig + rh_pert[13] > 100, 100, ifelse(rh_orig + rh_pert[13] < 10, 10, rh_orig + rh_pert[13]))
rh_14 <- ifelse(rh_orig + rh_pert[14] > 100, 100, ifelse(rh_orig + rh_pert[14] < 10, 10, rh_orig + rh_pert[14]))
rh_15 <- ifelse(rh_orig + rh_pert[15] > 100, 100, ifelse(rh_orig + rh_pert[15] < 10, 10, rh_orig + rh_pert[15]))
rh_16 <- ifelse(rh_orig + rh_pert[16] > 100, 100, ifelse(rh_orig + rh_pert[16] < 10, 10, rh_orig + rh_pert[16]))
rh_17 <- ifelse(rh_orig + rh_pert[17] > 100, 100, ifelse(rh_orig + rh_pert[17] < 10, 10, rh_orig + rh_pert[17]))
rh_18 <- ifelse(rh_orig + rh_pert[18] > 100, 100, ifelse(rh_orig + rh_pert[18] < 10, 10, rh_orig + rh_pert[18]))
rh_19 <- ifelse(rh_orig + rh_pert[19] > 100, 100, ifelse(rh_orig + rh_pert[19] < 10, 10, rh_orig + rh_pert[19]))
rh_20 <- ifelse(rh_orig + rh_pert[20] > 100, 100, ifelse(rh_orig + rh_pert[20] < 10, 10, rh_orig + rh_pert[20]))


u_pert <- rnorm(20, 0, 2)

u_1 <- ifelse(u_orig + u_pert[1] < 0, 0, u_orig + u_pert[1])
u_2 <- ifelse(u_orig + u_pert[2] < 0, 0, u_orig + u_pert[2])
u_3 <- ifelse(u_orig + u_pert[3] < 0, 0, u_orig + u_pert[3])
u_4 <- ifelse(u_orig + u_pert[4] < 0, 0, u_orig + u_pert[4])
u_5 <- ifelse(u_orig + u_pert[5] < 0, 0, u_orig + u_pert[5])
u_6 <- ifelse(u_orig + u_pert[6] < 0, 0, u_orig + u_pert[6])
u_7 <- ifelse(u_orig + u_pert[7] < 0, 0, u_orig + u_pert[7])
u_8 <- ifelse(u_orig + u_pert[8] < 0, 0, u_orig + u_pert[8])
u_9 <- ifelse(u_orig + u_pert[9] < 0, 0, u_orig + u_pert[9])
u_10 <- ifelse(u_orig + u_pert[10] < 0, 0, u_orig + u_pert[10])
u_11 <- ifelse(u_orig + u_pert[11] < 0, 0, u_orig + u_pert[11])
u_12 <- ifelse(u_orig + u_pert[12] < 0, 0, u_orig + u_pert[12])
u_13 <- ifelse(u_orig + u_pert[13] < 0, 0, u_orig + u_pert[13])
u_14 <- ifelse(u_orig + u_pert[14] < 0, 0, u_orig + u_pert[14])
u_15 <- ifelse(u_orig + u_pert[15] < 0, 0, u_orig + u_pert[15])
u_16 <- ifelse(u_orig + u_pert[16] < 0, 0, u_orig + u_pert[16])
u_17 <- ifelse(u_orig + u_pert[17] < 0, 0, u_orig + u_pert[17])
u_18 <- ifelse(u_orig + u_pert[18] < 0, 0, u_orig + u_pert[18])
u_19 <- ifelse(u_orig + u_pert[19] < 0, 0, u_orig + u_pert[19])
u_20 <- ifelse(u_orig + u_pert[20] < 0, 0, u_orig + u_pert[20])


lw_1 <- ifelse(lw_orig + lw_pert[1] < 0, 0, lw_orig + lw_pert[1])
lw_2 <- ifelse(lw_orig + lw_pert[2] < 0, 0, lw_orig + lw_pert[2])
lw_3 <- ifelse(lw_orig + lw_pert[3] < 0, 0, lw_orig + lw_pert[3])
lw_4 <- ifelse(lw_orig + lw_pert[4] < 0, 0, lw_orig + lw_pert[4])
lw_5 <- ifelse(lw_orig + lw_pert[5] < 0, 0, lw_orig + lw_pert[5])
lw_6 <- ifelse(lw_orig + lw_pert[6] < 0, 0, lw_orig + lw_pert[6])
lw_7 <- ifelse(lw_orig + lw_pert[7] < 0, 0, lw_orig + lw_pert[7])
lw_8 <- ifelse(lw_orig + lw_pert[8] < 0, 0, lw_orig + lw_pert[8])
lw_9 <- ifelse(lw_orig + lw_pert[9] < 0, 0, lw_orig + lw_pert[9])
lw_10 <- ifelse(lw_orig + lw_pert[10] < 0, 0, lw_orig + lw_pert[10])
lw_11 <- ifelse(lw_orig + lw_pert[11] < 0, 0, lw_orig + lw_pert[11])
lw_12 <- ifelse(lw_orig + lw_pert[12] < 0, 0, lw_orig + lw_pert[12])
lw_13 <- ifelse(lw_orig + lw_pert[13] < 0, 0, lw_orig + lw_pert[13])
lw_14 <- ifelse(lw_orig + lw_pert[14] < 0, 0, lw_orig + lw_pert[14])
lw_15 <- ifelse(lw_orig + lw_pert[15] < 0, 0, lw_orig + lw_pert[15])
lw_16 <- ifelse(lw_orig + lw_pert[16] < 0, 0, lw_orig + lw_pert[16])
lw_17 <- ifelse(lw_orig + lw_pert[17] < 0, 0, lw_orig + lw_pert[17])
lw_18 <- ifelse(lw_orig + lw_pert[18] < 0, 0, lw_orig + lw_pert[18])
lw_19 <- ifelse(lw_orig + lw_pert[19] < 0, 0, lw_orig + lw_pert[19])
lw_20 <- ifelse(lw_orig + lw_pert[20] < 0, 0, lw_orig + lw_pert[20])


sw_1 <- ifelse(sw_orig*sw_pert[1] < 0, 0, sw_orig*sw_pert[1])
sw_2 <- ifelse(sw_orig*sw_pert[2] < 0, 0, sw_orig*sw_pert[2])
sw_3 <- ifelse(sw_orig*sw_pert[3] < 0, 0, sw_orig*sw_pert[3])
sw_4 <- ifelse(sw_orig*sw_pert[4] < 0, 0, sw_orig*sw_pert[4])
sw_5 <- ifelse(sw_orig*sw_pert[5] < 0, 0, sw_orig*sw_pert[5])
sw_6 <- ifelse(sw_orig*sw_pert[6] < 0, 0, sw_orig*sw_pert[6])
sw_7 <- ifelse(sw_orig*sw_pert[7] < 0, 0, sw_orig*sw_pert[7])
sw_8 <- ifelse(sw_orig*sw_pert[8] < 0, 0, sw_orig*sw_pert[8])
sw_9 <- ifelse(sw_orig*sw_pert[9] < 0, 0, sw_orig*sw_pert[9])
sw_10 <- ifelse(sw_orig*sw_pert[10] < 0, 0, sw_orig*sw_pert[10])
sw_11 <- ifelse(sw_orig*sw_pert[11] < 0, 0, sw_orig*sw_pert[11])
sw_12 <- ifelse(sw_orig*sw_pert[12] < 0, 0, sw_orig*sw_pert[12])
sw_13 <- ifelse(sw_orig*sw_pert[13] < 0, 0, sw_orig*sw_pert[13])
sw_14 <- ifelse(sw_orig*sw_pert[14] < 0, 0, sw_orig*sw_pert[14])
sw_15 <- ifelse(sw_orig*sw_pert[15] < 0, 0, sw_orig*sw_pert[15])
sw_16 <- ifelse(sw_orig*sw_pert[16] < 0, 0, sw_orig*sw_pert[16])
sw_17 <- ifelse(sw_orig*sw_pert[17] < 0, 0, sw_orig*sw_pert[17])
sw_18 <- ifelse(sw_orig*sw_pert[18] < 0, 0, sw_orig*sw_pert[18])
sw_19 <- ifelse(sw_orig*sw_pert[19] < 0, 0, sw_orig*sw_pert[19])
sw_20 <- ifelse(sw_orig*sw_pert[20] < 0, 0, sw_orig*sw_pert[20])


p_1 <- ifelse(p_orig*p_pert[1] < 0, 0, p_orig*p_pert[1])
p_2 <- ifelse(p_orig*p_pert[2] < 0, 0, p_orig*p_pert[2])
p_3 <- ifelse(p_orig*p_pert[3] < 0, 0, p_orig*p_pert[3])
p_4 <- ifelse(p_orig*p_pert[4] < 0, 0, p_orig*p_pert[4])
p_5 <- ifelse(p_orig*p_pert[5] < 0, 0, p_orig*p_pert[5])
p_6 <- ifelse(p_orig*p_pert[6] < 0, 0, p_orig*p_pert[6])
p_7 <- ifelse(p_orig*p_pert[7] < 0, 0, p_orig*p_pert[7])
p_8 <- ifelse(p_orig*p_pert[8] < 0, 0, p_orig*p_pert[8])
p_9 <- ifelse(p_orig*p_pert[9] < 0, 0, p_orig*p_pert[9])
p_10 <- ifelse(p_orig*p_pert[10] < 0, 0, p_orig*p_pert[10])
p_11 <- ifelse(p_orig*p_pert[11] < 0, 0, p_orig*p_pert[11])
p_12 <- ifelse(p_orig*p_pert[12] < 0, 0, p_orig*p_pert[12])
p_13 <- ifelse(p_orig*p_pert[13] < 0, 0, p_orig*p_pert[13])
p_14 <- ifelse(p_orig*p_pert[14] < 0, 0, p_orig*p_pert[14])
p_15 <- ifelse(p_orig*p_pert[15] < 0, 0, p_orig*p_pert[15])
p_16 <- ifelse(p_orig*p_pert[16] < 0, 0, p_orig*p_pert[16])
p_17 <- ifelse(p_orig*p_pert[17] < 0, 0, p_orig*p_pert[17])
p_18 <- ifelse(p_orig*p_pert[18] < 0, 0, p_orig*p_pert[18])
p_19 <- ifelse(p_orig*p_pert[19] < 0, 0, p_orig*p_pert[19])
p_20 <- ifelse(p_orig*p_pert[20] < 0, 0, p_orig*p_pert[20])



#Save ensembles

en_01 <- data.frame(orig_obs[8760:61368,1:5], ta_1, rh_1, u_1, sw_1, lw_1, p_1)
en_02 <- data.frame(orig_obs[8760:61368,1:5], ta_2, rh_2, u_2, sw_2, lw_2, p_2)
en_03 <- data.frame(orig_obs[8760:61368,1:5], ta_3, rh_3, u_3, sw_3, lw_3, p_3)
en_04 <- data.frame(orig_obs[8760:61368,1:5], ta_4, rh_4, u_4, sw_4, lw_4, p_4)
en_05 <- data.frame(orig_obs[8760:61368,1:5], ta_5, rh_5, u_5, sw_5, lw_5, p_5)
en_06 <- data.frame(orig_obs[8760:61368,1:5], ta_6, rh_6, u_6, sw_6, lw_6, p_6)
en_07 <- data.frame(orig_obs[8760:61368,1:5], ta_7, rh_7, u_7, sw_7, lw_7, p_7)
en_08 <- data.frame(orig_obs[8760:61368,1:5], ta_8, rh_8, u_8, sw_8, lw_8, p_8)
en_09 <- data.frame(orig_obs[8760:61368,1:5], ta_9, rh_9, u_9, sw_9, lw_9, p_9)
en_10 <- data.frame(orig_obs[8760:61368,1:5], ta_10, rh_10, u_10, sw_10, lw_10, p_10)
en_11 <- data.frame(orig_obs[8760:61368,1:5], ta_11, rh_11, u_11, sw_11, lw_11, p_11)
en_12 <- data.frame(orig_obs[8760:61368,1:5], ta_12, rh_12, u_12, sw_12, lw_12, p_12)
en_13 <- data.frame(orig_obs[8760:61368,1:5], ta_13, rh_13, u_13, sw_13, lw_13, p_13)
en_14 <- data.frame(orig_obs[8760:61368,1:5], ta_14, rh_14, u_14, sw_14, lw_14, p_14)
en_15 <- data.frame(orig_obs[8760:61368,1:5], ta_15, rh_15, u_15, sw_15, lw_15, p_15)
en_16 <- data.frame(orig_obs[8760:61368,1:5], ta_16, rh_16, u_16, sw_16, lw_16, p_16)
en_17 <- data.frame(orig_obs[8760:61368,1:5], ta_17, rh_17, u_17, sw_17, lw_17, p_17)
en_18 <- data.frame(orig_obs[8760:61368,1:5], ta_18, rh_18, u_18, sw_18, lw_18, p_18)
en_19 <- data.frame(orig_obs[8760:61368,1:5], ta_19, rh_19, u_19, sw_19, lw_19, p_19)
en_20 <- data.frame(orig_obs[8760:61368,1:5], ta_20, rh_20, u_20, sw_20, lw_20, p_20)


write.table(en_01, file = "en_1.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_02, file = "en_2.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_03, file = "en_3.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_04, file = "en_4.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_05, file = "en_5.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_06, file = "en_6.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_07, file = "en_7.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_08, file = "en_8.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_09, file = "en_9.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_10, file = "en_10.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_11, file = "en_11.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_12, file = "en_12.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_13, file = "en_13.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_14, file = "en_14.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_15, file = "en_15.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_16, file = "en_16.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_17, file = "en_17.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_18, file = "en_18.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_19, file = "en_19.obs", sep = "\t", row.names = F, col.names = F)
write.table(en_20, file = "en_20.obs", sep = "\t", row.names = F, col.names = F)

