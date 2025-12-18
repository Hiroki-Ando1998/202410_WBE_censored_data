setwd("C:/WBE_ND")

library(deSolve)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(rstan)
library(stats)

data_arti <- read.csv("WBE_ND_SIR_Initialdata_used.csv")

# SIRモデル関数を定義する(βの値が途中で変わる)
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # tが100未満の場合、100から150の間、150以上の場合でbetaの値を変更する
    if (time < 35) {
      beta_value <- beta
    } else if (time >= 35 & time < 60) {
      beta_value <- beta/1 #3
    } else {
      beta_value <- beta*1 #2
    }
    
    dS <- -beta_value * S * I
    dI <- beta_value * S * I - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}


# 初期値
population <- 100000
A <- 1.652467229/population
initial_state <- c(S = 1 - A, I = A, R = 0)

# パラメータ
parameters <- c(beta = 0.750, gamma = 0.25) #0.50, 0.375(0.25), 0.875 
times <- seq(0, 250, by = 1)

# モデルを解く
output <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)

data_sim <- data.frame(output)


#新規感染者数に変換
incidence <- NULL
incidence <- A
run <- nrow(data_sim)

for(i in 1:run-1){
  n_incidence <- data_sim$S[i] - data_sim$S[i+1]
  incidence <- c(incidence, n_incidence) 
}

data_sim_incidence <- data.frame(incidence)
data_sim_2 <- cbind(data_sim, incidence)
data_sim_2 <- data_sim_2 %>% mutate(incidence_2 = incidence*population)
#data_sim_2 <- data_sim_2 %>% mutate(incidence_2 = round(incidence_2))


time <- data.frame(Time_2 = seq(nrow(data_arti), nrow(data_arti) + nrow(data_sim_2)-1, by = 1))
data_sim_3_A <- cbind(data_sim_2, time) 

data_sim_3 <- rbind(data_arti, data_sim_3_A)


#下水濃度に変換
data_shedding <- read.csv("WBE_ND_Assumed_shedding.csv")
wastewater <- numeric()
run <- nrow(data_sim_3)-1
for(i in 26:run){
  n_wastewater_2 <-0
  for(m in 1:25){
    n_wastewater_1 <- data_sim_3$incidence_2[i-m]*data_shedding$rapid[m] #rapid, middle, prolong, super_rapid
    n_wastewater_2 <- n_wastewater_2 + n_wastewater_1
  }
  wastewater <- c(wastewater, n_wastewater_2) 
}
data_waste <- data.frame(wastewater)


#測定誤差を組み込む
measured_ww <- c(0)
run_2 <- nrow(data_waste)
for(i in 2:run_2){
  measured_2 <- rnorm(n = 1, mean = log10(data_waste$wastewater[i]), sd = 0.5/sqrt(size))
  measured_ww <- c(measured_ww, measured_2) 
}

data_measured_ww <- data.frame(measured_ww)
data_waste_2 <- cbind(data_waste, data_measured_ww)

#データの統合
zeros_1 <- rep(0,23)
zeros_1_b <- rep(0,25)
zeros_2 <- rep(0,1)
data_b_2 <- data.frame(wastewater = zeros_1_b)
data_b_2_ww <- data.frame(measured_ww = zeros_1_b)
data_b_2_2 <- cbind(data_b_2, data_b_2_ww)

data_b_3 <- data.frame(wastewater = zeros_2)
data_b_3_ww <- data.frame(measured_ww = zeros_2)
data_b_3_2 <- cbind(data_b_3, data_b_3_ww)

data_b_3 <- data.frame(wastewater = zeros_2)
data_waste_3 <- rbind(data_b_2_2, data_waste_2, data_b_3_2)

data_sim_final <- cbind(data_sim_3, data_waste_3)
data_sim_final <- data_sim_final[26:430, ]
time_2 <- data.frame(time_3 = seq(0, nrow(data_sim_final)-1, by = 1))
data_sim_final <- cbind(data_sim_final, time_2) 

#Data_file
data_sim_final_2 <- data_sim_final %>% select(time_3, incidence_2, wastewater, measured_ww)
data_sim_final_3 <- data_sim_final_2 %>% filter(incidence_2 >= 0.4 & wastewater > 1)
data_sim_final_4 <- data_sim_final_3 %>% mutate(logWW = log10(wastewater))








#10% detection probability at 100 copies (a1, b1)
#50% detection probability at 500 copies (a2, b2)
a1 <- 0.10
b1 <- 300
a2 <- 0.5
b2 <- 1200

c2 <- log(a1/(1-a1))/(log10(b1)-log10(b2))
c1 <- -c2*log10(b2) 

c_test <- 0.10
c095 <- (log(c_test/(1-c_test))-c1)/c2 #LOD (95% detection)

size <- 3 #the number of analyzed samples
p3 <- c()
for(i in 1:nrow(data_sim_final_4)){
  A <- data_sim_final_4$logWW[i]
  p1 <- 1/(1 + exp(-c1 - c2*A))
  p2 <- rbinom(1, size = size, prob = p1)
  p3 <- c(p3, p2) 
}

data_test <- data.frame(ND = p3)
data_sim_final_5 <- cbind(data_sim_final_4, data_test)

data_sim_final_6_1 <- data_sim_final_5[20:250,] #250, 150, 140, 128
colnames(data_sim_final_6_1) <- c("time", "incidence_2", "wastewater","measured_ww", "logWW", "ND")
data_sim_final_6 <- data_sim_final_6_1 %>% mutate(substituion = ((logWW*ND+log10(b2)*(3-ND))/3))



#Rstan modeling
#7 day sampling
data_stan <- data_sim_final_6
#write.csv(x = data_stan, file = "C:/WBE_ND/20251215_WBE_ND_SIR_analyzed_data.csv")
sample_size <- nrow(data_stan)

threshold <- size*0.8 #threshold to selecting data on quantified concentration 

data_row_CD <- data.frame(true = which(!is.na(data_stan$ND))) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)
data_row_D <- data.frame(detected = which(data_stan$ND >= threshold)) #pick row numbers of detected data
sample_size_D <- nrow(data_row_D)

data_ww <- data_stan %>% filter(ND >= threshold)
ww <- data_ww$logWW #logWW or measured_ww
print(1-nrow(data_ww)/nrow(data_stan))
data_nd <- data_stan %>% drop_na(ND)
ND <- data_nd$ND

data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
                     ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$detected, size = size)

mcmc_7 <- stan(
  file = "20240809_stat_space_binomial.stan", #"20240809_stat_space_bernoulli.stan"
  data = data_list_ww,
  seed = 1,
  chain = 1,
  iter = 10000,
  warmup = 4000,
  thin = 1
)

print(mcmc_7, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))

mcmc_sample <- rstan::extract(mcmc_7)
state_name <- "mu" #状態の名前：stan fileを参照すること
result_7 <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_7) <- c("low_7", "med_7", "upr_7")


# #3-day sampling
# data_stan_3 <- data_sim_final_6
# data_stan_3[data_stan_3$time %% 7 == 1|data_stan_3$time %% 7 == 3|data_stan_3$time %% 7 == 5 |data_stan_3$time %% 7 == 6, ] <- NA
# data_stan <- data_stan_3
# 
# sample_size <- nrow(data_stan)
# data_row_CD <- data.frame(true = which(!is.na(data_stan$ND))) #pick row numbers for censored data
# sample_size_CD <- nrow(data_row_CD)
# data_row_D <- data.frame(detected = which(data_stan$ND >= threshold)) #pick row numbers of detected data
# sample_size_D <- nrow(data_row_D)
# 
# data_ww <- data_stan %>% filter(ND >= threshold)
# ww <- data_ww$logWW #logWW or measured_ww
# data_nd <- data_stan %>% drop_na(ND)
# ND <- data_nd$ND
# 
# data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
#                      ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$detected, size = size)
# 
# mcmc_3 <- stan(
#   file = "20240809_stat_space_binomial.stan", #"20240809_stat_space_bernoulli.stan"
#   data = data_list_ww,
#   seed = 1,
#   chain = 1,
#   iter = 10000,
#   warmup = 4000,
#   thin = 1
# )
# 
# print(mcmc_3, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))
# 
# mcmc_sample <- rstan::extract(mcmc_3)
# state_name <- "mu" #状態の名前：stan fileを参照すること
# result_3 <- data.frame(t(apply(
#   X = mcmc_sample[[state_name]],
#   MARGIN = 2,
#   FUN = quantile,
#   probs = c(0.025, 0.5, 0.975)
# )))
# 
# colnames(result_3) <- c("low_3", "med_3", "upr_3")
# 
# #1-day sampling
# data_stan_1 <- data_sim_final_6
# data_stan_1[data_stan_1$time %% 7 == 1|data_stan_1$time %% 7 == 2
#             |data_stan_1$time %% 7 == 3 |data_stan_1$time %% 7 == 4 
#             |data_stan_1$time %% 7 == 5 |data_stan_1$time %% 7 == 6,] <- NA
# data_stan <- data_stan_1
# 
# sample_size <- nrow(data_stan)
# data_row_CD <- data.frame(true = which(!is.na(data_stan$ND))) #pick row numbers for censored data
# sample_size_CD <- nrow(data_row_CD)
# data_row_D <- data.frame(detected = which(data_stan$ND >= threshold)) #pick row numbers of detected data
# sample_size_D <- nrow(data_row_D)
# 
# data_ww <- data_stan %>% filter(ND >= threshold)
# ww <- data_ww$logWW #logWW or measured_ww
# data_nd <- data_stan %>% drop_na(ND)
# ND <- data_nd$ND
# 
# data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
#                      ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$detected, size = size)
# 
# mcmc_1 <- stan(
#   file = "20240809_stat_space_binomial.stan", #"20240809_stat_space_bernoulli.stan"
#   data = data_list_ww,
#   seed = 1,
#   chain = 1,
#   iter = 10000,
#   warmup = 4000,
#   thin = 1
# )
# 
# print(mcmc_1, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))
# 
# mcmc_sample <- rstan::extract(mcmc_1)
# state_name <- "mu" #状態の名前：stan fileを参照すること
# result_1 <- data.frame(t(apply(
#   X = mcmc_sample[[state_name]],
#   MARGIN = 2,
#   FUN = quantile,
#   probs = c(0.025, 0.5, 0.975)
# )))
# 
# colnames(result_1) <- c("low_1", "med_1", "upr_1")


#calculation of Mrb
data_stan_true <- data_sim_final_6 %>% select(time, logWW, ND)
data_result_analyzed <- cbind(data_stan_true, result_7)
#write.csv(x = data_result_analyzed, file = "C:/WBE_ND/WBE_ND_SIR_analyzed_result_logis.csv")


data_cal_1 <- data_result_analyzed %>% filter(logWW <= log10(b2)) #or log10(b2)
data_cal_2 <- data_cal_1 %>% mutate(error_7 = abs(logWW - med_7)/logWW, error_3 = abs(logWW - med_3)/logWW, error_1 = abs(logWW - med_1)/logWW)
data_cal_3 <- data_cal_2 %>% mutate(abs_7 = abs(logWW - med_7), abs_3 = abs(logWW - med_3), abs_1 = abs(logWW - med_1))
mrb_7 <- sum(data_cal_2$error_7)/nrow(data_cal_2)
mrb_3 <- sum(data_cal_2$error_3)/nrow(data_cal_2)
mrb_1 <- sum(data_cal_2$error_1)/nrow(data_cal_2)

abs_7 <- sum(data_cal_3$abs_7)/nrow(data_cal_3)
abs_3 <- sum(data_cal_3$abs_3)/nrow(data_cal_3)
abs_1 <- sum(data_cal_3$abs_1)/nrow(data_cal_3)

print(mrb_7)
print(mrb_3)
print(mrb_1)

print(abs_7)
print(abs_3)
print(abs_1)






plot <- ggplot(data_result_analyzed, aes(x = time))
plot <- plot + geom_hline(yintercept = log10(1200), colour = "black", linetype = "dashed")
plot <- plot + geom_ribbon(aes(ymin = low_7, ymax = upr_7),colour = "NA", alpha = 0.5, fill = "#74A9CF") + geom_line(aes(y = med_7), color = "#0570B0", size = 1.5)
#plot <- plot + geom_line(aes(y = med_7_b), color = "#8C96C6", size = 1.5) + geom_line(aes(y = med_7_c), color = "#3690C0", size = 1.5)
plot <- plot + geom_point(aes(y = logWW, shape = factor(ND)), color = "#0570B0", fill = "white",  size = 1.4)
plot <- plot + scale_shape_manual(values = c(4, 25, 24, 21))
plot <- plot + labs(x = "day", y = "Wasteater concentration")
plot <- plot + scale_y_continuous(limits = c(0.5,6.5), breaks = seq(0.5, 6.5, 1.0))
plot <- plot + theme_classic()
plot <- plot + theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.line = element_line(size = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot <- plot + guides(fill = guide_legend(title = NULL))
plot














