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
parameters <- c(beta = 0.375, gamma = 0.25) #0.50, 0.375, 0.875 
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
  measured_2 <- rnorm(n = 1, mean = log10(data_waste$wastewater[i]), sd = 0.5)
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
b2 <- b1*4

size <- 3 #the number of analyzed samples
threshold <- size*0.8 #threshold to selecting data on quantified concentration 
c2 <- log(a1/(1-a1))/(log10(b1)-log10(b2))
c1 <- -c2*log10(b2) 

c_test <- 0.95
c095 <- (log(c_test/(1-c_test))-c1)/c2 #LOD (95% detection)


p3 <- c()
for(i in 1:nrow(data_sim_final_4)){
  A <- data_sim_final_4$logWW[i]
  p1 <- 1/(1 + exp(-c1 - c2*A))
  p2 <- rbinom(1, size = size, prob = p1)
  p3 <- c(p3, p2) 
}

data_test <- data.frame(ND = p3)
data_sim_final_5 <- cbind(data_sim_final_4, data_test)

data_sim_final_6 <- data_sim_final_5[20:150,]#250
colnames(data_sim_final_6) <- c("time", "incidence_2", "wastewater","measured_ww", "logWW", "ND")
data_sim_final_6 <- data_sim_final_6 %>% mutate(logWW_rep = if_else(ND >= threshold, logWW, log10(b2)))
#tail(data_sim_final_6, 20)
#write.csv(x = data_sim_final_6, file = "C:/WBE_ND/data_fig2_a_b.csv")