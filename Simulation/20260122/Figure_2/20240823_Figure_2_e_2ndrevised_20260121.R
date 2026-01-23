setwd("C:/WBE_ND")

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(rstan)
library(stats)
library(slider)

data <- read.csv("WBE_ND_fig_2_e_2.csv")
threshold <- 1.0 #threshold to selecting data on quantified concentration


#10% detection probability at 100 copies (a1, b1)
#50% detection probability at 500 copies (a2, b2)
a1 <- 0.10
b1 <- 300
a2 <- 0.5
b2 <- 1200

c2 <- log(a1/(1-a1))/(log10(b1)-log10(b2))
c1 <- -c2*log10(b2) 

c_test <- 0.95
c095 <- (log(c_test/(1-c_test))-c1)/c2 #LOD (95% detection)
size <- 8 #the number of analyzed samples



#-----------------------------------------------------------------------------------------------Simulation
data_result_xx_2 <- NULL
for (r in 1:20){
set.seed(100+r)
p3 <- c()
for(i in 1:nrow(data)){
  A <- data$logWW[i]
  p1 <- 1/(1 + exp(-c1 - c2*A))
  p2 <- rbinom(1, size = size, prob = p1)
  p3 <- c(p3, p2) 
}

data_test <- data.frame(ND_2 = p3)
data_2 <- cbind(data, data_test)


#------------------------------------------------------------------------------------------Baysian analysis
#data_sim_final_6 <- read.csv("WBE_ND_fig_2_e_analysis.csv")
data_sim_final_6 <- data_2
 
m1 <- 77
m2 <- nrow(data_2)
data_result_xx <- NULL
  
for (k in m1:m2){
#7-day sampling
data_stan_3 <- data_sim_final_6[1:k,]
#data_stan_3[data_stan_3$time %% 7 == 1|data_stan_3$time %% 7 == 3|data_stan_3$time %% 7 == 5 |data_stan_3$time %% 7 == 6, ] <- NA
data_stan <- data_stan_3

sample_size <- nrow(data_stan)
data_row_CD <- data.frame(true = which(!is.na(data_stan$ND_2))) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)
data_row_D <- data.frame(detected = which(data_stan$ND_2 >= threshold*3)) #pick row numbers of detected data
sample_size_D <- nrow(data_row_D)

data_ww <- data_stan %>% filter(ND_2 >= threshold*3)
ww <- data_ww$logWW
data_nd <- data_stan %>% drop_na(ND_2)
ND <- data_nd$ND_2

data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
                     ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$detected, size = 8)

mcmc_3 <- stan(
  file = "20260121_stat_space_binomial.stan", #"20240809_stat_space_bernoulli.stan"
  data = data_list_ww,
  seed = 1,
  chain = 1,
  iter = 10000,
  warmup = 4000,
  thin = 1
)

#print(mcmc_3, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))

mcmc_sample <- rstan::extract(mcmc_3)
state_name <- "mu" #状態の名前：stan fileを参照すること
result_3_a <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_3_a) <- c("low_3_a", "med_3_a", "upr_3_a")

data_result_1 <- cbind(data_stan, result_3_a)
data_result_2 <- data_result_1[k,]
data_result_xx <- rbind(data_result_xx, data_result_2)
}

data_result_xx_2 <- rbind(data_result_xx_2, data_result_xx)
}

data_result_2 <- mutate(data_result_xx_2, error = abs(logWW - med_3_a))


#------------------------------------------------------------------------------------------------------------------------------Data aggregation
data_xx_1 <- read.csv("20260122_Figure2_1.csv") #result: r:1:5 (Line 33)
data_xx_2 <- read.csv("20260122_Figure2_2.csv") #result: r:6:10
data_xx_3 <- read.csv("20260122_Figure2_3.csv") #result: r:11:15
data_xx_4 <- read.csv("20260122_Figure2_4.csv") #result: r:16:20

data_xx_result <- NULL

#-------------------------------------------------
data_xx <- data_xx_4
for(k in 1:5){
  start <-  1+55*(k-1)
  end <- 55*k
  data_xx_a <- data_xx[start:end,]
  #data_xx_a <- data_xx[1:55,]
  
  data_xx_b <- data_xx_a %>% mutate(
    ND_total = slide_dbl(
      ND_2,
      ~ sum(.x, na.rm = TRUE),
      .before = 6,
      .after  = -1,
      .complete = TRUE
    )
  )
  
  data_xx_c <- data_xx_b[7:nrow(data_xx_b),]
  data_xx_result <- rbind(data_xx_result, data_xx_c)
}

write.csv(data_xx_result, file = "C:/WBE_ND/20260122_Figure2e_result_estimationerror_siumaltionanalysis.csv")




#-----------------------------------------------------------------------------------------------------------------------Data analysis
data_result <- read.csv("20260122_Figure2e_result_estimationerror_siumaltionanalysis.csv")
data_result <- data_result %>% select(time, logWW, ND, ND_2, low_3_a, med_3_a, upr_3_a, error, ND_total)


data_result_0 <- data_result %>% filter(ND_2 >= 1, ND_total < 1)
data_result_1 <- data_result %>% filter(ND_2 >= 1, ND_total >= 1)
data_result_2 <- data_result %>% filter(ND_2 >= 1, ND_total >= 2)

mean(data_result_0$error)
median(data_result_0$error)

mean(data_result_1$error)
median(data_result_1$error)

mean(data_result_2$error)
median(data_result_2$error)


#--------------------
error_size <- 0.20
data_result_0_a <- data_result_0 %>% filter(error > error_size)
data_result_1_a <- data_result_1 %>% filter(error > error_size)
data_result_2_a <- data_result_2 %>% filter(error > error_size)

nrow(data_result_0_a)/nrow(data_result_0)*100
nrow(data_result_1_a)/nrow(data_result_1)*100
nrow(data_result_2_a)/nrow(data_result_2)*100

