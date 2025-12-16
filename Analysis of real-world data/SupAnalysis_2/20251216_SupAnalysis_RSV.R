setwd("C:/WBE_ND")

library(deSolve)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(rstan)
library(stats)

data_CA <- read.csv("20240820_CA.csv")
data_CO <- read.csv("20240820_CO.csv")
data_FL <- read.csv("20240820_FL.csv")

data <- data_CO
data_RSV <- data %>% select(collection_date, Count_RSV, Positive_RSV, Concentration_RSV, Concentration_sub_RSV)
colnames(data_RSV) <- c("date", "count", "positive", "concentration", "substitution")

threshold <- 0.8 #threshold to selecting data on quantified concentration 

#state-spce model with logistic
data_stan <- data_RSV
data_stan <- data_stan %>% mutate(positive_rate = positive/count)
sample_size <- nrow(data_stan)

#vector of row number used for the analysis
data_row_D <- data.frame(true = which((data_stan$positive_rate >= threshold))) #pick row numbers for censored data
sample_size_D <- nrow(data_row_D)
data_row_CD <- data.frame(true = which(data_stan$count > 0)) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)

#vector of concentration and positive data for the analysis
data_ww <- data_stan %>% filter(positive_rate >= threshold)
ww <- data_ww$concentration
print(1-nrow(data_ww)/nrow(data_stan))
data_nd <- data_stan %>% filter(count > 0)
ND <- data_nd$positive
count <- data_nd$count

data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
                     ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$true, n_count = count)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mcmc <- stan(
  file = "20240809_stat_space_binomial_real_data_CA.stan", 
  data = data_list_ww,
  seed = 1,
  chain = 4,
  iter = 200000,
  warmup = 10000,
  thin = 4
)

print(mcmc, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))

library(bayesplot)
mcmc_combo(mcmc, pars = c("c1", "c2", "s1", "s2"))


mcmc_sample <- rstan::extract(mcmc)
state_name <- "mu"
result_7_a <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_7_a) <- c("low_7_a", "med_7_a", "upr_7_a")
data_concentration <- cbind(data_RSV, result_7_a)

#write.csv(x = data_concentration, file = "C:/wastewater_reproduction_number/20251215_RSV_CA.csv")

data_ef <- data.frame(mcmc_sample["mu"])


# Effective reproduction number
gamma_function <- function(x, shape, rate) {
  return((rate^shape) * (x^(shape - 1)) * exp(-rate * x) / gamma(shape))
}
gamma_probability <- function(lower, upper, shape, rate) {
  integrate(gamma_function, lower = lower, upper = upper, shape = shape, rate = rate)$value
}
shape <- 7  # ガンマ関数の形状パラメータ
rate <- 1   # ガンマ関数のレートパラメータ
prob <- NULL
for(i in 0:14){
  probability <- gamma_probability(i, i + 1, shape, rate)
  prob <- c(prob, probability) 
}
gamma_prob <- data.frame(prob)

sample_size_2 <- nrow(data_ef)
summary_2_A <- NULL
summary_2_B <- NULL
summary_2_C <- NULL
for (i in 15:nrow(data_stan)){
  p_estimate_2 <- NULL   
  for(m in 1:sample_size_2){
    RSV <- 10^(data_ef[m,i]) #m行目i列目
    RSV_3 <- 10^(data_ef[m,i-3])
    RSV_4 <- 10^(data_ef[m,i-4])
    RSV_5 <- 10^(data_ef[m,i-5])
    RSV_6 <- 10^(data_ef[m,i-6])
    RSV_7 <- 10^(data_ef[m,i-7])
    RSV_8 <- 10^(data_ef[m,i-8])
    RSV_9 <- 10^(data_ef[m,i-9])
    RSV_10 <- 10^(data_ef[m,i-10])
    RSV_11 <- 10^(data_ef[m,i-11])
    RSV_12 <- 10^(data_ef[m,i-12])
    RSV_13 <- 10^(data_ef[m,i-13])
    RSV_14 <- 10^(data_ef[m,i-14])
    
    p_estimate <- RSV/(RSV_3*gamma_prob$prob[3] + RSV_4*gamma_prob$prob[4] + RSV_5*gamma_prob$prob[5] + RSV_6*gamma_prob$prob[6] + RSV_7*gamma_prob$prob[7]
                      + RSV_8*gamma_prob$prob[8] + RSV_9*gamma_prob$prob[9] + RSV_10*gamma_prob$prob[10] + RSV_11*gamma_prob$prob[11] + RSV_12*gamma_prob$prob[12]
                      + RSV_13*gamma_prob$prob[13] + RSV_14*gamma_prob$prob[14])
    p_estimate_2 <- c(p_estimate_2, p_estimate)
  }
  summary <- quantile(p_estimate_2, probs = c(0.025, 0.5, 0.975))
  summary_A <- quantile(p_estimate_2, probs = c(0.025))
  summary_B <- quantile(p_estimate_2, probs = c(0.5))
  summary_C <- quantile(p_estimate_2, probs = c(0.975))
  
  summary_2_A <- c(summary_2_A, summary_A)
  summary_2_B <- c(summary_2_B, summary_B)
  summary_2_C <- c(summary_2_C, summary_C)
}
data_result <- data.frame(A = summary_2_A, B = summary_2_B, C = summary_2_C)
colnames(data_result) <- c("lower", "median", "upper")

data_Date <- data_stan[15:nrow(data_stan),] %>% select(date)
data_eff_fig <- cbind(data_Date, data_result)
#write.csv(x = data_eff_fig, file = "C:/2023_R/20251215_RSV_CA_Re.csv")






