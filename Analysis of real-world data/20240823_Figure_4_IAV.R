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
data_IAV <- data %>% select(collection_date, Count_IAV, Positive_IAV, Concentration_IAV, Concentration_sub_IAV)
colnames(data_IAV) <- c("date", "count", "positive", "concentration", "substitution")

threshold <- 0.8 #threshold to selecting data on quantified concentration 


#state-spce model with logistic
data_stan <- data_IAV
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
  file = "20240809_stat_space_binomial_real_data.stan", 
  data = data_list_ww,
  seed = 1,
  chain = 4,
  iter = 400000,
  warmup = 200000,
  thin = 4
)

print(mcmc, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))

library(bayesplot)
#width: 500, hight: 600
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
data_concentration <- cbind(data_IAV, result_7_a)

#write.csv(x = data_concentration, file = "C:/wastewater_reproduction_number/20240901_IAV_CA.csv")


data_ef <- data.frame(mcmc_sample["mu"])

# Effective reproduction number
gamma_function <- function(x, shape, rate) {
  return((rate^shape) * (x^(shape - 1)) * exp(-rate * x) / gamma(shape))
}
gamma_probability <- function(lower, upper, shape, rate) {
  integrate(gamma_function, lower = lower, upper = upper, shape = shape, rate = rate)$value
}
shape <- 3  # ガンマ関数の形状パラメータ
rate <- 1   # ガンマ関数のレートパラメータ
prob <- NULL
for(i in 0:7){
  probability <- gamma_probability(i, i + 1, shape, rate)
  prob <- c(prob, probability) 
}
gamma_prob <- data.frame(prob)

sample_size_2 <- nrow(data_ef)
summary_2_A <- NULL
summary_2_B <- NULL
summary_2_C <- NULL
for (i in 8:nrow(data_stan)){
  p_estimate_2 <- NULL   
  for(m in 1:sample_size_2){
    IAV <- 10^(data_ef[m,i]) #m行目i列目
    IAV_1 <- 10^(data_ef[m,i-1])
    IAV_2 <- 10^(data_ef[m,i-2])
    IAV_3 <- 10^(data_ef[m,i-3])
    IAV_4 <- 10^(data_ef[m,i-4])
    IAV_5 <- 10^(data_ef[m,i-5])
    IAV_6 <- 10^(data_ef[m,i-6])
    IAV_7 <- 10^(data_ef[m,i-7])
    
    p_estimate <- IAV/(IAV_1*gamma_prob$prob[1] + IAV_2*gamma_prob$prob[2] + IAV_3*gamma_prob$prob[3] +
                         IAV_4*gamma_prob$prob[4] + IAV_5*gamma_prob$prob[5] + IAV_6*gamma_prob$prob[6] + IAV_7*gamma_prob$prob[7])
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

data_Date <- data_stan[8:nrow(data_stan),] %>% select(date)
data_eff_fig <- cbind(data_Date, data_result)
#write.csv(x = data_eff_fig, file = "C:/2023_R/20240901_IAV_CA_Re.csv")






#logistic function
mcmc_sample <- rstan::extract(mcmc)
data_ef_c1 <- data.frame(mcmc_sample["c1"])
data_ef_c2 <- data.frame(mcmc_sample["c2"])

ww <- 2 * 1.1^(0:(140-1))
data_ww <- data.frame(ww = ww)
sample_size_2 <- nrow(data_ef_c1)
summary_2_A <- NULL
summary_2_B <- NULL
summary_2_C <- NULL
for (i in 1:nrow(data_ww)){
  p_estimate_2 <- NULL   
  for(m in 1:sample_size_2){
    c1_1 <- data_ef_c1[m,1] #m行目i列目
    c2_1 <- data_ef_c2[m,1]
    
    p_estimate <- 1 / (1 + exp(-c1_1 -c2_1 * log10(data_ww$ww[i])))
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

data_fig_logis <- cbind(data_ww, data_result)
#write.csv(x = data_fig_logis, file = "C:/wastewater_reproduction_number/20240901_IAV_CA_logistic.csv")



plot <- ggplot(data_fig_logis, aes(x = log10(ww))) 
plot <- plot + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#3690C0", alpha = 0.3) 
plot <- plot + geom_line(aes(y = median), color = "#0570B0", size = 1) 
plot <- plot + labs(x = "wastewater concentration", y = "detection probability")
plot <- plot + theme_classic()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot



