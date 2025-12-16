setwd("C:/WBE_ND")

library(deSolve)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(rstan)
library(stats)

data_sim_final_6 <- read.csv("data_fig2_a_b.csv")
data_sim_final_6_1 <- data_sim_final_6 %>% mutate(substituion = ((logWW*ND+log10(b2)*(3-ND))/3))



#state-spce model with logistic
data_stan <- data_sim_final_6_1
#write.csv(x = data_stan, file = "C:/WBE_ND/WBE_ND_SIR_analyzed_data.csv")
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

mcmc_7_a <- stan(
  file = "20240809_stat_space_binomial.stan", 
  data = data_list_ww,
  seed = 1,
  chain = 1,
  iter = 15000,
  warmup = 4000,
  thin = 1
)

print(mcmc_7_a, pars = c("s1", "s2"), probe = c(0.025, 0.50, 0.975))

mcmc_sample <- rstan::extract(mcmc_7_a)
state_name <- "mu"
result_7_a <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_7_a) <- c("low_7_a", "med_7_a", "upr_7_a")



#No use of non-detection data
data_row_CD <- data.frame(true = which(!is.na(data_stan$ND))) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)
data_row_D <- data.frame(detected = which(data_stan$ND >= threshold)) #pick row numbers of detected data
sample_size_D <- nrow(data_row_D)

data_ww <- data_stan %>% filter(ND >= threshold)
ww <- data_ww$logWW
print(1-nrow(data_ww)/nrow(data_stan))
data_nd <- data_stan %>% drop_na(ND)
ND <- data_nd$ND

data_list_ww <- list(n_all = sample_size, n_d = sample_size_D,
                     ww = ww, row_d = data_row_D$detected, size = size)

mcmc_7_b <- stan(
  file = "20240809_stat_space_1.stan", 
  data = data_list_ww,
  seed = 1,
  chain = 1,
  iter = 15000,
  warmup = 4000,
  thin = 1
)

print(mcmc_7_b, pars = c("s1", "s2"), probe = c(0.025, 0.50, 0.975))

mcmc_sample_b <- rstan::extract(mcmc_7_b)
state_name <- "mu" #状態の名前：stan fileを参照すること
result_7_b <- data.frame(t(apply(
  X = mcmc_sample_b[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_7_b) <- c("low_7_b", "med_7_b", "upr_7_b")



#replacement
data_row_CD <- data.frame(true = which(!is.na(data_stan$ND))) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)


data_ww <- data_stan %>% drop_na()
ww <- data_ww$substituion #(logWW_rep)
print(1-nrow(data_ww)/nrow(data_stan))


data_list_ww <- list(n_all = sample_size, n_d = sample_size_CD,
                     ww = ww, row_d = data_row_CD$true, size = size)

mcmc_7_c <- stan(
  file = "20240809_stat_space_1.stan", 
  data = data_list_ww,
  seed = 1,
  chain = 1,
  iter = 15000,
  warmup = 4000,
  thin = 1
)

print(mcmc_7_c, pars = c("s1", "s2"), probe = c(0.025, 0.50, 0.975))

mcmc_sample_c <- rstan::extract(mcmc_7_c)
state_name <- "mu" #状態の名前：stan fileを参照すること
result_7_c <- data.frame(t(apply(
  X = mcmc_sample_c[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_7_c) <- c("low_7_c", "med_7_c", "upr_7_c")




#Creation of data for figures
data_stan_true <- data_sim_final_6 %>% select(time, logWW, ND)
data_fig2 <- cbind(data_stan_true, result_7_a, result_7_b, result_7_c)
data_fig2 <- data_fig2 %>% mutate(ND = as.factor(ND))


plot <- ggplot(data_fig2, aes(x = time))
plot <- plot + geom_hline(yintercept = log10(1200), colour = "black", linetype = "dashed")
plot <- plot + geom_ribbon(aes(ymin = low_7_a, ymax = upr_7_a),colour = "NA", alpha = 0.5, fill = "#74A9CF") + geom_line(aes(y = med_7_a), color = "#0570B0", size = 1.5)
plot <- plot + geom_line(aes(y = med_7_b), color = "#8C96C6", size = 1.5) + geom_line(aes(y = med_7_c), color = "#3690C0", size = 1.5)
plot <- plot + geom_point(aes(y = logWW, shape = ND), color = "#0570B0", fill = "white",  size = 1.4)
plot <- plot + scale_shape_manual(values = c(4, 25, 24, 21))
plot <- plot + labs(x = "day", y = "Wasteater concentration")
plot <- plot + scale_y_continuous(limits = c(0.5,5.5), breaks = seq(0.5, 5.5, 1.0))
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






#Effective reproduction number 
gamma_function <- function(x, shape, rate) {
  return((rate^shape) * (x^(shape - 1)) * exp(-rate * x) / gamma(shape))
}
gamma_probability <- function(lower, upper, shape, rate) {
  integrate(gamma_function, lower = lower, upper = upper, shape = shape, rate = rate)$value
}
shape <- 4  # ガンマ関数の形状パラメータ
rate <- 1   # ガンマ関数のレートパラメータ
prob <- NULL
for(i in 0:9){
  probability <- gamma_probability(i, i + 1, shape, rate)
  prob <- c(prob, probability) 
}
gamma_prob <- data.frame(prob)

#data_expect_true
case_all <- numeric()
data_case <- data_sim_final_6 %>% select(time, incidence_2) %>% filter(incidence_2 > 0)
zeros_2 <- rep(0,10)
data_c_0 <- data.frame(time = zeros_2)
data_c_1 <- data.frame(incidence_2 = zeros_2)
data_c_2_2 <- cbind(data_c_0, data_c_1)
data_m_1 <- rbind(data_c_2_2, data_case)

run <- nrow(data_m_1)
for(i in 11:run){
  case_2 <- 0
  for(m in 1:nrow(gamma_prob)){
    case_1 <- data_m_1$incidence_2[i-m]*gamma_prob$prob[m] #incidence_2, report_case
    case_2 <- case_2 + case_1
  }
  case_all <- c(case_all, case_2) 
}
data_expected_case <- data.frame(case_all)


zeros_1 <- rep(0,10)
data_b_0 <- data.frame(case_all = zeros_1)
data_eff_2_b <- rbind(data_b_0, data_expected_case)
data_eff_3 <- cbind(data_m_1, data_eff_2_b)
data_eff_3 <- data_eff_3[12:nrow(data_eff_3),]

#write.csv(x = data_eff_3, file = "C:/WBE_ND/Re_simulation.csv")
data_eff_3 <- data_eff_3 %>% mutate(incidence_3 = round(incidence_2), case_all_2 = round(case_all))

#incidence_2, data_report_error
data_list_eff_2 <- list(n1 = nrow(data_eff_3), incidence = data_eff_3$incidence_3, expect = data_eff_3$case_all_2)

mcmc_eff_cli_incidence <- stan(
  file = "20240825_clinical_Re_2_5day.stan",
  data = data_list_eff_2,
  seed = 1,
  chain = 1,
  iter = 10000,
  warmup = 4000,
  thin = 1
)
print(mcmc_eff_cli_incidence, probe = c(0.025, 0.50, 0.975))

mcmc_sample_Re <- rstan::extract(mcmc_eff_cli_incidence)
state_name <- "Re" #状態の名前：stan fileを参照すること
result_eff <- data.frame(t(apply(
  X = mcmc_sample_Re[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_eff) <- c("low_Re", "med_Re", "upr_Re")





#Wastewater Effective reproduction number
data_ef <- mcmc_sample[["mu"]]
size <- nrow(data_sim_final_6) 

sample_size_2 <- nrow(data_ef)
summary_2_A <- NULL
summary_2_B <- NULL
summary_2_C <- NULL
for (i in 11:size){
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
    IAV_8 <- 10^(data_ef[m,i-8])
    IAV_9 <- 10^(data_ef[m,i-9])
    IAV_10 <- 10^(data_ef[m,i-10])
    
    p_estimate <- IAV/(IAV_1*gamma_prob$prob[1] + IAV_2*gamma_prob$prob[2] + IAV_3*gamma_prob$prob[3] +
                         IAV_4*gamma_prob$prob[4] + IAV_5*gamma_prob$prob[5] + IAV_6*gamma_prob$prob[6] + IAV_7*gamma_prob$prob[7] +
                         IAV_8*gamma_prob$prob[8] + IAV_9*gamma_prob$prob[9] + IAV_10*gamma_prob$prob[10])
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
data_result_ReW_a <- data.frame(A = summary_2_A, B = summary_2_B, C = summary_2_C)
colnames(data_result_ReW_a) <- c("lower_ReW_a", "median_ReW_a", "upper_ReW_a")



#Wastewater Effective reproduction number_b
data_ef <- mcmc_sample_b[["mu"]]
size <- nrow(data_sim_final_6) 

sample_size_2 <- nrow(data_ef)
summary_2_A <- NULL
summary_2_B <- NULL
summary_2_C <- NULL
for (i in 11:size){
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
    IAV_8 <- 10^(data_ef[m,i-8])
    IAV_9 <- 10^(data_ef[m,i-9])
    IAV_10 <- 10^(data_ef[m,i-10])
    
    p_estimate <- IAV/(IAV_1*gamma_prob$prob[1] + IAV_2*gamma_prob$prob[2] + IAV_3*gamma_prob$prob[3] +
                         IAV_4*gamma_prob$prob[4] + IAV_5*gamma_prob$prob[5] + IAV_6*gamma_prob$prob[6] + IAV_7*gamma_prob$prob[7] +
                         IAV_8*gamma_prob$prob[8] + IAV_9*gamma_prob$prob[9] + IAV_10*gamma_prob$prob[10])
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
data_result_ReW_b <- data.frame(A = summary_2_A, B = summary_2_B, C = summary_2_C)
colnames(data_result_ReW_b) <- c("lower_ReW_b", "median_ReW_b", "upper_ReW_b")



#Wastewater Effective reproduction number
data_ef <- mcmc_sample_c[["mu"]]
size <- nrow(data_sim_final_6) 

sample_size_2 <- nrow(data_ef)
summary_2_A <- NULL
summary_2_B <- NULL
summary_2_C <- NULL
for (i in 11:size){
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
    IAV_8 <- 10^(data_ef[m,i-8])
    IAV_9 <- 10^(data_ef[m,i-9])
    IAV_10 <- 10^(data_ef[m,i-10])
    
    p_estimate <- IAV/(IAV_1*gamma_prob$prob[1] + IAV_2*gamma_prob$prob[2] + IAV_3*gamma_prob$prob[3] +
                         IAV_4*gamma_prob$prob[4] + IAV_5*gamma_prob$prob[5] + IAV_6*gamma_prob$prob[6] + IAV_7*gamma_prob$prob[7] +
                         IAV_8*gamma_prob$prob[8] + IAV_9*gamma_prob$prob[9] + IAV_10*gamma_prob$prob[10])
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
data_result_ReW_c <- data.frame(A = summary_2_A, B = summary_2_B, C = summary_2_C)
colnames(data_result_ReW_c) <- c("lower_ReW_c", "median_ReW_c", "upper_ReW_c")


data_Rew <- cbind(data_result_ReW_a, data_result_ReW_b, data_result_ReW_c)
data_Re <- read.csv("Re_simulation_2.csv")
data_Re_2 <- data_Re %>% drop_na() %>% select(time, Re_w)
data_Re_fig <- cbind(data_Re_2, data_Rew)

plot <- ggplot(data_Re_fig, aes(x = time))
plot <- plot + geom_ribbon(aes(ymin = lower_ReW_a, ymax = upper_ReW_a),colour = "NA", alpha = 0.5, fill = "#74A9CF") + geom_line(aes(y = median_ReW_a), color = "#0570B0", size = 1.5)
plot <- plot + geom_line(aes(y = median_ReW_b), color = "#8C96C6", size = 1.5) + geom_line(aes(y = median_ReW_c), color = "#3690C0", size = 1.5)
plot <- plot + geom_line(aes(y = Re_w), color = "red", size = 1.5)
plot <- plot + labs(x = "day", y = "effective reproduction number")
plot <- plot + scale_y_continuous(limits = c(0.0, 2.5), breaks = seq(0.0, 2.5, 0.5))
plot <- plot + scale_x_continuous(limits = c(20, 150), breaks = seq(0, 30, 150))
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