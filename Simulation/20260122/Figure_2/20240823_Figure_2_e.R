setwd("C:/WBE_ND")

library(deSolve)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(rstan)
library(stats)

data <- read.csv("WBE_ND_fig_2_e.csv")

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

size <- 20 #the number of analyzed samples
p3 <- c()
for(i in 1:nrow(data)){
  A <- data$logWW[i]
  p1 <- 1/(1 + exp(-c1 - c2*A))
  p2 <- rbinom(1, size = size, prob = p1)
  p3 <- c(p3, p2) 
}

data_test <- data.frame(ND_2 = p3)
data_2 <- cbind(data, data_test)

#10% detection probability at 100 copies (a1, b1)
#50% detection probability at 500 copies (a2, b2)
a1 <- 0.10
b1 <- 50
a2 <- 0.5
b2 <- 400

c2 <- log(a1/(1-a1))/(log10(b1)-log10(b2))
c1 <- -c2*log10(b2) 

c_test <- 0.95
c095 <- (log(c_test/(1-c_test))-c1)/c2 #LOD (95% detection)

size <- 3 #the number of analyzed samples
p3 <- c()
for(i in 1:nrow(data_2)){
  A <- data_2$logWW[i]
  p1 <- 1/(1 + exp(-c1 - c2*A))
  p2 <- rbinom(1, size = size, prob = p1)
  p3 <- c(p3, p2) 
}

data_test <- data.frame(ND_3 = p3)
data_3 <- cbind(data_2, data_test)

data_sim_final_6 <- data_3
colnames(data_sim_final_6) <- c("time", "incidence_2", "wastewater","measured_ww", "logWW", "ND_1", "ND_2", "ND_3")
 

#write.csv(x = data_sim_final_6, file = "C:/WBE_ND/WBE_ND_fig_2_e_analysis.csv")







threshold <- 0.8 #threshold to selecting data on quantified concentration
data_sim_final_6 <- read.csv("WBE_ND_fig_2_e_analysis.csv")

#3-day sampling
data_stan_3 <- data_sim_final_6
data_stan_3[data_stan_3$time %% 7 == 1|data_stan_3$time %% 7 == 3|data_stan_3$time %% 7 == 5 |data_stan_3$time %% 7 == 6, ] <- NA
data_stan <- data_stan_3

sample_size <- nrow(data_stan)
data_row_CD <- data.frame(true = which(!is.na(data_stan$ND_1))) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)
data_row_D <- data.frame(detected = which(data_stan$ND_1 >= threshold*3)) #pick row numbers of detected data
sample_size_D <- nrow(data_row_D)

data_ww <- data_stan %>% filter(ND_1 >= threshold*3)
ww <- data_ww$logWW
data_nd <- data_stan %>% drop_na(ND_1)
ND <- data_nd$ND_1

data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
                     ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$detected, size = 3)

mcmc_3 <- stan(
  file = "20240809_stat_space_binomial.stan", #"20240809_stat_space_bernoulli.stan"
  data = data_list_ww,
  seed = 1,
  chain = 1,
  iter = 10000,
  warmup = 4000,
  thin = 1
)

print(mcmc_3, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))

mcmc_sample <- rstan::extract(mcmc_3)
state_name <- "mu" #状態の名前：stan fileを参照すること
result_3_a <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_3_a) <- c("low_3_a", "med_3_a", "upr_3_a")


#3-day sampling (large number of samples)
sample_size <- nrow(data_stan)
data_row_CD <- data.frame(true = which(!is.na(data_stan$ND_2))) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)
data_row_D <- data.frame(detected = which(data_stan$ND_2 >= threshold*20)) #pick row numbers of detected data
sample_size_D <- nrow(data_row_D)

data_ww <- data_stan %>% filter(ND_2 >= threshold*20)
ww <- data_ww$logWW
data_nd <- data_stan %>% drop_na(ND_2)
ND <- data_nd$ND_2
data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
                     ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$detected, size = 20)

mcmc_3 <- stan(
  file = "20240809_stat_space_binomial.stan", #"20240809_stat_space_bernoulli.stan"
  data = data_list_ww,
  seed = 1,
  chain = 1,
  iter = 10000,
  warmup = 4000,
  thin = 1
)

print(mcmc_3, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))

mcmc_sample <- rstan::extract(mcmc_3)
state_name <- "mu" #状態の名前：stan fileを参照すること
result_3_b <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_3_b) <- c("low_3_b", "med_3_b", "upr_3_b")




#3-day sampling (high sensitivity)
sample_size <- nrow(data_stan)
data_row_CD <- data.frame(true = which(!is.na(data_stan$ND_3))) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)
data_row_D <- data.frame(detected = which(data_stan$ND_3 >= threshold*3)) #pick row numbers of detected data
sample_size_D <- nrow(data_row_D)

data_ww <- data_stan %>% filter(ND_3 >= threshold*3)
ww <- data_ww$logWW
data_nd <- data_stan %>% drop_na(ND_3)
ND <- data_nd$ND_3
data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
                     ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$detected, size = 3)

mcmc_3 <- stan(
  file = "20240809_stat_space_binomial.stan", #"20240809_stat_space_bernoulli.stan"
  data = data_list_ww,
  seed = 1,
  chain = 1,
  iter = 10000,
  warmup = 4000,
  thin = 1
)

print(mcmc_3, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))

mcmc_sample <- rstan::extract(mcmc_3)
state_name <- "mu" #状態の名前：stan fileを参照すること
result_3_c <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_3_c) <- c("low_3_c", "med_3_C", "upr_3_c")


data_fig2_e <- cbind(data_stan, result_3_a, result_3_b, result_3_c)

#write.csv(x = data_fig2_e, file = "C:/WBE_ND/WBE_ND_fig_2_e_data.csv")


data_fig2_e <- read.csv("WBE_ND_fig_2_e_data.csv")
#Creation of data for figures
data_fig2_e <- data_fig2_e %>% mutate(ND_1 = as.factor(ND_1))

plot <- ggplot(data_fig2_e, aes(x = time))
plot <- plot + geom_hline(yintercept = log10(1200), colour = "black", linetype = "dashed")
plot <- plot + geom_line(aes(y = med_3_b), color = "#0570B0", size = 1.25)
plot <- plot + geom_line(aes(y = med_3_C), color = "#3690C0", size = 1.25)
plot <- plot + geom_line(aes(y = med_3_a), color = "black", size = 1.25)
plot <- plot + geom_point(aes(y = logWW, shape = ND_1), color = "#0570B0", fill = "white",  size = 1.4)
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



