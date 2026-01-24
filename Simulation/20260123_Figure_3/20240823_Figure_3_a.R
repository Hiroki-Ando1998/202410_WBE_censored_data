setwd("C:/WBE_ND")

library(deSolve)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(rstan)
library(stats)

size = 3
data_sim_final_6 <- read.csv("data_fig2_a_b.csv")

#3-day sampling
data_stan_3 <- data_sim_final_6
data_stan_3[data_stan_3$time %% 7 == 1|data_stan_3$time %% 7 == 3|data_stan_3$time %% 7 == 5 |data_stan_3$time %% 7 == 6, ] <- NA
data_stan <- data_stan_3

sample_size <- nrow(data_stan)
data_row_CD <- data.frame(true = which(!is.na(data_stan$ND))) #pick row numbers for censored data
sample_size_CD <- nrow(data_row_CD)
data_row_D <- data.frame(detected = which(data_stan$ND >= threshold)) #pick row numbers of detected data
sample_size_D <- nrow(data_row_D)

data_ww <- data_stan %>% filter(ND >= threshold)
ww <- data_ww$measured_ww #logWW or measured_ww
data_nd <- data_stan %>% drop_na(ND)
ND <- data_nd$ND

data_list_ww <- list(n_all = sample_size, n_cd = sample_size_CD, n_d = sample_size_D,
                     ww = ww, nd = ND, row_cd = data_row_CD$true, row_d = data_row_D$detected, size = size)

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
result_3 <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975)
)))

colnames(result_3) <- c("low_3", "med_3", "upr_3")


data_stan_true <- data_sim_final_6 %>% select(time, measured_ww, logWW, ND)
data_fig3 <- cbind(data_stan_true, result_3)
data_fig3 <- data_fig3 %>% mutate(measured_ww_2 = if_else(ND > 2, measured_ww, -10))
data_fig3 <- data_fig3 %>% mutate(ND = as.factor(ND))

plot <- ggplot(data_fig3, aes(x = time))
plot <- plot + geom_hline(yintercept = log10(1200), colour = "black", linetype = "dashed")
plot <- plot + geom_ribbon(aes(ymin = low_7_a, ymax = upr_7_a),colour = "NA", alpha = 0.5, fill = "#74A9CF") + geom_line(aes(y = med_7_a), color = "#0570B0", size = 1.5)
plot <- plot + geom_point(aes(y = logWW, shape = ND), color = "#0570B0", fill = "white",  size = 1.4)
plot <- plot + geom_point(aes(y = measured_ww_2), fill = "#8C96C6", size = 1.4, shape = 21 )
plot <- plot + scale_shape_manual(values = c(4, 25, 24, 21))
plot <- plot + labs(x = "day", y = "Wasteater concentration")
plot <- plot + scale_y_continuous(limits = c(0.0,5.5), breaks = seq(0.0, 5.5, 1.0))
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