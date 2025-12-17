
getwd() #check where you should upload csv file analyzed

install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("ggplot2")
install.packages("rstan")
install.packages("stats")
install.packages("bayesplot")

setwd("C:/WBE_ND")

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(rstan)
library(stats)
library(bayesplot)

threshold <- 0.8 
data_1 <- read.csv("Demonstration_toydata.csv") #change the name of csv file



data <- data_1
data <- data %>% mutate(log_concentration = log10(concentration))
data <- data %>% mutate(date = as.Date(date))
data <- data %>% select("date", "number_of_tested_samples", "number_of_positive_samples", "log_concentration")
colnames(data) <- c("date", "count", "positive", "concentration")



#state-space model with logistic
data_stan <- data
data_stan <- data_stan %>% mutate(positive_rate = positive/count)
sample_size <- nrow(data_stan)

#vector of row number used for the analysis
#pick row numbers for censored data
data_row_D <- data.frame(true = which((data_stan$positive_rate >= threshold))) 
sample_size_D <- nrow(data_row_D)
#pick row numbers for censored data
data_row_CD <- data.frame(true = which(data_stan$count > 0)) 
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
  file = "state_space_model_with_logistic_highspeed.stan", 
  data = data_list_ww,
  seed = 1,
  chain = 4,
  iter = 10000,
  warmup = 5000,
  thin = 4
)

print(mcmc, pars = c("c1", "c2", "s1", "s2"), probe = c(0.025, 0.50, 0.975))

#check traceplots if you want
#mcmc_combo(mcmc, pars = c("c1", "c2", "s1", "s2")) 


#MCMC samples and 95% credible intervals
mcmc_sample <- rstan::extract(mcmc)
state_name <- "mu"
result <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs = c(0.025, 0.5, 0.975) #credible interval can be changed
)))

colnames(result) <- c("low", "median", "upr")
data_estimated_concentration <- cbind(data, result)


#figure
data_fig <- data_estimated_concentration
data_fig <- data_fig %>% mutate(posi = if_else(count < 1, "no", if_else(positive >= threshold*count, "yes", "no")))

plot <- ggplot(data_fig, aes(x = as.Date(date)))
plot <- plot + geom_point(aes(y = concentration, color = posi, shape = posi))
plot <- plot + scale_shape_manual(values = c(24, 16))
plot <- plot + scale_color_manual(values = c("#FEC44F","#0570B0")) #no: yellow, yes: blue
plot <- plot + geom_ribbon(aes(ymin = low, ymax = upr), fill = "#3690C0", alpha = 0.3)
plot <- plot + geom_line(aes(y = median), color = "#0570B0", size = 1) 
plot <- plot + labs(x = "Date", y = "wastewater concentration")
plot <- plot + scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b") #change the date
plot <- plot + scale_y_continuous(limits = c(2.0, 5.5), breaks = seq(2.0, 5.5, 1)) #change the scale of y axis
plot <- plot + theme_bw()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.0),
  axis.ticks.length = unit(-2, "mm"))
plot


#Export estimation result
#confirm the place where file should be loaded by using "getwd()": "C:/XXXX/"
#Write file name: "2024xxxx_xx_xx,csv"
write.csv(x = data_estimated_concentration, file = "C:/XXXX/2024XXXX_XX_XX.csv") 






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
    c1_1 <- data_ef_c1[m,1]
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


#Export estimation result
#confirm the place where file should be loaded by using "getwd()": "C:/XXXX/"
#Write file name: "2024xxxx_xx_xx,csv"
write.csv(x = data_fig_logis, file = "C:/XXXX/2024XXXX_XX_XX.csv")




