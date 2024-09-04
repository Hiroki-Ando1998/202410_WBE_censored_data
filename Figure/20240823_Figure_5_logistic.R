setwd("C:/WBE_ND")


library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(rstan)
library(stats)

data <- read.csv("Figure_6_logistic.csv")


plot <- ggplot(data, aes(x = log10(ww))) 
plot <- plot + geom_line(aes(y = RSV_FL), color = "#980043", size = 1, linetype = "dashed") 
plot <- plot + geom_line(aes(y = RSV_CO), color = "#D4B9DA", size = 1) 
plot <- plot + geom_line(aes(y = IAV_FL), color = "#023858", size = 1, linetype = "dashed") 
plot <- plot + geom_line(aes(y = IAV_CO), color = "#74A9CF", size = 1) 
plot <- plot + geom_line(aes(y = RSV_CA), color = "#E7298A", size = 1) 
plot <- plot + geom_line(aes(y = IAV_CA), color = "#0570B0", size = 1) 
plot <- plot + labs(x = "wastewater concentration", y = "detection probability")
plot <- plot + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))
plot <- plot + theme_bw()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot


