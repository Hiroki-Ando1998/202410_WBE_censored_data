setwd("C:/WBE_ND")


library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)


data_IAV_CA <- read.csv("Figure_6_IAV_CA.csv")
data_RSV_CA <- read.csv("Figure_6_RSV_CA.csv")



data <- data_IAV_CA
#FEC44F
plot <- ggplot(data, aes(x = as.Date(date)))
plot <- plot + geom_ribbon(aes(ymin = R_low, ymax = R_upper), fill = "#3690C0", alpha = 0.3)  #substitution
plot <- plot + geom_line(aes(y = R_med), color = "#0570B0", size = 1) 
plot <- plot + geom_ribbon(aes(ymin = T_low, ymax = T_upper), fill = "#FEE391", alpha = 0.8)  #trend model
plot <- plot + geom_line(aes(y = T_med), color = "#FEC44F", size = 1) 
plot <- plot + labs(y = "wastewater-based Re", x = "Date")
plot <- plot + scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot <- plot + scale_y_continuous(limits = c(0.5, 2.0), breaks = seq(0.5, 2.0, 0.5))
plot <- plot + theme_bw()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot


#CO
data_IAV_CO <- read.csv("Figure_6_IAV_CO.csv")
data_RSV_CO <- read.csv("Figure_6_RSV_CO.csv")

data_2 <- data_IAV_CO

plot <- ggplot(data_2, aes(x = as.Date(date)))
plot <- plot + geom_ribbon(aes(ymin = R_low, ymax = R_upper), fill = "#3690C0", alpha = 0.3)  #substitution
plot <- plot + geom_line(aes(y = R_med), color = "#0570B0", size = 1) 
plot <- plot + geom_ribbon(aes(ymin = T_low, ymax = T_upper), fill = "#FEE391", alpha = 0.8)  #trend model
plot <- plot + geom_line(aes(y = T_med), color = "#FEC44F", size = 1) 
plot <- plot + labs(x = "wastewater-based Re", y = "Date")
plot <- plot + scale_x_date(limits = c(as.Date("2022-05-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot <- plot + scale_y_continuous(limits = c(0.5, 2.0), breaks = seq(0.5, 2.0, 0.5))
plot <- plot + theme_bw()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot

#FL
data_IAV_FL <- read.csv("Figure_6_IAV_FL.csv")
data_RSV_FL <- read.csv("Figure_6_RSV_FL.csv")

data_3 <- data_RSV_FL

plot <- ggplot(data_3, aes(x = as.Date(date)))
plot <- plot + geom_ribbon(aes(ymin = R_low, ymax = R_upper), fill = "#3690C0", alpha = 0.3)  #substitution
plot <- plot + geom_line(aes(y = R_med), color = "#0570B0", size = 1) 
plot <- plot + geom_ribbon(aes(ymin = T_low, ymax = T_upper), fill = "#FEE391", alpha = 0.5)  #trend model
plot <- plot + geom_line(aes(y = T_med), color = "#FEC44F", size = 1) 
plot <- plot + labs(y = "wastewater-based Re", x = "Date")
plot <- plot + scale_x_date(limits = c(as.Date("2022-04-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot <- plot + scale_y_continuous(limits = c(0.5, 2.0), breaks = seq(0.5, 2.0, 0.5))
plot <- plot + theme_bw()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot





data_4 <- data_RSV_FL

min(data_4$T_med)
max(data_4$T_med)