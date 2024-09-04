setwd("C:/WBE_ND")


library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

library(RColorBrewer)
display.brewer.all(type = "seq")
# "PuBu"パレットを表示する
display.brewer.all(type = "seq")
# "PuBu"パレットの色を取得する
colors <- brewer.pal(9, "YlOrBr")  # 9は取得する色の数を指定します
print(colors)

"#FFF7FB" "#ECE7F2" "#D0D1E6" "#A6BDDB" "#74A9CF" "#3690C0" "#0570B0" "#045A8D" "#023858"
"#8C510A" "#BF812D" "#DFC27D" "#F6E8C3" "#F5F5F5" "#C7EAE5" "#80CDC1" "#35978F" "#01665E"


data_IAV_CA <- read.csv("Figure_4_IAV_CA.csv")
data_RSV_CA <- read.csv("Figure_4_RSV_CA.csv")

data <- data_IAV_CA
data <- data %>% mutate(concen = if_else(count < 1, NA, if_else(positive >= 0.8*count, concentration, substitution)))
data <- data %>% mutate(posi = if_else(count < 1, "no", if_else(positive >= 0.8*count, "yes", "no")))



plot <- ggplot(data, aes(x = as.Date(date)))
plot <- plot + geom_point(aes(y = concen, color = posi, shape = posi, fill = as.character(positive)))
plot <- plot + scale_shape_manual(values = c(24, 16))
plot <- plot + scale_fill_manual(values = c("NA", "NA", "#D0D1E6", "#80CDC1", "#0570B0", "#FEE391", "#FEE391"))
plot <- plot + scale_color_manual(values = c("#0570B0", "#FEC44F"))
plot <- plot + geom_ribbon(aes(ymin = low_7_s, ymax = upr_7_s), fill = "#3690C0", alpha = 0.3)  #trend model
plot <- plot + geom_line(aes(y = med_7_s), color = "#0570B0", size = 1) 
plot <- plot + geom_ribbon(aes(ymin = low_7_a, ymax = upr_7_a), fill = "#FEE391", alpha = 0.8)  #trend model
plot <- plot + geom_line(aes(y = med_7_a), color = "#FEC44F", size = 1) 
plot <- plot + labs(x = "Date", y = "wastewater concentration")
plot <- plot + scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot <- plot + scale_y_continuous(limits = c(2.0, 5.5), breaks = seq(2.0, 5.5, 1))
plot <- plot + theme_bw()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.0),
  axis.ticks.length = unit(-2, "mm"))
plot



#CO
data_IAV_CO <- read.csv("Figure_4_IAV_CO.csv")
data_RSV_CO <- read.csv("Figure_4_RSV_CO.csv")

data <- data_RSV_CO
data <- data %>% mutate(concen = if_else(count < 1, NA, if_else(positive >= 0.8*count, concentration, substitution)))
data <- data %>% mutate(posi = if_else(count < 1, "no", if_else(positive >= 0.8*count, "yes", "no")))

plot <- ggplot(data, aes(x = as.Date(date)))
plot <- plot + geom_point(aes(y = concen, color = posi, shape = posi, fill = as.character(positive)))
plot <- plot + scale_shape_manual(values = c(24, 16))
plot <- plot + scale_fill_manual(values = c("NA", "#0570B0", "#FEE391"))
plot <- plot + scale_color_manual(values = c("#0570B0", "#FEC44F"))
plot <- plot + geom_ribbon(aes(ymin = low_7_s, ymax = upr_7_s), fill = "#3690C0", alpha = 0.3)  #trend model
plot <- plot + geom_line(aes(y = med_7_s), color = "#0570B0", size = 1) 
plot <- plot + geom_ribbon(aes(ymin = low_7_a, ymax = upr_7_a), fill = "#FEE391", alpha = 0.8)  #trend model
plot <- plot + geom_line(aes(y = med_7_a), color = "#FEC44F", size = 1) 
plot <- plot + labs(x = "Date", y = "wastewater concentration")
plot <- plot + scale_x_date(limits = c(as.Date("2022-05-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot <- plot + scale_y_continuous(limits = c(2.0, 5.5), breaks = seq(2.0, 5.5, 1))
plot <- plot + theme_bw()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.0),
  axis.ticks.length = unit(-2, "mm"))
plot



#FL
data_IAV_FL <- read.csv("Figure_4_IAV_FL.csv")
data_RSV_FL <- read.csv("Figure_4_RSV_FL.csv")

data <- data_RSV_FL
data <- data %>% mutate(concen = if_else(count < 1, NA, if_else(positive >= 0.8*count, concentration, substitution)))
data <- data %>% mutate(posi = if_else(count < 1, "no", if_else(positive >= 0.8*count, "yes", "no")))

plot <- ggplot(data, aes(x = as.Date(date)))
plot <- plot + geom_point(aes(y = concen, color = posi, shape = posi, fill = as.character(positive)))
plot <- plot + scale_shape_manual(values = c(24, 16))
plot <- plot + scale_fill_manual(values = c("NA","#D0D1E6", "#0570B0", "#FEE391"))
plot <- plot + scale_color_manual(values = c("#0570B0", "#FEC44F"))
plot <- plot + geom_ribbon(aes(ymin = low_7_s, ymax = upr_7_s), fill = "#3690C0", alpha = 0.3)  #trend model
plot <- plot + geom_line(aes(y = med_7_s), color = "#0570B0", size = 1) 
plot <- plot + geom_ribbon(aes(ymin = low_7_a, ymax = upr_7_a), fill = "#FEE391", alpha = 0.8)  #trend model
plot <- plot + geom_line(aes(y = med_7_a), color = "#FEC44F", size = 1) 
plot <- plot + labs(x = "Date", y = "wastewater concentration")
plot <- plot + scale_x_date(limits = c(as.Date("2022-04-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot <- plot + scale_y_continuous(limits = c(2.0, 5.5), breaks = seq(2.0, 5.5, 1))
plot <- plot + theme_bw()
plot <- plot + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.0),
  axis.ticks.length = unit(-2, "mm"))
plot






data_IAV_CA <- read.csv("Figure_4_IAV_CA.csv")
data_RSV_CA <- read.csv("Figure_4_RSV_CA.csv")
data_IAV_CO <- read.csv("Figure_4_IAV_CO.csv")
data_RSV_CO <- read.csv("Figure_4_RSV_CO.csv")
data_IAV_FL <- read.csv("Figure_4_IAV_FL.csv")
data_RSV_FL <- read.csv("Figure_4_RSV_FL.csv")

data_a <- data_RSV_FL
data_a <- data_a %>% mutate(posi = if_else(count < 1, "na", if_else(positive >= 0.8*count, "yes", "no")))
table(data_a$posi)