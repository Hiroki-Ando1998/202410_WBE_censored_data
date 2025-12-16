
setwd("C:/WBE_ND/20251216_SupAnalysis_logisticparameter")


library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(cowplot)



data_CA_1 <- read.csv("Figure_4_RSV_CA.csv")
data_CO_1 <- read.csv("Figure_4_RSV_CO.csv")
data_FL_1 <- read.csv("Figure_4_RSV_FL.csv")
data_CA_2 <- read.csv("20251215_RSV_CA.csv")
data_CO_2 <- read.csv("20251215_RSV_CO.csv")
data_FL_2 <- read.csv("20251215_RSV_FL.csv")


data_CA_2_a <- select(data_CA_2, date, low_7_a, med_7_a, upr_7_a)
colnames(data_CA_2_a) <- c("date_2", "low_7_a_2", "med_7_a_2", "upr_7_a_2")
data_CO_2_a <- select(data_CO_2, date, low_7_a, med_7_a, upr_7_a)
colnames(data_CO_2_a) <- c("date_2", "low_7_a_2", "med_7_a_2", "upr_7_a_2")
data_FL_2_a <- select(data_FL_2, date, low_7_a, med_7_a, upr_7_a)
colnames(data_FL_2_a) <- c("date_2", "low_7_a_2", "med_7_a_2", "upr_7_a_2")


data_CA <- cbind(data_CA_1, data_CA_2_a)
data_CO <- cbind(data_CO_1, data_CO_2_a)
data_FL <- cbind(data_FL_1, data_FL_2_a)

#---------------------------------------------------------------------------------------------------------------------
# data <- data_RSV_CO
# data <- data %>% mutate(concen = if_else(count < 1, NA, if_else(positive >= 0.8*count, concentration, substitution)))
# data <- data %>% mutate(posi = if_else(count < 1, "no", if_else(positive >= 0.8*count, "yes", "no")))


plot_FL <- ggplot(data_FL, aes(x = as.Date(date)))
#plot <- plot + geom_point(aes(y = concen, color = posi, shape = posi, fill = as.character(positive)))
#plot <- plot + scale_shape_manual(values = c(24, 16))
#plot <- plot + scale_fill_manual(values = c("NA", "#0570B0", "#FEE391"))
#plot <- plot + scale_color_manual(values = c("#0570B0", "#FEC44F"))
plot_FL <- plot_FL + geom_ribbon(aes(ymin = low_7_a_2, ymax = upr_7_a_2), fill = "#3690C0", alpha = 0.3)  #trend model
plot_FL <- plot_FL + geom_ribbon(aes(ymin = low_7_a, ymax = upr_7_a), fill = "#FEE391", alpha = 0.3)  #trend model
plot_FL <- plot_FL + geom_line(aes(y = med_7_a_2), color = "#0570B0", size = 1) 
plot_FL <- plot_FL + geom_line(aes(y = med_7_a), color = "#FEC44F", size = 1) 
plot_FL <- plot_FL + labs(x = "Month", y = "wastewater concentration")
plot_FL <- plot_FL + scale_x_date(limits = c(as.Date("2022-05-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot_FL <- plot_FL + scale_y_continuous(limits = c(2.0, 5.5), breaks = seq(2.0, 5.5, 1))
plot_FL <- plot_FL + theme_bw()
plot_FL <- plot_FL + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.0),
  axis.ticks.length = unit(-2, "mm"))
plot_FL





plot_CA <- ggplot(data_CA, aes(x = as.Date(date)))
#plot <- plot + geom_point(aes(y = concen, color = posi, shape = posi, fill = as.character(positive)))
#plot <- plot + scale_shape_manual(values = c(24, 16))
#plot <- plot + scale_fill_manual(values = c("NA", "#0570B0", "#FEE391"))
#plot <- plot + scale_color_manual(values = c("#0570B0", "#FEC44F"))
plot_CA <- plot_CA + geom_ribbon(aes(ymin = low_7_a_2, ymax = upr_7_a_2), fill = "#3690C0", alpha = 0.3)  #trend model
plot_CA <- plot_CA + geom_ribbon(aes(ymin = low_7_a, ymax = upr_7_a), fill = "#FEE391", alpha = 0.3)  #trend model
plot_CA <- plot_CA + geom_line(aes(y = med_7_a_2), color = "#0570B0", size = 1) 
plot_CA <- plot_CA + geom_line(aes(y = med_7_a), color = "#FEC44F", size = 1) 
plot_CA <- plot_CA + labs(x = "Month", y = "wastewater concentration")
plot_CA <- plot_CA + scale_x_date(limits = c(as.Date("2022-05-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot_CA <- plot_CA + scale_y_continuous(limits = c(2.0, 5.5), breaks = seq(2.0, 5.5, 1))
plot_CA <- plot_CA + theme_bw()
plot_CA <- plot_CA + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.0),
  axis.ticks.length = unit(-2, "mm"))
plot_CA




plot_CO <- ggplot(data_CO, aes(x = as.Date(date)))
#plot <- plot + geom_point(aes(y = concen, color = posi, shape = posi, fill = as.character(positive)))
#plot <- plot + scale_shape_manual(values = c(24, 16))
#plot <- plot + scale_fill_manual(values = c("NA", "#0570B0", "#FEE391"))
#plot <- plot + scale_color_manual(values = c("#0570B0", "#FEC44F"))
plot_CO <- plot_CO + geom_ribbon(aes(ymin = low_7_a_2, ymax = upr_7_a_2), fill = "#3690C0", alpha = 0.3)  #trend model
plot_CO <- plot_CO + geom_ribbon(aes(ymin = low_7_a, ymax = upr_7_a), fill = "#FEE391", alpha = 0.3)  #trend model
plot_CO <- plot_CO + geom_line(aes(y = med_7_a_2), color = "#0570B0", size = 1) 
plot_CO <- plot_CO + geom_line(aes(y = med_7_a), color = "#FEC44F", size = 1) 
plot_CO <- plot_CO + labs(x = "Month", y = "wastewater concentration")
plot_CO <- plot_CO + scale_x_date(limits = c(as.Date("2022-05-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot_CO <- plot_CO + scale_y_continuous(limits = c(2.0, 5.5), breaks = seq(2.0, 5.5, 1))
plot_CO <- plot_CO + theme_bw()
plot_CO <- plot_CO + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.0),
  axis.ticks.length = unit(-2, "mm"))
plot_CO



left_panel <- plot_grid(
  plot_CA, plot_CO, plot_FL,
  ncol = 1,
  align = "v",
  rel_heights = c(1.5, 1.5, 1.5)
)
left_panel

