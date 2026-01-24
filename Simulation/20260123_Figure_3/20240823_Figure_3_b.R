setwd("C:/WBE_ND")

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

data <- read.csv("Figure_3_b_3_3.csv")
pd <- position_dodge(0.3)

plot <- ggplot(data, aes(x = Trial, y = med, colour = as.factor(type)))
plot <- plot + geom_errorbar(aes(ymin = low, ymax = uper), width = 0.2, size = 0.25, color = "black", position = pd)
plot <- plot + geom_point(size = 3.0, position = pd)
plot <- plot + scale_colour_manual(values = c("#0570B0", "#CE1256"))
plot <- plot + geom_hline(yintercept = -11.2, colour = "#0570B0", linetype = "dashed")
plot <- plot + geom_hline(yintercept = 3.64, colour = "#CE1256", linetype = "dashed")
plot <- plot + scale_y_continuous(limits = c(-20, 10), breaks = seq(-20, 10, 10))
plot <- plot + labs(x = "trial", y = "values")
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