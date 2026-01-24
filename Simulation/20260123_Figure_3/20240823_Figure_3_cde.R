setwd("C:/WBE_ND")

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

library(RColorBrewer)
# "PuBu"パレットを表示する
display.brewer.all(type = "seq")
# "PuBu"パレットの色を取得する
colors <- brewer.pal(9, "Greys") 
print(colors)


#Figure 2c
data_c <- read.csv("figure_3_c.csv")

plot <- ggplot(data_c, aes(x = as.factor(sampling_number), y = error, fill = as.factor(sampling_number)))
plot <- plot + geom_dotplot(binaxis="y", binwidth=0.0045, stackdir = "center", alpha = 1.0) 
#plot <- plot + geom_boxplot(width=0.3, notch = FALSE, color ="#737373", alpha = 0.0)
plot <- plot + labs(x = "The number of analyzed samples", y = "Mean percentage error")
plot <- plot + scale_fill_brewer(palette = "Blues") #scale_fill_manual(values = c("lightblue", "orange", "red"))
plot <- plot + scale_y_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05))
plot <- plot + theme_classic()
plot <- plot + stat_summary(fun = median, geom = "crossbar", color = "#CE1256", width = 0.75)
plot <- plot + stat_summary(fun = mean, geom = "crossbar", color = "#0570B0", width = 0.75)
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


#Figure2_d
data_d <- read.csv("figure_3_d.csv")

plot <- ggplot(data_d, aes(x = as.factor(frequency), y = error, fill = as.factor(frequency)))
plot <- plot + geom_dotplot(binaxis="y", binwidth=0.004, stackdir = "center", alpha = 1.0)
plot <- plot + labs(x = "The number of analyzed samples", y = "Mean percentage error")
plot <- plot + scale_fill_brewer(palette = "Blues")
plot <- plot + theme_classic()
plot <- plot + stat_summary(fun = median, geom = "crossbar", color = "#CE1256", width = 0.75)
plot <- plot + stat_summary(fun = mean, geom = "crossbar", color = "#0570B0", width = 0.75)
plot <- plot + scale_y_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05))
plot <- plot + theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.line = element_line(size = 1.0, lineend = "square"),
  text = element_text(colour = "black", size = 14),
  legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"),
  strip.text = element_blank() # ストリップラベルを非表示にする
)
plot <- plot + guides(fill = guide_legend(title = NULL))

# "sample"ごとにプロットを分ける
plot <- plot + facet_wrap(~ sample)
plot


#Figure2_e
data_e <- read.csv("figure_3_e.csv")

plot <- ggplot(data_e, aes(x = as.factor(frequency), y = error, fill = as.factor(frequency)))
plot <- plot + geom_dotplot(binaxis="y", binwidth=0.004, stackdir = "center", alpha = 1.0)
plot <- plot + labs(x = "The number of analyzed samples", y = "Mean percentage error")
plot <- plot + scale_fill_brewer(palette = "Blues")
plot <- plot + theme_classic()
plot <- plot + stat_summary(fun = median, geom = "crossbar", color = "#CE1256", width = 0.75)
plot <- plot + stat_summary(fun = mean, geom = "crossbar", color = "#0570B0", width = 0.75)
plot <- plot + scale_y_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05))
plot <- plot + theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.line = element_line(size = 1.0, lineend = "square"),
  text = element_text(colour = "black", size = 14),
  legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"),
  strip.text = element_blank() # ストリップラベルを非表示にする
)
plot <- plot + guides(fill = guide_legend(title = NULL))
plot <- plot + facet_wrap(~ total)
plot
