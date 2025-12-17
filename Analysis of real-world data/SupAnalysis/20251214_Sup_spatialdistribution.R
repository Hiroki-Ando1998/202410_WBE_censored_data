
setwd("C:/WBE_ND")


library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(cowplot)


data_IAV_CA <- read.csv("20251214_IAV_CA_ww.csv")
data_IAV_CO <- read.csv("20251214_IAV_CO_ww.csv")
data_IAV_FL <- read.csv("20251214_IAV_FL_ww.csv")
data_RSV_CA <- read.csv("20251214_RSV_CA_ww.csv")
data_RSV_CO <- read.csv("20251214_RSV_CO_ww.csv")
data_RSV_FL <- read.csv("20251214_RSV_FL_ww.csv")
data_IAV_R_CA <- read.csv("20251214_IAV_CA_Reww.csv")
data_IAV_R_CO <- read.csv("20251214_IAV_CO_Reww.csv")
data_IAV_R_FL <- read.csv("20251214_IAV_FL_Reww.csv")
data_RSV_R_CA <- read.csv("20251214_RSV_CA_Reww.csv")
data_RSV_R_CO <- read.csv("20251214_RSV_CO_Reww.csv")
data_RSV_R_FL <- read.csv("20251214_RSV_FL_Reww.csv")





#--------------------------------------------------------------------------------------------------------------------------------------Reww

#California
plot_CA <- ggplot(data_RSV_R_CA, aes(x = as.Date(date)))
plot_CA <- plot_CA + geom_ribbon(aes(ymin = low_ocean, ymax = upr_ocean), fill = "#3690C0", alpha = 0.2)  
plot_CA <- plot_CA + geom_ribbon(aes(ymin = low_palo, ymax = upr_palo), fill = "#FEE391", alpha = 0.2)
plot_CA <- plot_CA + geom_ribbon(aes(ymin = low_san, ymax = upr_san), fill = "#BD0026", alpha = 0.2)  
plot_CA <- plot_CA + geom_ribbon(aes(ymin = low_silicon, ymax = upr_silicon), fill = "#016C59", alpha = 0.2)
plot_CA <- plot_CA + geom_ribbon(aes(ymin = low_south, ymax = upr_south), fill = "#6A51A3", alpha = 0.2)  
plot_CA <- plot_CA + geom_ribbon(aes(ymin = low_sunny, ymax = upr_sunny), fill = "#A6BDDB", alpha = 0.2)
plot_CA <- plot_CA + geom_line(aes(y = med_ocean), color = "#0570B0", size = 1) 
plot_CA <- plot_CA + geom_line(aes(y = med_palo), color = "#FEC44F", size = 1) 
plot_CA <- plot_CA + geom_line(aes(y = med_san), color = "#BD0026", size = 1) 
plot_CA <- plot_CA + geom_line(aes(y = med_silicon), color = "#74C476", size = 1) 
plot_CA <- plot_CA + geom_line(aes(y = med_south), color = "#6A51A3", size = 1) 
plot_CA <- plot_CA + geom_line(aes(y = med_sunny), color = "#2B8CBE", size = 1) 
plot_CA <- plot_CA + labs(y = "wastewater-based Re", x = "Month")
plot_CA <- plot_CA + scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot_CA <- plot_CA + scale_y_continuous(limits = c(0.5, 2.0), breaks = seq(0.5, 2.0, 0.5))
plot_CA <- plot_CA + theme_bw()
plot_CA <- plot_CA + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot_CA






#Colorado
plot_CO <- ggplot(data_RSV_R_CO, aes(x = as.Date(date)))
plot_CO <- plot_CO + geom_ribbon(aes(ymin = low_north_1, ymax = upr_north_1), fill = "#3690C0", alpha = 0.3)  
plot_CO <- plot_CO + geom_ribbon(aes(ymin = low_north_2, ymax = upr_north_2), fill = "#FEE391", alpha = 0.4)  
plot_CO <- plot_CO + geom_line(aes(y = med_north_1), color = "#0570B0", size = 1) 
plot_CO <- plot_CO + geom_line(aes(y = med_north_2), color = "#FEC44F", size = 1) 
plot_CO <- plot_CO + labs(y = "wastewater-based Re", x = "Month")
plot_CO <- plot_CO + scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot_CO <- plot_CO + scale_y_continuous(limits = c(0.5, 2.0), breaks = seq(0.5, 2.0, 0.5))
plot_CO <- plot_CO + theme_bw()
plot_CO <- plot_CO + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot_CO





plot_FL <- ggplot(data_RSV_R_FL, aes(x = as.Date(date)))
plot_FL <- plot_FL + geom_ribbon(aes(ymin = low_eastern, ymax = upr_eastern), fill = "#3690C0", alpha = 0.2)  
plot_FL <- plot_FL + geom_ribbon(aes(ymin = low_north, ymax = upr_north), fill = "#FEE391", alpha = 0.2)
plot_FL <- plot_FL + geom_ribbon(aes(ymin = low_south, ymax = upr_south), fill = "#FC9272", alpha = 0.2)  
plot_FL <- plot_FL + geom_line(aes(y = med_eastern), color = "#0570B0", size = 1) 
plot_FL <- plot_FL + geom_line(aes(y = med_north), color = "#FEC44F", size = 1) 
plot_FL <- plot_FL + geom_line(aes(y = med_south), color = "#BD0026", size = 1) 
plot_FL <- plot_FL + labs(y = "wastewater-based Re", x = "Month")
plot_FL <- plot_FL + scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2023-06-01")), date_breaks = "2 months", date_labels = "%b")
plot_FL <- plot_FL + scale_y_continuous(limits = c(0.5, 2.0), breaks = seq(0.5, 2.0, 0.5))
plot_FL <- plot_FL + theme_bw()
plot_FL <- plot_FL + theme(
  axis.line = element_line(linewidth = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(linewidth = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot_FL


#width 350, hight: 500
left_panel <- plot_grid(
  plot_CA, plot_CO, plot_FL,
  ncol = 1,
  align = "v",
  rel_heights = c(1.5, 1.5, 1.5)
)
left_panel




"YlOrRd"
"#FFFFCC" "#FFEDA0" "#FED976" "#FEB24C" "#FD8D3C" "#FC4E2A" "#E31A1C" "#BD0026" "#800026"
"YlOrBr"
"#FFFFE5" "#FFF7BC" "#FEE391" "#FEC44F" "#FE9929" "#EC7014" "#CC4C02" "#993404" "#662506"
"YlGnBu"
"#FFFFD9" "#EDF8B1" "#C7E9B4" "#7FCDBB" "#41B6C4" "#1D91C0" "#225EA8" "#253494" "#081D58"
"YlGn"
"#FFFFE5" "#F7FCB9" "#D9F0A3" "#ADDD8E" "#78C679" "#41AB5D" "#238443" "#006837" "#004529"
"Reds"
"#FFF5F0" "#FEE0D2" "#FCBBA1" "#FC9272" "#FB6A4A" "#EF3B2C" "#CB181D" "#A50F15" "#67000D"
"RdPu"
"#FFF7F3" "#FDE0DD" "#FCC5C0" "#FA9FB5" "#F768A1" "#DD3497" "#AE017E" "#7A0177" "#49006A"
"Purples"
"#FCFBFD" "#EFEDF5" "#DADAEB" "#BCBDDC" "#9E9AC8" "#807DBA" "#6A51A3" "#54278F" "#3F007D"
"PuRd" 
"#F7F4F9" "#E7E1EF" "#D4B9DA" "#C994C7" "#DF65B0" "#E7298A" "#CE1256" "#980043" "#67001F"
"PuBuGn"
"#FFF7FB" "#ECE2F0" "#D0D1E6" "#A6BDDB" "#67A9CF" "#3690C0" "#02818A" "#016C59" "#014636"
"PuBu"
"#FFF7FB" "#ECE7F2" "#D0D1E6" "#A6BDDB" "#74A9CF" "#3690C0" "#0570B0" "#045A8D" "#023858"
#OrRd
"#FFF7EC" "#FEE8C8" "#FDD49E" "#FDBB84" "#FC8D59" "#EF6548" "#D7301F" "#B30000" "#7F0000"
"Oranges"
"#FFF5EB" "#FEE6CE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801" "#A63603" "#7F2704"
"Greys"
"#FFFFFF" "#F0F0F0" "#D9D9D9" "#BDBDBD" "#969696" "#737373" "#525252" "#252525" "#000000"
"Greens"
"#F7FCF5" "#E5F5E0" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D" "#238B45" "#006D2C" "#00441B"
"GnBu"
"#F7FCF0" "#E0F3DB" "#CCEBC5" "#A8DDB5" "#7BCCC4" "#4EB3D3" "#2B8CBE" "#0868AC" "#084081"
"BuPu"
"#F7FCFD" "#E0ECF4" "#BFD3E6" "#9EBCDA" "#8C96C6" "#8C6BB1" "#88419D" "#810F7C" "#4D004B"
"BuGn"
"#F7FCFD" "#E5F5F9" "#CCECE6" "#99D8C9" "#66C2A4" "#41AE76" "#238B45" "#006D2C" "#00441B"
#Blues
"#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"
