setwd("C:/WBE_ND")


library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(ggplot2)

data_RSV <- read.csv("20240627_RSV.csv")
data_IAV <- read.csv("20240627_IAV.csv")

data <- data_RSV %>% mutate(collection_date = as.Date(Collection_Date))
#NDを500 gc/gととする(論文より)
data <- data %>% mutate(RSV_gc_g_dry_weight = if_else(RSV_gc_g_dry_weight == 0, 500, RSV_gc_g_dry_weight))

data <- select(data, site_name, Plant, collection_date, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight) #IAV, RSV
#Plant <- table(data$Plant)
#print(Plant)

date_range <- seq(as.Date("2022/1/1"), as.Date("2023/8/1"), by = "days")
new_dates <- data.frame(collection_date = date_range)

#california
data_CA <- subset(data, grepl("CA", Plant)) #data_CA <- filter(data, State == "CA")
site_name_CA <- table(data_CA$site_name)
#print(site_name_CA)
data_palo_alto <- filter(data_CA, site_name == "Palo Alto Regional Water Quality Control Plant")####
data_silicon_valley <- filter(data_CA, site_name == "Silicon Valley Clean Water") ####
data_san_jose <- filter(data_CA, site_name == "San Jose-Santa Clara Regional Wastewater Facility")#####
data_southeast_san_francisco <- filter(data_CA, site_name == "Southeast San Francisco")#####
data_sunnyvale <- filter(data_CA, site_name == "City of Sunnyvale Water Pollution Control Plant")#####
data_oceanside <- filter(data_CA, site_name == "Oceanside Water Pollution Control Plant")#####


#日付の調整
merged_17 <- merge(new_dates, data_sunnyvale, by = "collection_date", all = TRUE)####south
merged_37 <- merge(new_dates, data_oceanside, by = "collection_date", all = TRUE)####south
merged_38 <- merge(new_dates, data_palo_alto, by = "collection_date", all = TRUE)####south
merged_43 <- merge(new_dates, data_san_jose, by = "collection_date", all = TRUE)####north
merged_47 <- merge(new_dates, data_silicon_valley, by = "collection_date", all = TRUE)####south
merged_51 <- merge(new_dates, data_southeast_san_francisco, by = "collection_date", all = TRUE)####south

#列名の変更と横に繋げるようにデータを編集
combine_17 <- select(merged_17, collection_date, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_17) <- c("collection_date", "bcov_sunnyvale", "RSV_sunnyvale", "PMMoV_sunnyvale")
combine_37 <- select(merged_37, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_37) <- c("bcov_oceanside", "RSV_oceanside", "PMMoV_oceanside")
combine_38 <- select(merged_38, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_38) <- c("bcov_palo_alto", "RSV_palo_alto", "PMMoV_palo_alto")
combine_43 <- select(merged_43, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_43) <- c("bcov_san_jose", "RSV_san_jose", "PMMoV_san_jose")
combine_47 <- select(merged_47, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_47) <- c("bcov_silicon_valley", "RSV_silicon_valley", "PMMoV_silicon_valley")
combine_51 <- select(merged_51, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_51) <- c("bcov_southeast_san_francisco", "RSV_southeast_san_francisco", "PMMoV_southeast_san_francisco")


data_combined <- cbind(combine_17,combine_37, combine_38, combine_43, combine_47, combine_51) #
write.csv(x = data_combined, file = "C:/WBE_ND/20240820_RSV_CA.csv")


#Georgia
data_GA <- subset(data, grepl("GA", Plant))
site_name_GA <- table(data_GA$site_name)
#print(site_name_GA)
data_big <- filter(data_GA, site_name == "Big Creek Water Reclamation Facility")
data_camp <- filter(data_GA, site_name == "Camp Creek Water Reclamation Facility")
data_john <- filter(data_GA, site_name == "Johns Creek Environmental Campus")
data_little <- filter(data_GA, site_name == "Little River Water Reclamation Facility")

#日付の調整
merged_1 <- merge(new_dates, data_big, by = "collection_date", all = TRUE)
merged_2 <- merge(new_dates, data_camp, by = "collection_date", all = TRUE)
merged_3 <- merge(new_dates, data_john, by = "collection_date", all = TRUE)
merged_4 <- merge(new_dates, data_little, by = "collection_date", all = TRUE)


#列名の変更と横に繋げるようにデータを編集
combine_1 <- select(merged_1, collection_date, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_1) <- c("collection_date", "bcov_sunnyvale", "RSV_sunnyvale", "PMMoV_sunnyvale")
combine_2 <- select(merged_2, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_2) <- c("bcov_oceanside", "RSV_oceanside", "PMMoV_oceanside")
combine_3 <- select(merged_3, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_3) <- c("bcov_palo_alto", "RSV_palo_alto", "PMMoV_palo_alto")
combine_4 <- select(merged_4, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_4) <- c("bcov_san_jose", "RSV_san_jose", "PMMoV_san_jose")

data_combined <- cbind(combine_1,combine_2, combine_3, combine_4)
write.csv(x = data_combined, file = "C:/WBE_ND/20240820_RSV_GA.csv")



#Florida
data_FL <- subset(data, grepl("FL", Plant))
site_name_FL <- table(data_FL$site_name)
#print(site_name_FL)
data_easter <- filter(data_FL, site_name == "Eastern Water Reclamation Facility")
data_north <- filter(data_FL, site_name == "Northwest Water Reclamation Facility")
data_south <- filter(data_FL, site_name == "South Water Reclamation Facility")


#日付の調整
merged_5 <- merge(new_dates, data_easter, by = "collection_date", all = TRUE)
merged_6 <- merge(new_dates, data_north, by = "collection_date", all = TRUE)
merged_7 <- merge(new_dates, data_south, by = "collection_date", all = TRUE)


#列名の変更と横に繋げるようにデータを編集
combine_5 <- select(merged_5, collection_date, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_5) <- c("collection_date", "bcov_sunnyvale", "RSV_sunnyvale", "PMMoV_sunnyvale")
combine_6 <- select(merged_6, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_6) <- c("bcov_oceanside", "RSV_oceanside", "PMMoV_oceanside")
combine_7 <- select(merged_7, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_7) <- c("bcov_palo_alto", "RSV_palo_alto", "PMMoV_palo_alto")

data_combined <- cbind(combine_5,combine_6, combine_7)
write.csv(x = data_combined, file = "C:/WBE_ND/20240820_RSV_FL.csv")


#Colorado
data_CO <- subset(data, grepl("CO", Plant))
site_name_CO <- table(data_CO$site_name)
#print(site_name_CO)
data_north_CO <- filter(data_CO, site_name == "Parker Water and Sanitation District North Water Reclamation Facility")
data_south_CO <- filter(data_CO, site_name == "Parker Water and Sanitation District South Water Reclamation Facility")

#日付の調整
merged_8 <- merge(new_dates, data_north_CO, by = "collection_date", all = TRUE)
merged_9 <- merge(new_dates, data_south_CO, by = "collection_date", all = TRUE)


#列名の変更と横に繋げるようにデータを編集
combine_8 <- select(merged_8, collection_date, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_8) <- c("collection_date", "bcov_sunnyvale", "RSV_sunnyvale", "PMMoV_sunnyvale")
combine_9 <- select(merged_9, bcov_recovery, RSV_gc_g_dry_weight, PMMoV_gc_g_dry_weight)
colnames(combine_9) <- c("bcov_oceanside", "RSV_oceanside", "PMMoV_oceanside")

data_combined <- cbind(combine_8,combine_9)
write.csv(x = data_combined, file = "C:/WBE_ND/20240820_RSV_CO.csv")

