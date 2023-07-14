# Read and organise Phenoanalyzer output 
# 14 July 2023
# Marguerite Mauritz

library(ggplot2)
library(dplyr)
library(tidyr)

setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Biocrust_Phenology/Experimental Data/PhenoAnalyzer")

bravo.pheno <- read.csv("BRAVO.csv", header=TRUE)

#pivot wide to long so all indices are in one column with column name as label
bravo.pheno1 <- bravo.pheno %>%
  pivot_longer(cols = -c("img", "idx_label"), names_to = "descriptor")

#split descriptor col into colorspace, ROI, index data 
bravo.pheno1<- bravo.pheno1 %>%
  separate(descriptor,c("colorspace","ROI","index"), sep = "\\.", extra = "merge", fill = "left", remove =FALSE) %>%
  mutate(across(c(colorspace, ROI, index),factor))

#Remove "ROI" from ROI_#
bravo.pheno1<- bravo.pheno1 %>%
  separate(ROI, c(NA, "ROI"), sep = "_") %>%  
  mutate(across(c(ROI),factor))

# split img col into year, month, day, time and remove '.B.lux.png' from time
# split time into hour and minute
# then create a formatted date and time into hour and minute
bravo.pheno1<- bravo.pheno1 %>%
  separate(img,c("year","month","day","time"), sep = "\\ ", extra = "merge", fill = "left", remove =FALSE) %>%
  separate(time, c("time",NA), sep = ".B") %>%
  separate(time,c("hour","minute"),sep="\\.", extra = "drop", fill = "left", remove=TRUE) %>%
  mutate(date = as.Date(paste(year, month, day, sep="-")),
         time = paste(hour,minute,sep=":"),
         datetime = as.POSIXct(paste(date, time, sep=" ")))

# MODIFY CODE TO USE ROI TO LABEL IMAGES
bravo.pheno1 <- bravo.pheno1 %>%
  mutate(site = case_when(ROI %in% c("01","02","03","07","08","09","13","14","15","19","20","21") ~ "A_Mountain",
                          TRUE ~ "Jornada_Tarbush"),
         crust = case_when(ROI %in% c("01","02","03","04","05","06","07","08","09","10","11","12") ~ "Cyano",
                           TRUE ~ "Lichen"))

# graph GCC 
bravo.pheno1 %>%
  filter(index=="pctG")%>%
  ggplot(., aes(idx_label, value, colour=ROI))+
  geom_point()+
  geom_line()

# MODIFY ACCORDING TO EXPERIMENT
 # facet_grid(crust~site)+
 # labs(title="GCC from dry to T72hr", y="GCC", x="Time")+
 # theme_bw()


# graph RCC
bravo.pheno1 %>%
  filter(index=="pctR")%>%
  ggplot(., aes(idx_label, value, colour=ROI))+
  geom_point()+
  geom_line()

# MODIFY ACCORDING TO EXPERIMENT
# facet_grid(crust~site)+
# labs(title="RCC from dry to T72hr", y="GCC", x="Time")+
# theme_bw()


# graph nNDVI
bravo.pheno1 %>%
  filter(index=="nNDVI")%>%
  ggplot(., aes(idx_label, value, colour=ROI))+
  geom_point()+
  geom_line()

# MODIFY ACCORDING TO EXPERIMENT
# facet_grid(crust~site)+
# labs(title="nNDVI from dry to T72hr", y="GCC", x="Time")+
# theme_bw()


