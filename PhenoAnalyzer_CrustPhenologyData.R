# Read and organise Phenoanalyzer output 
# for lab experiment in April 2023
# Experiment run by: Mikaela Hoellrich and Brenda Almanza
# Advised by: Nicole Pietrasiak and Marguerite Mauritz
# Script: 
# 14 July 2023
# Marguerite Mauritz and Mikaela Hoellrich

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)

setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Biocrust_Phenology/Experimental Data/PhenoAnalyzer")

# read data from each camera separately
alpha.pheno <- read.csv("AlphaALL.csv", header=TRUE)
bravo.pheno <- read.csv("BRAVO.csv", header=TRUE)
charlie.pheno <- read.csv("CharlieALL.csv", header=TRUE)

#pivot wide to long so all indices are in one column with column name as label
# add camera label to each
alpha.pheno1 <- alpha.pheno %>%
  pivot_longer(cols = -c("img", "idx_label"), names_to = "descriptor")%>%
  mutate(camera="alpha")

bravo.pheno1 <- bravo.pheno %>%
  pivot_longer(cols = -c("img", "idx_label"), names_to = "descriptor")%>%
  mutate(camera="bravo")

charlie.pheno1 <- charlie.pheno %>%
  pivot_longer(cols = -c("img", "idx_label"), names_to = "descriptor")%>%
  mutate(camera="charlie")

# combine all three camera frames into one
pheno_all <- rbind(alpha.pheno1, bravo.pheno1, charlie.pheno1)

#split descriptor col into colorspace, ROI, index data 
pheno_all <- pheno_all %>%
  separate(descriptor,c("colorspace","ROI","index"), sep = "\\.", extra = "merge", fill = "left", remove =FALSE) %>%
  mutate(across(c(colorspace, ROI, index),factor))

#Remove "ROI" from ROI_#
pheno_all<- pheno_all %>%
  separate(ROI, c(NA, "ROI"), sep = "_") %>%  
  mutate(across(c(ROI),factor))


# split img col into year, month, day, time and remove '.B.lux.png' from time
# split time into hour and minute
# then create a formatted date and time into hour and minute
pheno_all<- pheno_all %>%
  separate(img,c("year","month","day","time"), sep = "\\ ", extra = "merge", fill = "left", remove =FALSE) %>%
  separate(time, c("time",NA), sep = ".B") %>%
  separate(time,c("hour","minute"),sep="\\.", extra = "drop", fill = "left", remove=TRUE) %>%
  mutate(date = as.Date(paste(year, month, day, sep="-")),
         time = paste(hour,minute,sep=":"),
         datetime = as.POSIXct(paste(date, time, sep=" ")))


# checks
# look at number of records per camera, ROI, and index
camchecks <- pheno_all %>%
  group_by(camera, ROI, date,index)%>%
  summarise(records = length(value))

# check records only for pctG ... should be the same for each index
camchecks %>% 
  filter(index=="pctG") %>%
  ggplot(., aes(date, records))+
  geom_point()+
  facet_grid(camera~ROI)

# import list of ROI and sample IDs
# FOR MERGING #
TK <- read.csv("PhenoAnaTracking.csv", header=TRUE)

# convert ROI to factor and camera to lower case to match column type in pheno_all
TK <- TK %>%
  mutate(ROI = as.factor(ROI),
         camera = tolower(Camera))%>%
  select(camera, ROI, Sample, SampleID, Site, Type, Water, Rep)

# data on watering dates/times
W<- read_xlsx("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Biocrust_Phenology/Experimental Data/PhenoWaterTrack.xlsx",
              sheet = "in")

# format watering date sheet for merging
# corrected typo in DateTime_yyyymmdd_hhmm column in original sheet
# 202030424 to 20230424
W <- W %>%
  rename(Sample=Number)%>%
  mutate(date_time = ymd_hm(DateTime_yyyymmdd_hhmm))
 
# import image lists for caps on/off amd no crusts in picture
img_notes<- read.csv("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Biocrust_Phenology/Experimental Data/PiCam/Pictures ABC/PhotoLists/Photolist_ALL.csv")
 
# format data for merging and add .lux to the images for alpha and bravo
img_notes <- img_notes %>%
  rename(img=ImageName, camera=Camera) %>%
  mutate(camera = tolower(camera), 
         ) %>%
  select(img,Use, Notes)
  

# join phenoanalyser data with sample ID for each ROI (TK), moisture data, and sample thickness
pheno_all<-inner_join(pheno_all,TK, by=c("ROI","camera"))

# join phenoanalyser data with image notes to exclude caps on/off amd no crusts in picture
pheno_all<-inner_join(pheno_all,img_notes, by=c("img"))


# join phenoanalyser data with water content and crust thickness

# check for sample overlap
ggplot(pheno_all, aes(camera, Sample))+
         geom_point()

# list indices by colorspace
# RGB       HSV                 Lab
# 1 redDN     1 Hue             1 L. 
# 2 greenDN   2 Saturation      2 a.
# 3 blueDN    3 Value           3 b. 
# 4 2G_RBi                      4 test.L.a...a..1..L...2550.
# 5 pctR    
# 6 pctG    
# 7 pctB    
# 8 nNDVI   
# 9 totalRGB
# 10 AnyColor

# plot pctG by date and Sample number colored by camera
pheno_all %>%
  filter(index=="pctG") %>%
ggplot(., aes(datetime, value, color=camera))+
  geom_point()+
  facet_wrap(Sample~.)+
  labs(title="Percent Green by Sample", y="GCC (%)", x="Date and Time of Image")


# graph colored by 'Use' column
pheno_all %>%
  filter( index=="Hue") %>%
  ggplot(., aes(datetime, value, colour=factor(Use)))+
  geom_point()+
  facet_grid(camera~.)+
  labs(title = "Mark to exclude using notes", y="Hue")

# exclude data when Use==0
pheno_all_filter <- pheno_all%>%
  filter(Use==1)

# explore indices for filtering times with no sample in camera view
pheno_all_filter %>%
  filter(index=="greenDN") %>%
  ggplot(., aes(datetime, value, color=camera))+
  geom_point(size=0.5)+
  geom_line()+
  facet_wrap(Sample~.)+
  labs(y="Green DN")


pheno_all_filter %>%
  filter(index=="pctG") %>%
  ggplot(., aes(datetime, value, color=camera))+
  geom_point(size=0.5)+
  geom_line()+
  facet_wrap(Sample~.)+
  labs(y="percent green")

pheno_all_filter %>%
  filter(index=="pctR") %>%
  ggplot(., aes(datetime, value, color=camera))+
  geom_point(size=0.5)+
  geom_line()+
  facet_wrap(Sample~.)+
  labs(y="percent red")

pheno_all_filter %>%
  filter(index=="Hue") %>%
  ggplot(., aes(datetime, value, color=camera))+
  geom_point(size=0.5)+
  geom_smooth(method="loess")+
  facet_wrap(Sample~.)+
  labs(y="Hue")

pheno_all_filter %>%
  filter(index=="Saturation") %>%
  ggplot(., aes(datetime, value, color=camera))+
  geom_point(size=0.5)+
  geom_line()+
  facet_wrap(Sample~.)+
  labs(y="Saturation")

# mark images to exclude based on criteria
# CHECK CODE FOR FITTING FUNCTIONS
loess.test <- pheno_all_filter %>%
  filter(index=="Hue") %>%
  group_by(camera, Sample) %>%
  summarise(loessfx = loess(value~datetime))

# graph by Site and Crust type
pheno_all %>%
  filter(index=="pctG") %>%
  ggplot(., aes(datetime, value, color=factor(Sample)))+
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(Site~Type)+
  labs(y="percent green")


pheno_all %>%
  filter(index=="pctR") %>%
  ggplot(., aes(datetime, value, color=factor(Sample)))+
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(Site~Type)+
  labs(y="percent red")

