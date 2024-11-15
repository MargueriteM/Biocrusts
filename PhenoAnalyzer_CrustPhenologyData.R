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
library(plotly)

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
         datetime = as.POSIXct(paste(date, time, sep=" "),tz="UTC"))


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
# make sure that 1-9 are converted to 01-09
TK <- TK %>%
  mutate(ROI1 = case_when(ROI<10 ~ as.character(paste0("0",ROI))),
         ROI2 = case_when(ROI>=10 ~ as.character(ROI)),
         ROI3 = case_when(is.na(ROI1) ~ ROI2,
                          is.na(ROI2) ~ ROI1),
         camera = tolower(Camera))%>%
  select(camera, ROI3, Sample, SampleID, Site, Type, Water, Rep)

TK <- TK %>%
  rename(ROI=ROI3)

# data on watering dates/times
W<- read_xlsx("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Biocrust_Phenology/Experimental Data/PhenoWaterTrack.xlsx",
              sheet = "in")

# format watering data sheet: 
# change Number to Sample
# drop 'thickness' columns
# make long to calculate sample weight without tin
W.long <- W %>%
  rename(Sample=Number)%>%
  select(!starts_with("Thickness"))%>%
  pivot_longer(cols=!(c(Sample, SampleID, Site, Type, Water, Rep, TinWeight)), names_to="Weight_day_time", values_to="SampleTin_mass_g")%>%
  separate(Weight_day_time,c(NA,"date_time"), sep = "Weight.", extra = "merge", fill = "left", remove =FALSE) %>%
  mutate(WetWeight_g = SampleTin_mass_g-TinWeight,
         date_time=ymd_hm(date_time))

# calculate the difference between sequential water events by day
W.long <- W.long%>%
  arrange(Sample)%>%
  group_by(date(date_time),Sample)%>%
  mutate(WaterContent=c(NA,diff(WetWeight_g)))

# graph sample weight over time
ggplot(W.long, aes(date_time, WetWeight_g, colour=factor(Sample)))+
  geom_point()+
  geom_line()+
  facet_grid(Water~Site)

ggplot(W.long, aes(hour(date_time), WetWeight_g, colour=factor(Sample)))+
  geom_point()+
  geom_line()+
  facet_grid(Water~date(date_time))


# graph moisture change on days with consecutive measurements
na.omit(W.long) %>%
ggplot(., aes(hour(date_time), WaterContent, colour=factor(Sample)))+
  geom_point()+
  geom_line()+
  facet_grid(Water~date(date_time))+
  ylim(c(-2,2))


# make a box plot of the first weight recorded on 2023-04-11 and 2023-04-18 
# BUT ... this is total sample weight so if there is a difference in soil weight by treatment,
# then this isn't a true reflection of water content
W.long %>%
  filter(date_time==ymd_hms("2023-04-11 09:00:00")|date_time==ymd_hms("2023-04-18 09:00:00"))%>%
  ggplot(., aes(factor(date_time), WetWeight_g,fill=Water))+
  geom_boxplot(width=0.5)

# read data on sample thickness
Thick<- read_xlsx("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Biocrust_Phenology/Experimental Data/CrustThickness.xlsx")

# format to long
# calculate mean thickness per sample
Thick <- Thick %>%
  rename(Sample=Number)%>%
  pivot_longer(cols=!(c(Sample, SampleID, Site, Type, Water, Rep)), names_to="ThicknessRep", values_to="Thickness_mm")%>%
  group_by(Sample, SampleID, Site, Type, Water, Rep)%>%
  summarise(Thickness_mean = mean(Thickness_mm))%>%
  mutate(Type = factor(Type, levels=c("L","D","P","C")))

# graph thickness by crust type and site
ggplot(Thick, aes(Site,Thickness_mean,fill=Type))+
  geom_boxplot()
 
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

# times when all samples were positioned in frame, at the begining
# alpha start time >= 14:20 on April 6th 
# bravo start time >= 12:40 on April 6th 
# charlie start time >= 12:40 on April 6th 

# and exclude data befΩ¸¸¸ΩΩore all dishes are in the field of view
pheno_all_filter <- pheno_all_filter %>% 
  mutate(missing_filter = (case_when(camera == "alpha" & datetime < ymd_hms("2023-04-06 14:20:00") ~ "remove",
                                   camera == "bravo" & datetime < ymd_hms("2023-04-06 12:40:00") ~ "remove",
                                   camera == "charlie" & datetime < ymd_hms("2023-04-06 12:40:00") ~"remove",
                         TRUE ~ "keep")))

# Hue maybe best index for filtering times with no sample in camera view

pheno_all_filter %>%
  filter(index=="Hue" ) %>%
  ggplot(., aes(datetime, value, color=camera))+
  geom_point(size=0.5)+
  geom_smooth(method="loess")+
  facet_wrap(Sample~.)+
  labs(y="Hue")

pheno_all_filter %>%
  filter(index=="Hue" ) %>%
  ggplot(., aes(factor(date), value, color=missing_filter))+
  geom_point(size=0.5)+
  facet_wrap(Sample~.)+
  labs(y="Hue")


# calculate median of Hue
hue_median <- pheno_all_filter %>%
  filter(index=="Hue" & missing_filter=="keep") %>%
  group_by(date,camera,Sample)%>%
  summarise(Hue_med = median(value))

# join back to hue median to all data with Hue only
hue_filter<-inner_join(pheno_all_filter%>%filter(index=="Hue")%>%select(date,datetime,camera,Sample,value,missing_filter),hue_median, by=c("date", "camera", "Sample"))

# calculate the distance from median
hue_filter <- hue_filter %>%
  mutate(dist_med = value-Hue_med)


# graph hue with daily median
hue_filter %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=value,color=camera),size=0.5)+
  geom_line(aes(y=Hue_med, colour=camera),linewidth=0.7, colour="black")+
  facet_wrap(Sample~.)+
  labs(y="Hue")


# graph distance from median as timeseries
hue_filter %>%
  filter(Sample >= 51 & Sample <=75 ) %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=dist_med,color=camera),size=0.5,colour="black")+
  geom_hline(yintercept=c(-0.1,0.1))+
  facet_wrap(Sample~.)+
  labs(y="Hue distance from Median")

hue_filter %>%
  filter(Sample>25 & Sample <=50) %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=dist_med,color=camera),size=0.5,colour="black")+
  geom_hline(yintercept=c(-0.1,0.1))+
  facet_wrap(Sample~.)+
  labs(y="Hue distance from Median")

hue_filter %>%
  filter(Sample>50 ) %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=dist_med,color=camera),size=0.5,colour="black")+
  geom_hline(yintercept=c(-0.1,0.1))+
  facet_wrap(Sample~.)+
  labs(y="Hue distance from Median")

hue_filter %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=dist_med,color=camera),size=0.5)+
  geom_hline(yintercept=c(-0.1,0.1))+
  facet_wrap(Sample~.)+
  labs(y="Hue distance from Median")

# mark images to exclude based on criteria
# Use Hue to remove outlying points
hue_filter <- hue_filter %>%
  mutate(missing_filter = case_when(missing_filter == "remove" ~ "remove",
                                    dist_med < -0.1 | dist_med > 0.1 ~ "remove",
                                    TRUE ~ "keep"))

# graph points marked for removal
hue_filter %>%
  filter(Sample<=25) %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=value,color=missing_filter),size=0.5)+
  facet_wrap(Sample~.)+
  labs(y="Hue")

hue_filter %>%
  filter(Sample>25 & Sample <=50) %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=value,color=missing_filter),size=0.5)+
  facet_wrap(Sample~.)+
  labs(y="Hue distance from Median")

hue_filter %>%
  filter(Sample>50 ) %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=value,color=missing_filter),size=0.5)+
  facet_wrap(Sample~.)+
  labs(y="Hue distance from Median")

# merge hue filter  back to whole data set 
pheno_all_filter1<-full_join(pheno_all_filter%>% select(!missing_filter),hue_filter %>% select(!value), by=c("date", "datetime", "camera", "Sample"))


# save hue_filter for manual check of excluded images
# (does not include ROIs 1-9 due to naming issue with 01-09)
# write.table(hue_filter, "ImageFilter_AllCamera.csv",row.names = FALSE, sep=",", dec=".")

# import manual check of hue filter
# (does not include ROIs 1-9 due to naming issue with 01-09)
hue_filter_check <- read.csv("ImageFilter_AllCamera_BAEdit_VisualImageCheck.csv", header=TRUE)

# create a binary 1/0 variable for missing_filter and dish_present
# missing_filter: 1= keep, 0=remove
# dish_present: 1 = yes, 0 = no

hue_filter_check <- hue_filter_check %>%
  mutate(date = mdy(date),
         datetime=mdy_hm(datetime),
         missing_filter_b = case_when(missing_filter == "keep" ~ 1,
                                      missing_filter == "remove" ~ 0),
         dish_present_b = case_when(dish_present == "yes" ~ 1,
                                    dish_present == "no" ~ 0)) 

# count the number of times for keep/yes, keep/no, remove/yes, remove/no
hue_filter_check_count <-  hue_filter_check %>% 
  group_by(missing_filter, dish_present) %>% 
  summarise(number = n()) 


# graph the count of when dish is present and filter says 'keep'
hue_filter_check %>%
  filter(missing_filter == "keep")%>%
ggplot(., aes(dish_present))+
  labs(title="Hue Filter = KEEP ")+
  geom_bar()

# graph the count of when dish is present/absent and filter says 'remove'
hue_filter_check %>%
  filter(missing_filter == "remove")%>%
  ggplot(., aes(dish_present))+
  labs(title="Hue Filter = REMOVE ")+
  geom_bar()


# merge all pheno data with manual dish present notes back to whole data set 
pheno_all_filter1<-full_join(pheno_all_filter1,hue_filter_check %>% select(!c(value,missing_filter)), by=c("date", "datetime", "camera", "Sample"))

# graph 
pheno_all_filter1 %>%
  filter(index=="Hue") %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=value,color=missing_filter),size=0.5)+
  facet_wrap(Sample~.)+
  labs(y="Hue distance from Median")

# graph pctG marked with missing filter
pheno_all_filter1 %>%
  filter(index=="pctG") %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=value,color=missing_filter),size=0.5)+
  facet_wrap(Sample~.)+
  labs(y="GCC with filter")

# graph pctG marked with dish present yes/no
pheno_all_filter1 %>%
  filter(index=="pctG") %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=value,color=dish_present),size=0.5)+
  facet_wrap(Sample~.)+
  labs(y="GCC with filter")

# graph pctG facetted with dish present yes/no and in sample groups
# in general those marked remove should have no dish present, those marked keep should have dish present
pheno_all_filter1 %>%
  filter(index=="pctG"&Sample>=51 & Sample<=75) %>%
  ggplot(., aes(x=datetime))+
  geom_point(aes(y=value,color=missing_filter,shape=dish_present),size=0.5)+
  facet_wrap(.~Sample)+
  scale_color_manual(values=c("black","orange"))+
  scale_shape_manual(values=c(3,20))+
  labs(y="GCC with filter")+
  theme_bw()

# some outliers remain. Graph with plotly to identify & check
ggplotly(pheno_all_filter1 %>%
              filter(index=="pctG" & missing_filter == "keep") %>%
              ggplot(., aes(datetime, value, color=factor(Sample)))+
              geom_point(size=0.5)+
              geom_line()+
              facet_grid(Site~Type)+
              labs(y="percent green"))
 
# based on Hue:
# sample 98 on 2023-04-06 15:00 and 2023-04-07 10:00-15:00
# sample 82 2023-04-24
# sample 75 2023-04-23 and 2024-04-24
# sample 76 2023-04-23 and 2024-04-24

# based on pctG
# * Sample 91 2023-04-08 10:00 (alpha): all dishes missing at 10:00 and most at 11:00 
# *sample 16 2024-04-09 10:00, 2023-04-23 10:00 ( bravo ROI 38, charlie ROI 30; issue is in bravo). No dishes present until 12:00 for 4-09 and 4-24 has no dishes unitl 11:00 and a hand at 11:00
# *sample 20 2024-04-09 10:00, 2023-04-24 10:00: no dishes present at both times, 04-24 has no dishes until 11:00
# *48 2023-04-14 10:00 and 2023-04-16 10:00 and 2023-04-21 10:00 (bravo and charlie): 04-14, 04-16, 04-21 no dishes until 12:00
# Add notes for all dishes missing to Photolist_All for manual filtering *

# manually edit filter to "remove" for 
# ** sample 97 on 2023-04-06 14:40, and 2023-04-07 10:00-15:00 (alpha ): don't filter, dish present! 
# sample 98 on 2023-04-06 14:20, 14:40, 15:00 and 2023-04-07 10:00-15:00 (alpha ROI 1): no dish present! 
# sample 98 on 2023-04-06 15:00 and 2023-04-07 10:00-15:00 (alpha ROI 1): no dish present! 
# sample 94 on 2023-04-24 15:00 (alpha): no dish present!
# 73 04-23 13:00 (alpha): no dish present!
# 69 04-24 12:00 (alpha): no dish present! 
# 64 2024-04-24 10:00 (alpha): no dish present
# 62 2024-04-23 11:00 (alpha): no dish present
pheno_all_filter1 <- pheno_all_filter1 %>%
  mutate(filter_manual = case_when(datetime == ymd_hms("2023-04-06 14:20:00") & Sample==98 ~ "remove",
                                   datetime == ymd_hms("2023-04-06 14:40:00") & Sample==98 ~ "remove",
                                   datetime == ymd_hms("2023-04-06 15:00:00") & Sample==98 ~ "remove",
                                   datetime >= ymd_hms("2023-04-07 10:00:00") & datetime <= ymd_hms("2023-04-07 15:00:00") & Sample==98 ~ "remove",
                                   datetime == ymd_hms("2023-04-24 15:00:00") & Sample==94 ~ "remove",
                                   datetime == ymd_hms("2023-04-23 13:00:00") & Sample==73 ~ "remove",
                                   datetime == ymd_hms("2023-04-24 12:00:00") & Sample==69 ~ "remove",
                                   datetime == ymd_hms("2023-04-24 10:00:00") & Sample==64 ~ "remove",
                                   datetime == ymd_hms("2023-04-23 11:00:00") & Sample==62 ~ "remove",
                                   #datetime == ymd_hms("2023-04-08 10:00:00") & camera=="alpha" ~ "remove",#added to TK notes
                                   #datetime == ymd_hms("2023-04-08 11:00:00") & camera=="alpha" ~ "remove", #added to TK notes
         TRUE ~ "keep"))


ggplotly(pheno_all_filter1 %>%
           filter(index=="pctG" & missing_filter == "keep" & filter_manual =="keep") %>%
           ggplot(., aes(datetime, value, color=factor(Sample)))+
           geom_point(size=0.5)+
           geom_line()+
           facet_grid(Site~Type)+
           labs(y="percent green"))

# graph by Site and Crust type with missing filter removed
pheno_all_filter1 %>%
  filter(index=="pctG" & missing_filter == "keep"  & filter_manual =="keep") %>%
  ggplot(., aes(datetime, value, color=factor(Sample)))+
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(Site~Type)+
  labs(y="percent green")

# graph the hourly pattern by camera
cam.pick <- "charlie"
pheno_all_filter1 %>%
           filter(index=="pctG" & missing_filter == "keep" & filter_manual =="keep"&camera==cam.pick) %>%
           ggplot(., aes(hour(datetime), value, color=factor(Sample)))+
           geom_point(size=0.5)+
           geom_line()+
           facet_wrap(.~date(datetime))+
           labs(y="percent green")

# calculate mean by site and crust type
gcc.mean <-pheno_all_filter1 %>%
  filter(index=="pctG" & missing_filter == "keep"  & filter_manual =="keep") %>%
  group_by(date,Site, Type)%>%
  summarise(gcc.mean = mean(value, na.rm=TRUE),
            gcc.sd = sd(value, na.rm=TRUE), 
            n = n(),
            gcc.se = gcc.sd/sqrt(n))%>%
  mutate(Type = factor(Type, levels=c("L","D","P","C")))

# graph means
ggplot(gcc.mean, aes(date, gcc.mean, colour=Type))+
  geom_point(size=0.5)+
  geom_line(linewidth=0.3)+
  geom_errorbar(aes(ymin=gcc.mean-gcc.se, ymax=gcc.mean+gcc.se))+
  scale_x_date(date_breaks="5 days", date_minor_breaks = "1 day",date_labels= "%b %d")+
  facet_grid(Site~Type)+
  labs(y="mean GCC")

# graph means grouped only by site
ggplot(gcc.mean, aes(date, gcc.mean, colour=Type))+
  geom_point(size=0.5)+
  geom_line(linewidth=0.3)+
  geom_errorbar(aes(ymin=gcc.mean-gcc.se, ymax=gcc.mean+gcc.se))+
  geom_vline(xintercept=as.Date("2023-04-23"), linetype="dashed", colour="dark grey")+
  scale_x_date(date_breaks="5 days", date_minor_breaks = "1 day",date_labels= "%b %d")+
  facet_grid(.~Site)+
  labs(y="mean GCC")+
  theme_bw()

# create a dataframe of all points marked for removal to cross-check with photos
# once with all images and samples and once only per image
pheno_all_remove_Sample <- pheno_all_filter1 %>%
  filter(missing_filter == "remove") %>%
  distinct(img,Sample)

pheno_all_remove_img <- pheno_all_filter1 %>%
  filter(missing_filter == "remove") %>%
  distinct(img)

# save as "ImageFilter_AllCamera_VisualImageCheck.csv" to ./Phenoanalyzer folder

# save data with missing_filter == "keep"  & filter_manual =="keep"
# save all indices and pctG only

pheno_all_analysis <- pheno_all_filter1 %>%
  filter(missing_filter == "keep"  & filter_manual =="keep")%>%
  select(img,datetime,year,month,day,hour,minute,camera,ROI,Sample,Site,Type,Water,Rep,colorspace,index,value)

pheno_all_analysis_pctg <- pheno_all_filter1 %>%
  filter(index=="pctG" & missing_filter == "keep"  & filter_manual =="keep") %>%
  select(img,datetime,year,month,day,hour,minute,camera,ROI,Sample,Site,Type,Water,Rep,colorspace,index,value)


# graph to check
pheno_all_analysis %>%
  filter(index=="pctG") %>%
  ggplot(., aes(datetime, value, color=factor(Sample)))+
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(Site~Type)+
  labs(y="percent green")

pheno_all_analysis_pctg %>%
  ggplot(., aes(datetime, value, color=factor(Sample)))+
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(Site~Type)+
  labs(y="percent green")

# save to ./PhenoAnalyzer
# write.table(pheno_all_analysis, "PhenoAnalyzer_Filtered_AllIndices.csv", sep=",",row.names=FALSE)
# write.table(pheno_all_analysis_pctg, "PhenoAnalyzer_Filtered_pctG.csv", sep=",",row.names=FALSE)

