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
TK <- TK %>%
  mutate(ROI = as.factor(ROI),
         camera = tolower(Camera))%>%
  select(camera, ROI, Sample, SampleID, Site, Type, Water, Rep)

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

# and exclude data before all dishes are in the field of view
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
  filter(Sample <= 25 ) %>%
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

# merge hue filter back to whole data set 
pheno_all_filter1<-inner_join(pheno_all_filter %>% select(!missing_filter),hue_filter %>% select(!value), by=c("date", "datetime", "camera", "Sample"))

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


# graph by Site and Crust type with missing filter removed
pheno_all_filter1 %>%
  filter(index=="pctG" & missing_filter == "keep") %>%
  ggplot(., aes(datetime, value, color=factor(Sample)))+
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(Site~Type)+
  labs(y="percent green")


# calculate mean by site and crust type
gcc.mean <-pheno_all_filter1 %>%
  filter(index=="pctG" & missing_filter == "keep") %>%
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

# save files for further use
write.table(hue_filter, "ImageFilter_AllCamera.csv",row.names = FALSE, sep=",", dec=".")




