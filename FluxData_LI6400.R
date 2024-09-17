#####################################################################
#         Import and prepare LI-6400 data for analysis              #
#                                                                   #
#                    written by M. Mauritz                          #
#                       17 September 2024                           #
#                 for JRN Seed, REU, and NRCS project               #
#####################################################################

# INSTRUCTIONS FOR INPUT FILES
# This script can read processed excel files from the LI-6400
# to prepare files, take raw excel files, remove all comments rows
# each file must start with header rows (do not modify or delete anything)
# add logfile to first column before obs and add SampleID column after HHMMSS column
# prepare a file named HeaderNames.xlsx that contains reformatted column names in the first row. These must be in the EXACT SAME order as the prepared file and should have no spaces or special symbols like '/' or ':'
# prepare a file that links Sample ID with sample description like crust type, site, etc. The name of the SampleID column must match the flux file
# each file must start with header rows (do not modify or delete anything)

# append sample ID information
# add sample-specific area and recalculate photosynthesis
# graph light response curves by sample ID and other descriptors

# load libraries
library(ggplot2)
library(data.table)
library(readxl)
library(dplyr)
library(tidyverse)

# Read processed files
# Keep all files in one folder to read and bind together
# all file names include path, but could also set working directory
# set working directory to folder with processed files (all comments removed from raw files, logfile and sampleID added)
# setwd("/Users/memauritz/Downloads/CFlux_Licor6400/RReady")

# list all the files in the directory
flux.files <- list.files(path="/Users/memauritz/Downloads/CFlux_Licor6400/RReady", full.names=TRUE, pattern="_process") 

# read the header names file
flux.colnames <- colnames(read_excel("/Users/memauritz/Downloads/CFlux_Licor6400/RReady/HeaderNames.xlsx"))

# read the sample ID file with descriptions and crust area
sampleids <- read_excel("/Users/memauritz/Downloads/DataSheets_SampleIDs_DataEntry.xlsx", sheet = "CompleteRandomList")

# read all flux data files and combine into one, rename columns using HeaderNames File
flux.dat <- flux.files %>%
  map_dfr(~read_excel(.x)) %>%
  setNames(flux.colnames) 

# see if column names have been renamed
names(flux.dat)

# format the HHMMSS column into a time variable
flux.dat <- flux.dat %>%
  mutate(time = parse_time(HHMMSS, format="%H:%M:%S"))


# merge with sampleids
flux.dat <- left_join(flux.dat, sampleids, by="SampleID")


# calculate photosynthesis with adjusted area
flux.dat <- flux.dat %>%
  mutate(fda.adjust = (Flow*1*10^-6)/(Area*10^-4),
         Photo.adjust = (CO2R-(CO2S*(1000-H2OR)/(1000-H2OS)))*fda.adjust)

# graph to check adjusted photosynthesis calculation
ggplot(flux.dat, aes(Photo,Photo.adjust))+
    geom_point()+
 geom_abline(intercept=0,slope=1)

# Graph Data

# Light response curves by sampleID
ggplot(flux.dat, aes(PARi,Photo,colour=factor(SampleID)))+
  geom_point()+
  geom_line()+
  facet_wrap(logfile~.)

# Light response curves by sampleID from only one of the logfiles
flux.dat %>%
  filter(logfile=="20240620_REU_3")%>%
ggplot(., aes(PARi,Photo,colour=factor(SampleID)))+
  geom_point()+
  geom_line()+
  facet_wrap(logfile~.)

# Light response curves by site and crust type
ggplot(flux.dat, aes(PARi,Photo,colour=factor(SampleID)))+
  geom_point()+
  geom_line()+
  facet_grid(Site~CrustType)

# Light response curves by SoilType and crust type
ggplot(flux.dat, aes(PARi,Photo,colour=factor(SampleID)))+
  geom_point()+
  geom_line()+
  facet_grid(CrustType~SoilType)

# Graph the C match delta
ggplot(flux.dat, aes(time,CsMch,colour=factor(SampleID)))+
  geom_point()+
  geom_line()+
  facet_wrap(logfile~.)



