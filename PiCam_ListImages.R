##################################################################
# This code creates lists of photos from the Crust Phenlogy Project
# from May 2023
# Project people: Nicole Pietrasiak, Mikaela Hoellrich, Brenda Almanza, Marguerite Mauritz, Maya Gabitzsch
# Code started by M. Mauritz, 25 May 2023
##################################################################

# create a list of all files present in Alpha directory (linked One Drive folder)
alphalist <- list.files("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Biocrust_Phenology/Experimental Data/PiCam/Pictures ABC/alfa/bare")

# set working directory to PhotoList folder in linked One Drive
setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Biocrust_Phenology/Experimental Data/PiCam/Pictures ABC/PhotoLists")
# save the list with column name
write.table(alphalist, 'photolist_alpha.csv',
            row.names = FALSE,col.names = "ImageName",
            sep=",")
