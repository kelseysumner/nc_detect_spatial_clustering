# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Zip Code Influenza Data         #
#              July 18, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

# what this is doing: 
# reading in the naloxone geocoded data set, merging with census data at the tract level

#### ------- load the libraries ---------- ####

# load in tidyverse and geospatial libraries (sf)
library(tidyverse)
library(GISTools)
library(rgdal)
library(foreign)
library(zipcode)
library(readxl)
library(sp)


#### -------- load in the data sets -------- ####

# set working directory
setwd("C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data")

# read in the data sets
# first the cc and triage notes
data_ccandtriagenotes = read_csv("ILIbyZIP_ccandtriagenotes.csv") %>%
  mutate(zip = str_pad(as.character(ZIP), width = 5, side = "left", pad = "0")) %>%
  dplyr::select(-ZIP)
colnames(data_ccandtriagenotes) = c("Count", "visitdate", "zip")
# then the cc only one
data_cconly = read_csv("ILIbyZIP_cconly.csv") %>%
  mutate(zip = str_pad(as.character(ZIP), width = 5, side = "left", pad = "0")) %>%
  dplyr::select(-ZIP)
colnames(data_cconly) = c("Count", "visitdate", "zip")


# look at quick summaries of both data sets
table(nchar(data_ccandtriagenotes$zip))
table(nchar(data_cconly$zip))
table(data_ccandtriagenotes$zip)
table(data_cconly$zip)



#### ----- add zip codes latitude and longitude coordinates for satscan ------- ####

# cc and triage notes data set
#add lat and long based on zip code matches - matches using zipcode package, remove nonmatches
data("zipcode")
clean_data_ccandtriagenotes = data_ccandtriagenotes %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.)))
#write to csv for import into SaTScan as case and coordinates files
clean_data_ccandtriagenotes %>%
  write.csv("clean_ILIbyZIP_ccandtriagenotes.csv")


# cc only data set
#add lat and long based on zip code matches - matches using zipcode package, remove nonmatches
data("zipcode")
clean_data_cconly = data_cconly %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.)))
#write to csv for import into SaTScan as case and coordinates files
clean_data_cconly %>%
  write.csv("clean_ILIbyZIP_cconly.csv")


