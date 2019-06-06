# ----------------------------------------------- #
#          NC DETECT Spatial Project              #
#           Naloxone qSum data                    #
#              June 6, 2019                       #
#                K. Sumner                        #
# ----------------------------------------------- #

# what this is doing: 
# reading in the NC DETECT naloxone qsum cluster data and adding lat/long to it and making it a shapefile for comparison 


#### ------- load the libraries ---------- ####

# load in the libraries of interest
library(tidyverse)
library(sf)
library(tidycensus)
library(GISTools)
library(rgdal)
library(foreign)


#### ------- read in the data sets ---------- ####

# set working directory
setwd("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data")

# read in the cleaned naloxone data with the lat and long centroid pulled out
qsum_data = read_csv("EMSNaloxone2017annotationOverview_qsum.csv")

# find latitude and longitude for the counties
# get spatial data from tidy census
nc_sf = tidycensus::get_acs(geography = "county", state = "NC", 
                            variables = "B19013_001",
                            summary_var = "B01001_001", geometry = TRUE,  
                            key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0") %>%
  rename(totpop = estimate)
plot(nc_sf %>% st_geometry()) 

# create a new variable that is just the county name for nc_sf
Location = rep(NA,nrow(nc_sf))
for (i in 1:nrow(nc_sf)){
  Location[i] = str_split(nc_sf$NAME[i]," ")[[1]][1]
}
table(Location, useNA = "always")
nc_sf$Location = Location
# change new to new hanover
nc_sf$Location[which(nc_sf$Location=="New")]="New Hanover"
# cherokee tribal gets dropped - is this different than Cherokee?
# for now, recode cherokee tribal as cherokee in the qsum data set
qsum_data$Location[which(qsum_data$Location=="Cherokee Tribal")]="Cherokee"

# make sure all the data sets have geoid coded correctly
nc_sf$Location = as.character(nc_sf$Location)
qsum_data$Location = as.character(qsum_data$Location)

# look at how many entries are in your data set for geoid
length(unique(nc_sf$Location)) # 100, good
length(unique(qsum_data$Location)) # only 57

# first merge in the data set to the spatial ones
# nc_sf
sf_merged = left_join(qsum_data,nc_sf,by="Location")
colnames(sf_merged)
length(unique(sf_merged$Location)) # 57, good
sf_merged = sf_merged[which(!(is.na(sf_merged$GEOID))),] # kept all
length(unique(sf_merged$Location)) # 57, good

# write out the sf_merged object as a shapefile
#st_write(sf_merged, "qsum_data_county_level_shapefile_by_day.shp")












