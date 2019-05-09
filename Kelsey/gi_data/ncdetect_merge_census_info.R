# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Vomiting/Diarrhea Data          #
#    Trying to Merge in Census Data in R    #
#            Mike Fliss's Method            #
#               May 9, 2019                 #
#                K. Sumner                  #
# ----------------------------------------- #


# what this is doing: 
# reading in the gi data set by zip code at the monthly level, merging with census data, and making spark plots from

#### ------- load the libraries ---------- ####

# load in tidyverse and geospatial libraries (sf)
library(tidyverse)
library(sf)
library(tidycensus)
library(GISTools)
library(rgdal)
library(foreign)


#### -------- load in the data sets -------- ####

# set working directory
setwd("/Users/kelseysumner/Desktop/NC DETECT")

# read in the dbf file for the SE region shapefile
gi_merged_data = read.dbf("Shapefiles/zip_seregion_with_gi_data/gi_data_seregion_by_crossection.dbf")

# read in the zip code gi symptom count data at the monthly level
ncd = read_csv("GI visit data/gi_case_data_by_month.csv")


#### ------ merge gi count data with census data --------- ####

# make sure the gi count data is coded correctly
str(ncd$zip_clean)
str(ncd$month_year)
str(ncd$ed_count_vomd_for_month)
# recode zip clean to a character
ncd$zip_clean = as.character(ncd$zip_clean)
str(ncd$zip_clean)

# make a list of all the se region zip codes of interest from the dbf file
# zip codes for: NC, Georgia, Tennessee, SC, and Virginia

# Get population data from tidy census 
this_data = tidycensus::get_acs(geography = "zcta",
                                survey="acs5", year=2017, variables = c("B03002_001E", "B03002_003E", "B03002_004E"), 
                                key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0", output = "wide")
this_data = this_data %>%
  rename(totpop = B03002_001E, WnH = B03002_003E, BnH = B03002_004E) 


# get spatial data from tidy census
nc_sf = tidycensus::get_acs(geography = "zip code tabulation area", 
                                   variables = "B19013_001",
                                   summary_var = "B01001_001", geometry = TRUE,  
                                   key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0") %>%
  rename(totpop = estimate)
plot(nc_sf %>% st_geometry()) 
# note that for faster plotting, consider simplifying the spatial geometry - you don't need foot-level resolution. 
# Look at st_simplify() or https://mapshaper.org/ or rmapshaper.

# make sure the this_data is coded correctly
str(this_data$GEOID)
str(this_data$NAME)
str(this_data$totpop)
str(this_data$B03002_001M)
str(this_data$WnH)
str(this_data$B03002_003M)
str(this_data$BnH)
str(this_data$B03002_004M)

# make sure nc_sf is coded correctly
str(nc_sf$GEOID)
str(nc_sf$NAME)
str(nc_sf$variable)
str(nc_sf$totpop)
str(nc_sf$moe)
str(nc_sf$summary_est)
str(nc_sf$summary_moe)

# make sure gi_merged_data is coded correctly
str(gi_merged_data$OBJECTID)
str(gi_merged_data$ZIP)
gi_merged_data$ZIP = as.character(gi_merged_data$ZIP)
str(gi_merged_data$PO_NAME)

# now subset nc_sf to just the geoids that are in the zip code dbf file
nc_sf_seregion = nc_sf %>%
  filter(nc_sf$GEOID %in% gi_merged_data$ZIP)
# check the subset
length(unique(gi_merged_data$ZIP))
length(which(nc_sf$GEOID %in% gi_merged_data$zip_clean))
length(intersect(nc_sf$GEOID,gi_merged_data$ZIP))
length(which(nc_sf$GEOID == "28671"))

# now subset this_data to just the geoid that are in the zip code dbf file
this_data_seregion = this_data %>%
  filter(this_data$GEOID %in% gi_merged_data$ZIP)
# check the subset
length(unique(gi_merged_data$ZIP))
length(which(this_data_seregion$GEOID %in% gi_merged_data$ZIP))

# check how many zip codes are in both the nc detect data and the gi merged data
length(unique(ncd$zip_clean)) # 5105 
length(intersect(ncd$zip_clean,this_data$GEOID)) # 4749
ncd = rename(ncd,"GEOID"="zip_clean")
test = anti_join(ncd,this_data,by="GEOID")
# some of these zip codes exist and are in NC so not sure why not merging

# first merge in the ncd data to this_data_seregion and nc_sf_seregion
sf_merged = right_join(nc_sf_seregion,ncd,by="GEOID")
table(sf_merged$month_year)
colnames(sf_merged)

# make a plot of the incidence by month
sf_merged %>% filter(month_year=="1-2018") %>% ggplot() + geom_sf(aes(fill=ed_count_vomd_for_month)) 
















