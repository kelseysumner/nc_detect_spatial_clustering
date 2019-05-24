# ----------------------------------------------- #
#          NC DETECT Spatial Project              #
#           Naloxone Geocoded Data                #
# Read in clean data and merge with tidycensus    #
#              May 24, 2019                       #
#                K. Sumner                        #
# ----------------------------------------------- #

# what this is doing: 
# reading in the naloxone geocoded data set, merging with census data at the tract level


#### ------- load the libraries ---------- ####

# load in tidyverse and geospatial libraries (sf)
library(tidyverse)
library(sf)
library(tidycensus)
library(GISTools)
library(rgdal)
library(foreign)


#### ------- read in the data sets ---------- ####

# set working directory
setwd("C://Users//kelseyms//OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data")

# read in the cleaned geocoded naloxone data set
mydata = read_csv("./clean_nalox_data.csv")



#### ------- do one more data cleaning check before merging in with census data --------- ####

# look at a summary of the data set
summary(mydata)

# look at the fips codes
table(nchar(mydata$fips))
# these are all 33 characters, so are the census tracts

# look at the tract codes
table(nchar(mydata$tract)) # these are a wide range of numbers, will not clean for now because will probably just use fips variable




#### --------- merge in the naloxone geocoded data with the fips data ------ #####

# make a list of all the census tracts of interest from the dbf file

# Get population data from tidy census 
this_data = tidycensus::get_acs(geography = "tract", state = "NC",
                                survey="acs5", year=2017, variables = c("B03002_001E", "B03002_003E", "B03002_004E"), 
                                key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0", output = "wide")
this_data = this_data %>%
  rename(totpop = B03002_001E, WnH = B03002_003E, BnH = B03002_004E) 
table(nchar(this_data$GEOID)) # all 11 characters which is good


# get spatial data from tidy census
nc_sf = tidycensus::get_acs(geography = "tract", state = "NC", 
                            variables = "B19013_001",
                            summary_var = "B01001_001", geometry = TRUE,  
                            key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0") %>%
  rename(totpop = estimate)
plot(nc_sf %>% st_geometry()) 

# rename the zips variable to geoid in mydata data
mydata = rename(mydata,"GEOID"="fips")

# make sure all the data sets have geoid coded correctly
mydata$GEOID = as.character(mydata$GEOID)
nc_sf$GEOID = as.character(nc_sf$GEOID)
this_data$GEOID = as.character(this_data$GEOID)

# look at how many entries are in your data set for geoid
length(unique(mydata$GEOID)) # 1830 unique entries so we will want to left join on this to not lose data

# first merge in the data set to the spatial ones
# nc_sf
sf_merged = left_join(mydata,nc_sf,by="GEOID")
colnames(sf_merged)
length(unique(sf_merged$GEOID)) # 1830, this is correct
sf_merged = sf_merged[which(!(is.na(sf_merged$geometry))),] # didn't lose any, good
length(unique(sf_merged$GEOID)) # 1830, good

# this_data
this_data_merged = left_join(mydata,this_data,by="GEOID")
colnames(this_data_merged)
length(unique(this_data_merged$GEOID)) # 1830, good
this_data_merged = this_data_merged[which(!(is.na(this_data_merged$NAME))),] # didn't lose any, good
length(unique(this_data_merged$GEOID)) # 1830, which is correct

# write out the sf_merged object as a shapefile
st_write(sf_merged, "naloxone_data_tract_level.shp")







