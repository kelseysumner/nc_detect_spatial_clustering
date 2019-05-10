# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Naloxone Geocoded Data          #
#    Trying to Merge in Census Data in R    #
#              May 10, 2019                 #
#                K. Sumner                  #
# ----------------------------------------- #

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


#### -------- load in the data sets -------- ####

# set working directory
setwd("/Users/kelseysumner/Desktop/NC DETECT")

# read in a data set
nalox_data = read_csv("Naloxone Geocoded Data/naloxone_geocoded_deidentified_tract.csv")


#### ----- clean the data set ---------- ####

# look at a summary
summary(nalox_data)

# subset to just the variables of interest
vars_to_keep = c("arc_city","arc_state","arc_zip_co","unitnotf_1","week","inccity","incstate","inczip","syndromeid","ptage","cnty_fips","stcofips","fips","pop2010","tract")
length(vars_to_keep)
nalox_data = nalox_data[,vars_to_keep]

# recode the remaining variables 

# arc_city
str(nalox_data$arc_city)
nalox_data$arc_city = tolower(nalox_data$arc_city)
table(nalox_data$arc_city,useNA="always")
# some numbers in the city part, recode these 
nalox_data$arc_city[which(nalox_data$arc_city=="1400")] = NA
nalox_data$arc_city[which(nalox_data$arc_city=="2960")] = NA
nalox_data$arc_city[which(nalox_data$arc_city=="5660")] = NA
nalox_data$arc_city[which(nalox_data$arc_city=="680")] = NA
nalox_data$arc_city[which(nalox_data$arc_city=="8080")] = NA
nalox_data$arc_city[which(nalox_data$arc_city=="8480")] = NA
nalox_data$arc_city[which(nalox_data$arc_city=="8880")] = NA
nalox_data$arc_city[which(nalox_data$arc_city=="8960")] = NA
nalox_data$arc_city[which(nalox_data$arc_city=="9800")] = NA
# look at the recoding
table(nalox_data$arc_city,useNA="always")

# arc_state
str(nalox_data$arc_state)
table(nalox_data$arc_state, useNA="always")
# recode the miscodings
nalox_data$arc_state[which(nalox_data$arc_state=="N2")] = "NC"
nalox_data$arc_state[which(nalox_data$arc_state=="N4")] = "NC"
# look at the recoding
table(nalox_data$arc_state, useNA="always")

# arc_zip_co
str(nalox_data$arc_zip_co)
table(nalox_data$arc_zip_co, useNA="always")
table(nchar(nalox_data$arc_zip_co))
# recode the zip code data
new_zip = rep(NA,nrow(nalox_data))
for (i in 1:nrow(nalox_data)) {
  if (nchar(nalox_data$arc_zip_co[i])==1 | nalox_data$arc_zip_co[i] == "NULL"){
    new_zip[i] = NA
  } else if (nchar(nalox_data$arc_zip_co[i])==4){
    new_zip[i] = paste0("0",nalox_data$arc_zip_co[i])
  } else if (nchar(nalox_data$arc_zip_co[i])==9){
    new_zip[i] = substr(nalox_data$arc_zip_co[i],1,5)
  } else {
    new_zip[i] = nalox_data$arc_zip_co[i]
  }
}
table(nchar(new_zip))
tail(new_zip,20)
tail(nalox_data$arc_zip_co,20)
# add to the data set
nalox_data$arc_zip_co = new_zip

# unitnotf_1



