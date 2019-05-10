# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Vomiting/Diarrhea Data          #
#             Clean Raw Data                #
#               May 2, 2019                 #
#                K. Sumner                  #
# ----------------------------------------- #


# what this is doing: 
# reading in the gi data set by zip code and cleaning up to then go merge with zip code centroids in ArcGIS


#### ------ read in the libraries ------ ####

# load the packages
library(sp)
library(readr)
library(lubridate)
library(tidyr)
library(dplyr)
library(GISTools)
library(rgdal)


#### ------- read in the data sets ------ ####

# set working directory
setwd("/Users/kelseysumner/Desktop/NC DETECT/GI visit data")

# read in the raw gi data
gi_data = read_csv("GIVomDbyZIP.csv")


#### ------ clean the gi data before entering it into satscan ------- ####

# look at a summary of the data
summary(gi_data)

# recode and rename the ED Visit Count of GI VomD variable
gi_data = rename(gi_data,"ed_count_vomd"="ED Visit Count of GI VomD")
gi_data$ed_count_vomd = as.numeric(gi_data$ed_count_vomd)
str(gi_data$ed_count_vomd)

# recode and rename the ZIP variable
gi_data = rename(gi_data,"zip"="ZIP")
# look at the distribution of gi_data
table(gi_data$zip, useNA = "always")
# looks like there are some zip codes that are not 5 digits and will need to be cleaned up

# create a function that loops through each zip code and if it's <5 digits, adds leading 0s
table(nchar(gi_data$zip))
str(gi_data$zip)
# first remove the missing zip codes
length(which(is.na(gi_data$zip))) # 24 missing
gi_data = gi_data[-which(is.na(gi_data$zip)),]
# then do a for loop to add leading 0s
zip_new = rep(NA,nrow(gi_data))
for (i in 1:nrow(gi_data)){
  if (gi_data$zip[i]=="NULL"){
    zip_new[i] = NA
  } else if (nchar(gi_data$zip[i])==4 & gi_data$zip[i] != "NULL"){
    zip_new[i] = as.character(paste0("0",gi_data$zip[i]))
  } else if (nchar(gi_data$zip[i])==3){
    zip_new[i] = as.character(paste0("00",gi_data$zip[i]))
  } else if (nchar(gi_data$zip[i])==2){
    zip_new[i] = as.character(paste0("000",gi_data$zip[i]))
  } else {
    zip_new[i] = as.character(gi_data$zip[i])
  }
}
# check the output
head(zip_new)
table(nchar(zip_new))
gi_data$zip_clean = zip_new

# look at a summary of the cleaned zip data
summary(gi_data$zip_clean)
table(gi_data$zip_clean)
max(gi_data$zip_clean)
# is a zip code of 99999 real or does that represent missing?
# assume that zip code of 99999 represents that it is missing and code as missing
gi_data$zip_clean[which(gi_data$zip_clean==99999)] = NA

# remove the rows that have missing zip code data because we will not be able to map these
length(which(is.na(gi_data$zip_clean))) # 423 missing 
gi_data_clean = gi_data[which(!(is.na(gi_data$zip_clean))),] # 185819-423=185396
nrow(gi_data_clean) # 185396

# export the cleaned case file
write_csv(gi_data_clean,"gi_data_case_file.csv")






