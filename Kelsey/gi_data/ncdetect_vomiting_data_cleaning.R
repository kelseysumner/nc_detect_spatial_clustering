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
# looks like there are some zip codes that are not 5 digits and will need to be clenaed up
# for now, excluding these zip codes, but will need to decide how to clean these later on
# create a new variable that excludes zip codes that are <5 digits
zip_clean = ifelse(nchar(gi_data$zip)==5,gi_data$zip,NA)
gi_data$zip_clean = zip_clean
gi_data$zip_clean = as.numeric(gi_data$zip_clean)
# look at a summary of the cleaned zip data
summary(gi_data$zip_clean)
table(gi_data$zip_clean)
max(gi_data$zip_clean)
# is a zip code of 99999 real or does that represent missing?
# assume that zip code of 99999 represents that it is missing and code as missing
gi_data$zip_clean[which(gi_data$zip_clean==99999)] = NA

# remove the rows that have missing zip code data because we will not be able to map these
length(which(is.na(gi_data$zip_clean))) # 1061 missing 
gi_data_clean = gi_data[which(!(is.na(gi_data$zip_clean))),] # 185843-1061=184782
nrow(gi_data_clean) # 184782

# export the cleaned case file
write_csv(gi_data_clean,"gi_data_case_file.csv")






