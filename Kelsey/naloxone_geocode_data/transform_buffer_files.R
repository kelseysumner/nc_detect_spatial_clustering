# ----------------------------------------------- #
#          NC DETECT Spatial Project              #
#           Naloxone Geocoded Data                #
#         Edit SaTScan buffer files               #
#              June 13, 2019                      #
#                K. Sumner                        #
# ----------------------------------------------- #

# what this is doing: 
# reading in the naloxone shapefile (by day) and running rsatscan on it


#### ------- load the libraries ---------- ####

# load in the libraries of interest
library(tidyverse)
library(foreign)
library(lubridate)


#### ------- read in the data sets ---------- ####

## work with 30 day naloxone data

# set working directory
setwd("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data/satscan/30_days/additional shapefiles")

# add month variable to the dbf data

# read in the cleaned naloxone data with the lat and long centroid pulled out
satscan_30_day_buffers = read.dbf("./30day_sig_cluster_buffers_3.dbf")

# change the season variable
satscan_30_day_buffers$season <- NULL
satscan_30_day_buffers$date = lubridate::mdy(satscan_30_day_buffers$date)
satscan_30_day_buffers$month = month(satscan_30_day_buffers$date)
table(satscan_30_day_buffers$month, useNA = "always")

# write out the new dbf file
write.dbf(satscan_30_day_buffers,"30day_sig_cluster_buffers_3.dbf")



## work with 7 day naloxone data

# set working directory
setwd("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data/satscan/7_days/additional shapefiles")

# add month variable to the dbf data

# read in the cleaned naloxone data with the lat and long centroid pulled out
satscan_30_day_buffers = read.dbf("./7day_sig_clusters_buffer.dbf")

# change the season variable
satscan_30_day_buffers$season <- NULL
satscan_30_day_buffers$date = lubridate::mdy(satscan_30_day_buffers$date)
satscan_30_day_buffers$month = month(satscan_30_day_buffers$date)
table(satscan_30_day_buffers$month, useNA = "always")

# write out the new dbf file
write.dbf(satscan_30_day_buffers,"7day_sig_clusters_buffer.dbf")



## subset the 7 day opioid overdose data significant cluster data sets to just be in the 1 year period (2018)

# set working directory
setwd("/Users/kelseysumner/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Opioid Overdose Data/SaTScan/rsatscan/7_days")

# read in the cleaned data with the lat and long centroid pulled out
opioid_data = read_csv("./sig_clusters.csv")

# change the month variable
opioid_data$month = month(opioid_data$date)
table(opioid_data$month, useNA = "always")

# restrict the data set to just being within 2018
opioid_data$year = year(opioid_data$date)
table(opioid_data$year, useNA = "always")
opioid_data = opioid_data[which(opioid_data$year == "2018"),]

# now export
write_csv(opioid_data,"sig_clusters_subset.csv")



## subset the 30 day opioid overdose data significant cluster data sets to just be in the 1 year period (2018)

# set working directory
setwd("/Users/kelseysumner/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Opioid Overdose Data/SaTScan/rsatscan/30_days")

# read in the cleaned data with the lat and long centroid pulled out
opioid_data = read_csv("./sig_clusters.csv")

# change the month variable
opioid_data$month = month(opioid_data$date)
table(opioid_data$month, useNA = "always")

# restrict the data set to just being within 2018
opioid_data$year = year(opioid_data$date)
table(opioid_data$year, useNA = "always")
opioid_data = opioid_data[which(opioid_data$year == "2018"),]

# now export
write_csv(opioid_data,"sig_clusters_subset.csv")





## subset the 7 day gi data significant cluster data sets to just be in the 1 year period (2018)

# set working directory
#setwd("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/GI visit data/rsatscan/7_days")
setwd("/Users/kelseysumner/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/GI visit data/rsatscan/7_days")

# read in the cleaned data with the lat and long centroid pulled out
gi_data = read_csv("./sig_clusters.csv")

# change the month variable
gi_data$month = month(gi_data$date)
table(gi_data$month, useNA = "always")

# restrict the data set to just being within 2018
gi_data$year = year(gi_data$date)
table(gi_data$year, useNA = "always")
gi_data = gi_data[which(gi_data$year == "2018"),]

# now export
write_csv(gi_data,"sig_clusters_subset.csv")





## subset the 30 day gi data significant cluster data sets to just be in the 1 year period (2018)

# set working directory
#setwd("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/GI visit data/rsatscan/30_days")
setwd("/Users/kelseysumner/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/GI visit data/rsatscan/30_days")


# read in the cleaned data with the lat and long centroid pulled out
gi_data = read_csv("./sig_clusters.csv")

# change the month variable
gi_data$month = month(gi_data$date)
table(gi_data$month, useNA = "always")

# restrict the data set to just being within 2018
gi_data$year = year(gi_data$date)
table(gi_data$year, useNA = "always")
gi_data = gi_data[which(gi_data$year == "2018"),]

# now export
write_csv(gi_data,"sig_clusters_subset.csv")








