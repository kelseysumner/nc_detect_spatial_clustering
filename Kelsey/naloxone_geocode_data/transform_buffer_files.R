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












