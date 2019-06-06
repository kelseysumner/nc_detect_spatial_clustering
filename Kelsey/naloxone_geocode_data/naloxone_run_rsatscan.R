# ----------------------------------------------- #
#          NC DETECT Spatial Project              #
#           Naloxone Geocoded Data                #
#      Try running rsatscan on the data set       #
#              June 6, 2019                       #
#                K. Sumner                        #
# ----------------------------------------------- #

# what this is doing: 
# reading in the naloxone shapefile (by day) and running rsatscan on it


#### ------- load the libraries ---------- ####

# load in the libraries of interest
library(rsatscan)
library(tidyverse)


#### ------- read in the data sets ---------- ####

# set working directory
setwd("C://Users//kelseyms//OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data")

# read in the cleaned naloxone data with the lat and long centroid pulled out
nalox_data = read_csv("./clean_nalox_data_latlong.csv")



#### ------- try running rsatscan on the data set --------- ####







