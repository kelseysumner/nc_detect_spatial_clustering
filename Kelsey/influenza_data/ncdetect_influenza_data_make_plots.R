# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Zip Code Influenza Data         #
#     Make Plots of clusters Over Time      #
#              August 1, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

# what this is doing: 
# reading in the influenza data and making plots of clusters over time with the week aggregation


#### ------- load the libraries ---------- ####

# load in tidyverse
library(tidyverse)
library(tidycensus)


#### -------- user setup ----------------- ####

if (str_detect(tolower(Sys.info()["user"]), "kelsey")) {
  user = "kelsey"
  wd_cconly = "C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data\\cc_only\\180_days_WEEKLY\\weekly_shapefiles"
  wd_cctriage = "C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data\\ccandtriagenotes\\180_days_WEEKLY\\weekly_shapefiles"
  
} else if (str_detect(tolower(Sys.info()["user"]), "joyce")) {
  user = "joyce"
  wd_cconly = "C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Influenza Data\\cc_only\\180_days_WEEKLY\\weekly_shapefiles"
  wd_cctriage = "C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Influenza Data\\ccandtriagenotes\\180_days_WEEKLY\\weekly_shapefiles"
  
} else {
  print("Specify working directory")
}


#### -------- load in the data set for cconly  -------- ####

# set working directory
setwd(wd_cconly)