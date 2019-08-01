# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Zip Code Influenza Data         #
#             Make Maps by Week             #
#              August 1, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

# what this is doing: 
# reading in the naloxone geocoded data set, merging with census data at the tract level


#### ------- load the libraries ---------- ####

# load in tidyverse
library(tidyverse)
library(tidycensus)
library(rgdal)


#### -------- user setup ----------------- ####

if (str_detect(tolower(Sys.info()["user"]), "kelsey")) {
  user = "kelsey"
  wd_cconly = "C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data\\cc_only\\180_days_WEEKLY\\shapefiles"
  wd_cctriage = "C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data\\ccandtriagenotes\\180_days_WEEKLY\\shapefiles"
  
} else if (str_detect(tolower(Sys.info()["user"]), "joyce")) {
  user = "joyce"
  wd_cconly = "C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Influenza Data\\cc_only\\180_days_WEEKLY\\shapefiles"
  wd_cctriage = "C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Influenza Data\\ccandtriagenotes\\180_days_WEEKLY\\shapefiles"
  
} else {
  print("Specify working directory")
}


#### -------- load in the data set for cconly  -------- ####

# set working directory
setwd(wd_cconly)

# look at a list of the directories in the current working directory
dir_list = list.dirs()

# find latitude and longitude for the counties to use as basemap
# get spatial data from tidy census
nc_sf_counties = tidycensus::get_acs(geography = "county", state = "NC", 
                                     variables = "B19013_001",
                                     summary_var = "B01001_001", geometry = TRUE,  
                                     key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0") %>%
  rename(totpop = summary_est)

# create directory for weekly plots
dir.create("../weekly_plots")

# now loop through every directory
for (i in length(dir_list)){
  
  # move into the next level directory
  setwd(dir_list[i])
  
  # load up area shape file:
  cluster_area <- readOGR("sp_sig_clusters.shp")
  
  # open jpeg file
  dir_name = str_remove(dir_list[i], ".")
  jpeg(paste0("../../weekly_plots",dir_name,"_cluster_map.jpg"), width = 750, height = 750)
  
  # now overlay the cluster area on the latitude and longitude map for counties
  plot(nc_sf_counties %>% st_geometry(),col="grey")
  plot(cluster_area, add=TRUE, col=alpha("red",0.4))
  
  # close the file and save the plot
  dev.off()
  
  
}








