# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Zip Code Influenza Data         #
#              July 18, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

# what this is doing: 
# reading in the zip code level influenza data, merging with census data at the tract level

#### ------- load the libraries ---------- ####

# load in tidyverse and geospatial libraries (sf)
library(tidyverse)
library(GISTools)
library(rgdal)
library(foreign)
library(zipcode)
library(readxl)
library(sp)
library(lubridate)

#### -------- user setup ----------------- ####

if (str_detect(tolower(Sys.info()["user"]), "kelsey")) {
  wd = "C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data"
  
} else if (str_detect(tolower(Sys.info()["user"]), "joyce")) {
  wd = "C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Influenza Data"
  
} else {
  print("Specify working directory")
}


#### -------- load in the data sets -------- ####

# set working directory
setwd(wd)

# read in the data sets
# first the cc and triage notes
data_ccandtriagenotes = read_csv("./ccandtriagenotes/ILIbyZIP_ccandtriagenotes.csv") %>%
  filter(ZIP != "NULL") %>%
  mutate(visitdate = mdy(visitdate), zip = str_pad(as.character(ZIP), width = 5, side = "left", pad = "0")) %>%
  dplyr::select(visitdate, Count = syndromecount, zip)

# then the cc only one
data_cconly = read_csv("./cc_only/ILIbyZIP_cconly.csv") %>%
  filter(ZIP != "NULL") %>%
  mutate(visitdate = mdy(visitdate), zip = str_pad(as.character(ZIP), width = 5, side = "left", pad = "0")) %>%
  dplyr::select(visitdate, Count = syndromecount, zip)


# look at quick summaries of both data sets
table(nchar(data_ccandtriagenotes$zip))
table(nchar(data_cconly$zip))
table(data_ccandtriagenotes$zip)
table(data_cconly$zip)



#### ----- add zip codes latitude and longitude coordinates for satscan ------- ####

# cc and triage notes data set
#add lat and long based on zip code matches - matches using zipcode package, remove nonmatches
data("zipcode")
clean_data_ccandtriagenotes = data_ccandtriagenotes %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate(visitweek = epiyear(visitdate)*100 + epiweek(visitdate))

#write to csv for import into SaTScan as case and coordinates files
clean_data_ccandtriagenotes %>%
  write_csv("./ccandtriagenotes/clean_ILIbyZIP_ccandtriagenotes.csv")


# cc only data set
#add lat and long based on zip code matches - matches using zipcode package, remove nonmatches
data("zipcode")
clean_data_cconly = data_cconly %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate(visitweek = epiyear(visitdate)*100 + epiweek(visitdate))


#write to csv for import into SaTScan as case and coordinates files
clean_data_cconly %>%
  write_csv("./cc_only/clean_ILIbyZIP_cconly.csv")



#############################################################
#### ----- aggregate weekly counts (Sun-Sat)------ ####
# wk_data_ccandtriagenotes= data_ccandtriagenotes %>%
#   mutate(visitdate = mdy(visitdate)) %>%
#   mutate(visitweek = epiweek(visitdate)) %>%
#   group_by(zip, visitweek) %>%
#   summarize(Count = sum(Count)) %>%
#   left_join(zipcode, by = "zip") %>%
#   filter_all(all_vars(!is.na(.)))
# 
# write_csv(wk_data_ccandtriagenotes, "./ccandtriagenotes/clean_weekly_ILIbyZIP_ccandtriagenotes.csv")
# 
# wk_data_cconly = data_cconly %>%
#   mutate(visitweek = epiweek(mdy(visitdate))) %>%
#   group_by(zip, visitweek) %>%
#   summarize(Count = sum(Count)) %>%
#   left_join(zipcode, by = "zip") %>%
#   filter_all(all_vars(!is.na(.)))
# 
# write_csv(wk_data_cconly, "./cc_only/clean_weekly_ILIbyZIP_cconly.csv")

