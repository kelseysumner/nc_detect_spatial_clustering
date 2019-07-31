library(tidyverse)
library(zipcode)
library(sf)
setwd("C:\\Users\\joyceyan\\OneDrive - University of North Carolina at Chapel Hill\\CCHI\\SaTScan\\Opioid")


data = readxl::read_xlsx(".\\MedDrugZIP_20190510.xlsx") %>%
  mutate(zip = str_pad(as.character(ZIP), width = 5, side = "left", pad = "0")) %>%
  select(-ZIP)
colnames(data) = c("Count", "visitdate", "zip")

#add lat and long based on zip code matches - 30622 matches using zipcode package, remove nonmatches
data("zipcode")
clean_data = data %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.)))

uniquezipmatch = clean_data$zip %>% 
  unique() 
length(uniquezipmatch) #1598 unique zip matches

#write to csv for import into SaTScan as case and coordinates files
clean_data %>%
  write.csv("OpioidData.csv")

subset_data = clean_data %>%
  filter(state %in% c("NC", "GA", "TN", "SC", "VA"))

subset_data %>%
  write.csv("Subset_OpioidData.csv")

# clean_data = clean_data %>%
#   mutate(month = lubridate::month(visitdate))
# 
# 
# clean_data %>%
#   group_by(month, zip, latitude, longitude) %>%
#   summarize(count = sum(Count)) %>% View()


zcta_sf = tidycensus::get_acs(geography = "zip code tabulation area", variables = "B01001_001",
                               geometry = TRUE,  survey="acs5", year=2017,
                               key="82ba4a82d9468e84f59ab762beec5bb58cafcf7c") %>%
  select(zcta5 = GEOID, geometry) %>%
  filter(zcta5 %in% data$zip)



zcta_centroids = read.csv("C:\\Users\\joyceyan\\OneDrive - University of North Carolina at Chapel Hill\\CCHI\\SaTScan\\gaz2016zcta5centroid.csv") %>%
  mutate(zcta5 = str_pad(as.character(zcta5), width = 5, side = "left", pad = "0")) %>%
  select(zcta5, lat = intptlat, long = intptlong) %>%
  filter(zcta5 %in% data$zip)

zcta_opi = zcta_sf %>%
  left_join(zcta_centroids, by = "zcta5") %>%
  filter(zcta5 %in% data$zip) 

uniqueZCTAmatch = zcta_opi$zcta5  #1455 unique zip matches

uniquezipmatch %in% uniqueZCTAmatch
uniquezipmatch[!(uniquezipmatch %in% uniqueZCTAmatch)]

(uniqueZCTAmatch %in% uniquezipmatch) %>% sum()



centroids = matrix(data = NA, nrow = nrow(zcta_sf), ncol = 3) %>% as.data.frame()
colnames(centroids) = c("zip", "lat", "long")
centroids$zip = zcta_sf$zcta5
for (i in 1:nrow(zcta_sf)) {
  point_list = st_cast(zcta_sf$geometry[i], "POINT")
  point_df = matrix(unlist(point_list), ncol = 2, byrow = TRUE) %>%
    as.data.frame()
  #average latitude and longitude to find centroid lat and long
  centroids[i,2:3] = point_df %>% apply(2, mean)
}