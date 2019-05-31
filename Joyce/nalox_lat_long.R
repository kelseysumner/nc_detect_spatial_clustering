library(tidyverse)
library(sf)

setwd("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Naloxone Geocoded Data")


## get spatial data from tidy census
nc_sf = tidycensus::get_acs(geography = "tract", state = "NC", 
                            variables = "B19013_001", #B19013_001 is median household income, #B01001_001 is total of males and females
                            summary_var = "B01001_001", geometry = TRUE,  
                            key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0") %>%
  #rename(totpop = estimate)
  rename(totpop = summary_est)


# pull lat/long out for each census tract by averaging points in each polygon
centroids = matrix(data = NA, nrow = nrow(nc_sf), ncol = 3) %>% as.data.frame()
colnames(centroids) = c("GEOID", "lat", "long")
centroids$GEOID = nc_sf$GEOID
for (i in 1:nrow(nc_sf)) {
  
  #if geometry is empty, lat and long stay as NA
  if (!st_is_empty(nc_sf$geometry[i])){
    point_list = st_cast(nc_sf$geometry[i], "POINT")
    point_df = matrix(unlist(point_list), ncol = 2, byrow = TRUE) %>%
      as.data.frame()
    #average latitude and longitude to find centroid lat and long
    centroids[i,2:3] = point_df %>% apply(2, mean)
  }
  
}

centroids %>% filter(is.na(lat) | is.na(long))  # census tracts had empty multipolygons in geometry

centroids = centroids %>%
  filter(!is.na(lat) & !is.na(long)) #remove census tracts with NA lat/long

write.csv(centroids, "census_tract_latlong.csv", row.names = FALSE)


