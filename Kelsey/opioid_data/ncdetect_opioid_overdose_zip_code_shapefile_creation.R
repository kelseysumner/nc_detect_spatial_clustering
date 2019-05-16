# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#      Zip Code Opioid Overdose Data        #
#    Trying to Merge in Census Data in R    #
#              May 16, 2019                 #
#                K. Sumner                  #
# ----------------------------------------- #

# what this is doing: 
# reading in the naloxone geocoded data set, merging with census data at the tract level

#### ------- load the libraries ---------- ####

# load in tidyverse and geospatial libraries (sf)
library(tidyverse)
library(GISTools)
library(rgdal)
library(foreign)
library(zipcode)
library(readxl)
library(sp)


#### -------- load in the data sets -------- ####

# set working directory
setwd("C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Opioid Overdose Data")

# read in the data set
data = readxl::read_xlsx(".\\MedDrugZIP_20190510.xlsx") %>%
  mutate(zip = str_pad(as.character(ZIP), width = 5, side = "left", pad = "0")) %>%
  dplyr::select(-ZIP)
colnames(data) = c("Count", "visitdate", "zip")


#### -------- create shapefiles from the opioid overdose data ----------- ####

#add lat and long based on zip code matches - 30622 matches using zipcode package, remove nonmatches
data("zipcode")
clean_data = data %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.)))

#write to csv for import into SaTScan as case and coordinates files
clean_data %>%
  write.csv("OpioidData.csv")

# create another clean data set
clean_data_nsp = clean_data

# convert to a spatial points data frame
class(clean_data)
head(clean_data)
colnames(clean_data)
coordinates(clean_data) <- c("longitude","latitude") # this pulls out lat/long
class(clean_data)

# now export the spatial points data frame as a shapefile
# add proj4 string
proj4string(clean_data) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(clean_data,dsn=".",layer="opioid_overdose",driver="ESRI Shapefile")
# this created a shapefile of all the data

# now subset the data to look at the data aggregated by months
month_data = clean_data_nsp %>%
  mutate(month=lubridate::month(visitdate),year=lubridate::year(visitdate)) %>%
  mutate(month_year=paste0(month,"-",year)) %>%
  group_by(month_year,zip,latitude,longitude) %>%
  summarize(Count = sum(Count))
length(unique(month_data$month_year)) # 17, which is correct

# create separate data sets for each month
data_1_18 = month_data[which(month_data$month_year=="1-2018"),]
data_2_18 = month_data[which(month_data$month_year=="2-2018"),]
data_3_18 = month_data[which(month_data$month_year=="3-2018"),]
data_4_18 = month_data[which(month_data$month_year=="4-2018"),]
data_5_18 = month_data[which(month_data$month_year=="5-2018"),]
data_6_18 = month_data[which(month_data$month_year=="6-2018"),]
data_7_18 = month_data[which(month_data$month_year=="7-2018"),]
data_8_18 = month_data[which(month_data$month_year=="8-2018"),]
data_9_18 = month_data[which(month_data$month_year=="9-2018"),]
data_10_18 = month_data[which(month_data$month_year=="10-2018"),]
data_11_18 = month_data[which(month_data$month_year=="11-2018"),]
data_12_18 = month_data[which(month_data$month_year=="12-2018"),]
data_1_19 = month_data[which(month_data$month_year=="1-2019"),]
data_2_19 = month_data[which(month_data$month_year=="2-2019"),]
data_3_19 = month_data[which(month_data$month_year=="3-2019"),]
data_4_19 = month_data[which(month_data$month_year=="4-2019"),]
data_5_19 = month_data[which(month_data$month_year=="5-2019"),]
table(month_data$month_year, useNA = "always")

# now create separate separate shapefiles for each month
# data_1_18
coordinates(data_1_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_1_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_1_18,dsn=".",layer="opioid_overdose_data_1_18",driver="ESRI Shapefile")
# data_2_18
coordinates(data_2_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_2_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_2_18,dsn=".",layer="opioid_overdose_data_2_18",driver="ESRI Shapefile")
# data_3_18
coordinates(data_3_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_3_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_3_18,dsn=".",layer="opioid_overdose_data_3_18",driver="ESRI Shapefile")
# data_4_18
coordinates(data_4_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_4_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_4_18,dsn=".",layer="opioid_overdose_data_4_18",driver="ESRI Shapefile")
# data_5_18
coordinates(data_5_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_5_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_5_18,dsn=".",layer="opioid_overdose_data_5_18",driver="ESRI Shapefile")
# data_6_18
coordinates(data_6_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_6_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_6_18,dsn=".",layer="opioid_overdose_data_6_18",driver="ESRI Shapefile")
# data_7_18
coordinates(data_7_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_7_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_7_18,dsn=".",layer="opioid_overdose_data_7_18",driver="ESRI Shapefile")
# data_8_18
coordinates(data_8_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_8_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_8_18,dsn=".",layer="opioid_overdose_data_8_18",driver="ESRI Shapefile")
# data_9_18
coordinates(data_9_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_9_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_9_18,dsn=".",layer="opioid_overdose_data_9_18",driver="ESRI Shapefile")
# data_10_18
coordinates(data_10_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_10_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_10_18,dsn=".",layer="opioid_overdose_data_10_18",driver="ESRI Shapefile")
# data_11_18
coordinates(data_11_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_11_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_11_18,dsn=".",layer="opioid_overdose_data_11_18",driver="ESRI Shapefile")
# data_12_18
coordinates(data_12_18) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_12_18) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_12_18,dsn=".",layer="opioid_overdose_data_12_18",driver="ESRI Shapefile")
# data_1_19
coordinates(data_1_19) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_1_19) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_1_19,dsn=".",layer="opioid_overdose_data_1_19",driver="ESRI Shapefile")
# data_2_19
coordinates(data_2_19) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_2_19) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_2_19,dsn=".",layer="opioid_overdose_data_2_19",driver="ESRI Shapefile")
# data_3_19
coordinates(data_3_19) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_3_19) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_3_19,dsn=".",layer="opioid_overdose_data_3_19",driver="ESRI Shapefile")
# data_4_19
coordinates(data_4_19) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_4_19) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_4_19,dsn=".",layer="opioid_overdose_data_4_19",driver="ESRI Shapefile")
# data_5_19
coordinates(data_5_19) <- c("longitude","latitude") # this pulls out lat/long
proj4string(data_5_19) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(data_5_19,dsn=".",layer="opioid_overdose_data_5_19",driver="ESRI Shapefile")

# now aggregate by year
year_data = clean_data_nsp %>%
  mutate(year=lubridate::year(visitdate)) %>%
  group_by(year,zip,latitude,longitude) %>%
  summarize(Count = sum(Count))
length(unique(year_data$year)) # 2, which is correct

# now make separate data sets for each year
year_data_2018 = year_data[which(year_data$year=="2018"),]
year_data_2019 = year_data[which(year_data$year=="2019"),]
table(year_data$year, useNA = "always")

# now make separate shape files for each year
# 2018
coordinates(year_data_2018) <- c("longitude","latitude") # this pulls out lat/long
proj4string(year_data_2018) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(year_data_2018,dsn=".",layer="opioid_overdose_year_data_2018",driver="ESRI Shapefile")
# 2019
coordinates(year_data_2019) <- c("longitude","latitude") # this pulls out lat/long
proj4string(year_data_2019) <- CRS("+init=epsg:4326") # this sets the geographical projection (WGS 1984)
writeOGR(year_data_2019,dsn=".",layer="opioid_overdose_year_data_2019",driver="ESRI Shapefile")












