# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Vomiting/Diarrhea Data          #
#        Cleaning Merged Spatial File       #
#               May 3, 2019                 #
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
library(foreign)


#### ------- read in the data sets ------ ####

# set working directory
setwd("/Users/kelseysumner/Desktop/NC DETECT")

# read in the raw gi data
gi_raw_clean_data = read_csv("GI visit data/gi_data_case_file.csv")


#### ------- manipulate the cleaned data to have various data set aggregations ------- ####

## make a data set of the gi data at the day level
# check that the long data set is clean (where have entry by day)
summary(gi_raw_clean_data)
# remove the zip column
gi_raw_clean_data$zip <- NULL
summary(gi_raw_clean_data)
# make the zip_clean column a character
gi_raw_clean_data$zip_clean = as.character(gi_raw_clean_data$zip_clean)
# looks good, now export
write_csv(gi_raw_clean_data,"gi_case_data_by_day.csv")

## now make a new dataset that is aggregated by the month level
# make a month variable
gi_raw_clean_data$visit_date_formatted = mdy(gi_raw_clean_data$visitdate)
gi_raw_clean_data$month = month(gi_raw_clean_data$visit_date_formatted)
min(gi_raw_clean_data$visit_date_formatted)
max(gi_raw_clean_data$visit_date_formatted)
# make a year variable
gi_raw_clean_data$year = year(gi_raw_clean_data$visit_date_formatted)
# make a combined month-year variable
gi_raw_clean_data$month_year = paste0(gi_raw_clean_data$month,"-",gi_raw_clean_data$year)
# now group based on zip code and month
gi_month_data = gi_raw_clean_data %>%
  group_by(zip_clean,month_year) %>%
  summarize(ed_count_vomd_for_month = sum(ed_count_vomd,na.rm=T))
# test out that the month data is correct
test = gi_raw_clean_data[which(gi_raw_clean_data$zip_clean == "10029"),]
gi_month_data[which(gi_month_data$zip_clean=="10029"),]
# looks like it worked correctly
# export the new data set
write_csv(gi_month_data,"gi_case_data_by_month.csv")

## now make a new dataset that is aggregated at the zip code level across the entire time period (1/1/2018-4/9/2019)
# this is a cross-sectional data set
# now group based on zip code only
gi_crosssection_data = gi_raw_clean_data %>%
  group_by(zip_clean) %>%
  summarize(ed_count_vomd_for_crosssection = sum(ed_count_vomd,na.rm=T))
# test out that the cross-sectional data is correct
test = gi_raw_clean_data[which(gi_raw_clean_data$zip_clean == "10029"),]
sum(test$ed_count_vomd) # 13
gi_crosssection_data[which(gi_crosssection_data$zip_clean=="10029"),] # 13
# looks like it worked correctly
# now export the data set
write_csv(gi_crosssection_data,"gi_case_data_by_crosssection.csv")













