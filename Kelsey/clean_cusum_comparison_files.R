# --------------------------- #
# Clean Cusum Comparison Data #
#         June 27, 2019       #
#            K. Sumner        #
# --------------------------- #


#### ----- load libraries --------- ####

# load in the libraries of interest
library(tidyverse)
library(foreign)
library(lubridate)
library(zipcode)



#### ------ read in the data sets ------- ####

# load in the cusum data sets
nalox_data = read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/CUSUM flags/EMSNaloxone2017annotationOverview_cusum.csv")
opioid_data = read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/CUSUM flags/meddrug_cusum_flags.csv")
gi_data= read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/CUSUM flags/vomd_ears_signals.csv")


#### ------ clean the data ------- ####

# subset the naloxone data to just jan 10, 2017
nalox_data_subset = nalox_data[which(nalox_data$Date == "1/10/2017"),]
# looks like the flag was in Buncombe county

# merge the opioid data with zip code lat and long
#add lat and long based on zip code matches - remove nonmatches
opioid_data = rename(opioid_data,"zip"="ears_signals")
opioid_data$zip = as.character(opioid_data$zip)
data("zipcode")
clean_opioid_data = opioid_data %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.)))

# merge the gi data with zip code lat and long
#add lat and long based on zip code matches - remove nonmatches
gi_data = rename(gi_data,"zip"="ears_signal")
gi_data$zip = as.character(gi_data$zip)
data("zipcode")
clean_gi_data = gi_data %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.)))

# look at the number of unique zip codes
length(unique(clean_opioid_data$zip))
length(unique(clean_gi_data$zip))

# export the data sets
write_csv(clean_opioid_data ,"C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/CUSUM flags/cusum_opioid_data_for_mapping.csv")
write_csv(clean_gi_data ,"C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/CUSUM flags/cusum_gi_data_for_mapping.csv")






