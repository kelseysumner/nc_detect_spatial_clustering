# --------------------------- #
#   Make Tables for SaTScan   #
#           Deliverable       #
#         June 20, 2019       #
#            K. Sumner        #
# --------------------------- #


#### ----- load libraries --------- ####

# load in the libraries of interest
library(tidyverse)
library(foreign)
library(lubridate)



#### ------ read in the data sets ------- ####


# load in the med/drug data sets
opioid_7day = read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Opioid Overdose Data/SaTScan/rsatscan/7_days/sig_clusters_subset.csv")
opioid_30day = read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Opioid Overdose Data/SaTScan/rsatscan/30_days/sig_clusters_subset.csv")


# load in the gi data sets
gi_7day = read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/GI visit data/rsatscan/7_days/sig_clusters_subset.csv")
gi_30day = read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/GI visit data/rsatscan/30_days/sig_clusters_subset.csv")


# load in the naloxone data sets
nalox_7day = read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data/satscan/7_days/sig_clusters.csv")
nalox_30day = read_csv("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data/satscan/30_days/sig_clusters.csv")



#### ---------- create tables for med/drug data -------- ####


## 7 day med/drug data

# highest count is 2 clusters on the same day
opioid_7day %>% group_by(date) %>% summarize(Count = n()) %>% arrange(desc(Count))

# summarize counts
sig_clusters_plot = opioid_7day %>%
  mutate(date = lubridate::ymd(date))%>%
  filter(date >= "2018-01-01" & date <= "2018-12-31") %>%
  group_by(date) %>%
  summarize(number_loc = sum(number_loc), radius = sum(radius))

#are there particular days with large radii or large number of locations?
top10_numloc = sig_clusters_plot %>%
  arrange(desc(number_loc)) %>% 
  select(date, number_loc) %>% 
  top_n(10)

top10_radius =  sig_clusters_plot %>%
  arrange(desc(radius)) %>% 
  select(date, radius) %>% 
  top_n(10)




## 30 day med/drug data

# highest count is 2 clusters on the same day
opioid_30day %>% group_by(date) %>% summarize(Count = n()) %>% arrange(desc(Count))

# summarize counts
sig_clusters_plot = opioid_30day %>%
  mutate(date = lubridate::ymd(date))%>%
  filter(date >= "2018-01-01" & date <= "2018-12-31") %>%
  group_by(date) %>%
  summarize(number_loc = sum(number_loc), radius = sum(radius))

#are there particular days with large radii or large number of locations?
top10_numloc = sig_clusters_plot %>%
  arrange(desc(number_loc)) %>% 
  select(date, number_loc) %>% 
  top_n(10)

top10_radius =  sig_clusters_plot %>%
  arrange(desc(radius)) %>% 
  select(date, radius) %>% 
  top_n(10)





#### ---------- create tables for gi data -------- ####


## 7 day gi data

# highest count is 2 clusters on the same day
gi_7day %>% group_by(date) %>% summarize(Count = n()) %>% arrange(desc(Count))

# summarize counts
sig_clusters_plot = gi_7day %>%
  mutate(date = lubridate::ymd(date))%>%
  filter(date >= "2018-01-01" & date <= "2018-12-31") %>%
  group_by(date) %>%
  summarize(number_loc = sum(number_loc), radius = sum(radius))

#are there particular days with large radii or large number of locations?
top10_numloc = sig_clusters_plot %>%
  arrange(desc(number_loc)) %>% 
  select(date, number_loc) %>% 
  top_n(10)

top10_radius =  sig_clusters_plot %>%
  arrange(desc(radius)) %>% 
  select(date, radius) %>% 
  top_n(10)




## 30 day gi data

# highest count is 2 clusters on the same day
gi_30day %>% group_by(date) %>% summarize(Count = n()) %>% arrange(desc(Count))

# summarize counts
sig_clusters_plot = gi_30day %>%
  mutate(date = lubridate::ymd(date))%>%
  filter(date >= "2018-01-01" & date <= "2018-12-31") %>%
  group_by(date) %>%
  summarize(number_loc = sum(number_loc), radius = sum(radius))

#are there particular days with large radii or large number of locations?
top10_numloc = sig_clusters_plot %>%
  arrange(desc(number_loc)) %>% 
  select(date, number_loc) %>% 
  top_n(10)

top10_radius =  sig_clusters_plot %>%
  arrange(desc(radius)) %>% 
  select(date, radius) %>% 
  top_n(10)





#### ---------- create tables for naloxone data -------- ####


## 7 day nalox data

# highest count is 2 clusters on the same day
nalox_7day %>% group_by(date) %>% summarize(Count = n()) %>% arrange(desc(Count))

# summarize counts
sig_clusters_plot = nalox_7day %>%
  mutate(date = lubridate::mdy(date))%>%
  filter(date >= "2017-01-01" & date <= "2017-12-31") %>%
  group_by(date) %>%
  summarize(number_loc = sum(number_loc), radius = sum(radius))

#are there particular days with large radii or large number of locations?
top10_numloc = sig_clusters_plot %>%
  arrange(desc(number_loc)) %>% 
  select(date, number_loc) %>% 
  top_n(10)

top10_radius =  sig_clusters_plot %>%
  arrange(desc(radius)) %>% 
  select(date, radius) %>% 
  top_n(10)



## 30 day nalox data

# highest count is 2 clusters on the same day
nalox_30day %>% group_by(date) %>% summarize(Count = n()) %>% arrange(desc(Count))

# summarize counts
sig_clusters_plot = nalox_30day %>%
  mutate(date = lubridate::mdy(date))%>%
  filter(date >= "2017-01-01" & date <= "2017-12-31") %>%
  group_by(date) %>%
  summarize(number_loc = sum(number_loc), radius = sum(radius))

#are there particular days with large radii or large number of locations?
top10_numloc = sig_clusters_plot %>%
  arrange(desc(number_loc)) %>% 
  select(date, number_loc) %>% 
  top_n(10)

top10_radius =  sig_clusters_plot %>%
  arrange(desc(radius)) %>% 
  select(date, radius) %>% 
  top_n(10)

