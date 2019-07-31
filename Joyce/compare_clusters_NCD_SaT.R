library(tidyverse)

setwd("C:/Users/joyceyan/University of North Carolina at Chapel Hill/Sumner, Kelsey Marie - nc_detect_one_drive/Naloxone Geocoded Data")

satscan_clusters = read.csv("./satscan/10_days/clusters.csv") %>%
  mutate(date = lubridate::ymd(date))

NCD_clusters = read.csv("./EMSNaloxone2017annotationOverview_cusum.csv") %>%
  mutate(Date = lubridate::mdy(Date))

NCD_C1 = NCD_clusters %>%
  filter(str_detect(CUSUM.Flags, "1"))


satscan_NCD = satscan_clusters %>%
  full_join(NCD_C1, by = c("date" = "Date", "county" = "Location")) %>%
  select(date, county, fips, CUSUM.Flags) %>%
  mutate(satscan = !is.na(fips), NCD = !is.na(CUSUM.Flags))
