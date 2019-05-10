library(tidyverse)
library(zipcode)

data = readxl::read_xlsx(".\\MedDrugZIP_20190510.xlsx") %>%
  mutate(zip = str_pad(as.character(ZIP), width = 5, side = "left", pad = "0")) %>%
  select(-ZIP)
colnames(data) = c("Count", "visitdate", "zip")

#add lat and long based on zip code matches - 30622 matches using zipcode package, remove nonmatches
data("zipcode")
clean_data = data %>%
  left_join(zipcode, by = "zip") %>%
  filter_all(all_vars(!is.na(.)))

#write to csv for import into SaTScan as case and coordinates files
clean_data %>%
  write.csv("OpioidData.csv")