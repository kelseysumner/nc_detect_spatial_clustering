library(tidyverse)
data2018 <- readRDS("C:\\Users\\joyceyan\\OneDrive - University of North Carolina at Chapel Hill\\CCHI\\Data Quality\\2018 report\\ED_data.rds")

icd10cm = icd.data::icd10cm2016 %>%
  mutate(code = gsub('^([[:alnum:]]{3})([[:alnum:]]+)$', '\\1.\\2', as.character(code)))

### ------------------- GI Vom D ----------------------------------

setwd("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\GI visit data\\rsatscan")

gi_zips = c(27534, 27530, 27533, 28333, 27863, 28551, 28578, 27830, 28580, 27569,
            28365, 28502, 27883, 27813, 28504, 28508, 27851, 28538, 27888, 27542,
            27568, 27555, 28554, 28501, 28525, 28341, 27576, 27893, 27894, 27895,
            27828, 27577, 27829, 28572, 28366, 27880, 27896, 27811, 28349, 28398,
            27524, 28530, 27593, 28513, 27852, 27527, 27822, 28526, 28393, 27807,
            28590, 27557, 27864, 28329, 28518, 27504)

gi_jan10_visits = read.csv(".\\GIVomD_1-10-18_Visits.csv", 
                           col.names = c("Visit_ID", "Initial_Doc_Time"))

gi_jan10_cluster = data2018 %>%
  filter(Visit_ID %in% gi_jan10_visits$Visit_ID) %>%
  mutate(Zip = str_pad(Zip, width = 5, side = "left", pad = "0")) %>%
  filter(Zip %in% gi_zips)


gi_jan10_allcodes = gi_jan10_cluster %>%
  select(starts_with("Diag")) %>%
  gather(key = "DiagNum", value = "Code") %>%
  filter(!is.na(Code)) 

gi_jan10_topn = gi_jan10_allcodes %>%
  group_by(Code) %>%
  summarize(Count = n()) %>% 
  arrange(desc(Count)) %>%
  mutate(Code = as.character(Code)) %>%
  left_join(icd10cm, by = c("Code" = "code")) %>%
  select(Code, Description = long_desc, Count) %>%
  top_n(10, Count)

write.csv(gi_jan10_topn, "gi_jan10_top10.csv")


### -------------------Med Drug OD --------------------

setwd("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Opioid Overdose Data\\SaTScan\\rsatscan")

meddrug_zips = c(27055, 27023, 28642, 27028, 27012, 28621)

meddrug_jan12_visits = read.csv(".\\MedDrug_1-12-18_Visits.csv",
                                col.names = c("Visit_ID", "Initial_Doc_Time"))

meddrug_jan12_cluster = data2018 %>%
  filter(Visit_ID %in% meddrug_jan12_visits$Visit_ID) %>%
  mutate(Zip = str_pad(Zip, width = 5, side = "left", pad = "0")) %>%
  filter(Zip %in% meddrug_zips)

meddrug_jan12_allcodes = meddrug_jan12_cluster %>%
  select(starts_with("Diag")) %>%
  gather(key = "DiagNum", value = "Code") %>%
  filter(!is.na(Code))

meddrug_jan12_topn = meddrug_jan12_allcodes %>%
  group_by(Code) %>%
  summarize(Count = n()) %>% 
  arrange(desc(Count)) %>%
  mutate(Code = as.character(Code)) %>%
  left_join(icd10cm, by = c("Code" = "code")) %>%
  select(Code, Description = long_desc, Count) %>%
  top_n(4, Count)


write.csv(meddrug_jan12_topn, "meddrug_jan12_top4.csv")


