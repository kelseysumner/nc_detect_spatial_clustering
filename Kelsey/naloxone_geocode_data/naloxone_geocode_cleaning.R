# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Naloxone Geocoded Data          #
#              Cleaning Data                #
#              May 10, 2019                 #
#                K. Sumner                  #
# ----------------------------------------- #

# what this is doing: 
# reading in the naloxone geocoded data set, cleaning it before we merge with census data in a later script


#### ------- load the libraries ---------- ####

# load in tidyverse and geospatial libraries (sf)
library(tidyverse)
library(sf)
library(tidycensus)
library(GISTools)
library(rgdal)
library(foreign)


#### -------- load in the data sets -------- ####

# set working directory
#setwd("C://Users//kelseyms//OneDrive - University of North Carolina at Chapel Hill")

setwd("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Naloxone Geocoded Data")

# read in a data set
nalox_data = read_csv("./naloxone_geocoded_deidentified_tract.csv")


#### ----- clean the data set ---------- ####

# look at a summary
summary(nalox_data)

# subset to just the variables of interest
vars_to_keep = c("arc_city","arc_state","arc_zip_co","unitnotf_1","week","inccity","incstate","inczip","syndromeid","ptage","cnty_fips","stcofips","fips","pop2010","tract")
length(vars_to_keep)
nalox_data = nalox_data[,vars_to_keep]

# RECODING OF ALL VARIABLES WITH ERRORS
nalox_data = nalox_data %>%
  mutate(
    arc_city = case_when(
      str_detect(arc_city, "[^a-zA-Z\\s[:punct:]]") ~ NA_character_,  #any cities with any characters that are not letters, spaces, or punctuation (ends up being all numbers for our data)
      arc_city == "Null" ~ NA_character_,
      TRUE ~ str_to_title(arc_city) #convert city names to title case
    ),
    arc_state = "NC",
    arc_zip_co = case_when(
      arc_zip_co %in% c("0", "NULL") ~ NA_character_,
      nchar(arc_zip_co) == 9 & !str_detect(arc_zip_co, "[^0-9]") ~ substr(arc_zip_co, 1, 5),  #length of 9 and no nonnumeric characters
      nchar(arc_zip_co) == 5 & !str_detect(arc_zip_co, "[^0-9]")  ~ arc_zip_co, 
      TRUE ~ NA_character_
    ),
    inccity = case_when(
      str_detect(inccity, "[^a-zA-Z\\s[:punct:]]") ~ NA_character_,  #any cities with any characters that are not letters, spaces, or punctuation (ends up being all numbers for our data)
      inccity == "Null" ~ NA_character_,
      TRUE ~ str_to_title(inccity) #convert city names to title case
    ),
    incstate = "NC",
    inczip = case_when(
      inczip %in% c("0", "NULL") ~ NA_character_,
      nchar(inczip) == 9 & !str_detect(inczip, "[^0-9]") ~ substr(inczip, 1, 5),  #length of 9 and no nonnumeric characters
      nchar(inczip) == 5 & !str_detect(inczip, "[^0-9]")  ~ inczip, #length of 5 and no nonnumeric characters
      TRUE ~ NA_character_
    ),
    ptage = case_when(
      ptage == 999 ~ NA_real_,
      TRUE ~ ptage
    ),
    pop2010 = case_when(
      pop2010 %in% c(0, 1) ~ NA_real_,
      TRUE ~ pop2010
    )
  )

# arc_city
unique(nalox_data$arc_city)
nalox_data$arc_city = str_to_title(nalox_data$arc_city)
table(nalox_data$arc_city,useNA="always")
# some numbers in the city part, recode these 

  nalox_data = nalox_data %>%
    mutate(arc_city = case_when(
      str_detect(arc_city, "[^a-zA-Z\\s[:punct:]]") ~ NA_character_,  #any cities with any characters that are not letters, spaces, or punctuation (ends up being all numbers for our data)
      arc_city == "Null" ~ NA_character_,
      TRUE ~ str_to_title(arc_city) #convert city names to title case
    ))
  
  
# arc_state
unique(nalox_data$arc_state)
table(nalox_data$arc_state, useNA="always")
# recode the miscodings

  nalox_data$arc_state = "NC"



# arc_zip_co
unique(nalox_data$arc_zip_co)
table(nalox_data$arc_zip_co, useNA="always")
table(nchar(nalox_data$arc_zip_co), useNA="always")

nalox_data %>%
  filter(nchar(arc_zip_co) < 5) %>%
  dplyr::select(arc_zip_co) %>%
  unique()

# recode the zip code data
  nalox_data = nalox_data %>%
    mutate(arc_zip_co = case_when(
      arc_zip_co %in% c("0", "NULL") ~ NA_character_,
      nchar(arc_zip_co) == 9 & !str_detect(arc_zip_co, "[^0-9]") ~ substr(arc_zip_co, 1, 5),  #length of 9 and no nonnumeric characters
      nchar(arc_zip_co) == 5 & !str_detect(arc_zip_co, "[^0-9]")  ~ arc_zip_co, 
      TRUE ~ NA_character_
    )) 
  
# unitnotf_1
str(nalox_data$unitnotf_1)
table(nalox_data$unitnotf_1)
# looks like the data is all of 2017 (January 1 - December 31)
# data in correct date format

# week
str(nalox_data$week)
summary(nalox_data$week)

sum(!(nalox_data$week %in% c(1:53)))  # looks coded correctly, no weeks numbered less than 1 or greater than 53

# inccity
table(nalox_data$inccity, useNA="always")
nalox_data = nalox_data %>%
  mutate(inccity = case_when(
    str_detect(inccity, "[^a-zA-Z\\s[:punct:]]") ~ NA_character_,  #any cities with any characters that are not letters, spaces, or punctuation (ends up being all numbers for our data)
    inccity == "Null" ~ NA_character_,
    TRUE ~ str_to_title(inccity) #convert city names to title case
  ))
table(nalox_data$inccity, useNA="always")

# incstate
nalox_data$incstate %>% unique()
nalox_data$incstate = "NC"

#inczip
table(nchar(nalox_data$inczip))
nalox_data %>% filter(nchar(inczip) < 5) %>% dplyr::select(inczip) %>% unique()


nalox_data = nalox_data %>%
  mutate(inczip = case_when(
    inczip %in% c("0", "NULL") ~ NA_character_,
    nchar(inczip) == 9 & !str_detect(inczip, "[^0-9]") ~ substr(inczip, 1, 5),  #length of 9 and no nonnumeric characters
    nchar(inczip) == 5 & !str_detect(inczip, "[^0-9]")  ~ inczip, #length of 5 and no nonnumeric characters
    TRUE ~ NA_character_
  )) 

#syndromeid is good
unique(nalox_data$syndromeid)


#ptage
summary(nalox_data$ptage)
nalox_data %>%
  filter(ptage > 120 & ptage < 999)  # all values above 120 are 999

nalox_data = nalox_data %>%
  mutate(ptage = case_when(
    ptage == 999 ~ NA_real_,
    TRUE ~ ptage
  ))


# cnty_fips is good
unique(nalox_data$cnty_fips)
unique(nchar(nalox_data$cnty_fips))

# stcofips is good
unique(nalox_data$stcofips)

# fips seems to be good
unique(nalox_data$fips)
table(nchar(nalox_data$fips))

# pop2010 - some counts of 0 or 1
nalox_data$pop2010 %>% summary()
nalox_data %>% filter(pop2010 < 10)

nalox_data = nalox_data %>%
  mutate(pop2010 = case_when(
    pop2010 %in% c(0, 1) ~ NA_real_,
    TRUE ~ pop2010
  ))

# tract seems to be good
table(nchar(nalox_data$tract))

# write to csv
write.csv(nalox_data, "clean_nalox_data.csv")



####### --------- patch in some zip codes?? ---------
nalox_data_addzipcity = nalox_data

na_cities = nalox_data_addzipcity %>%
  filter(is.na(arc_zip_co)) %>%
  dplyr::select(arc_city) %>%
  unique() %>%
  arrange(arc_city)

identical(nalox_data_addzipcity$arc_city, nalox_data_addzipcity$inccity)

na_cities_zips = nalox_data_addzipcity %>%
  filter(arc_city %in% na_cities$arc_city) %>%
  arrange(arc_city) %>% 
  group_by(arc_city, arc_zip_co) %>%
  summarize(n()) %>%
  filter(!is.na(arc_zip_co)) %>%
  dplyr::select(arc_city, arc_zip_co)

na_cities_onezip = na_cities_zips %>%
  group_by(arc_city) %>%
  summarize(Zips = n()) %>%
  filter(Zips == 1)

na_cities_replacezip = na_cities_onezip %>%
  left_join(na_cities_zips, by = "arc_city") %>%
  dplyr::select(arc_city, arc_zip_co)

for (city in na_cities_replacezip$arc_city) {
  nalox_data_addzipcity[which(nalox_data_addzipcity$arc_city == city),]$arc_zip_co <-
    na_cities_replacezip[which(na_cities_replacezip$arc_city == city),]$arc_zip_co
  
  nalox_data_addzipcity[which(nalox_data_addzipcity$arc_city == city),]$inczip <-
    na_cities_replacezip[which(na_cities_replacezip$arc_city == city),]$arc_zip_co
}
  
#zips added
nalox_data_addzipcity %>%
  filter(arc_city %in% na_cities_onezip$arc_city) %>%
  filter(is.na(arc_zip_co) | is.na(inczip))

#original data set
nalox_data %>%
  filter(arc_city %in% na_cities_onezip$arc_city) %>%
  filter(is.na(arc_zip_co) | is.na(inczip))


######## ----- patching in cities based on zips --------
na_zips = nalox_data %>%
  filter(is.na(arc_city) & !is.na(arc_zip_co)) %>%
  dplyr::select(arc_zip_co) %>%
  unique()

nalox_data %>% 
  filter(arc_zip_co %in% na_zips$arc_zip_co) %>%
  arrange(arc_zip_co)

na_cities_zips = nalox_data_addzipcity %>%
  filter(arc_zip_co %in% na_zips$arc_zip_co) %>%
  arrange(arc_zip_co) %>% 
  group_by(arc_zip_co, arc_city) %>%
  summarize(n()) %>%
  filter(!is.na(arc_city)) %>%
  dplyr::select(arc_zip_co, arc_city)

na_zips_onecity = na_cities_zips %>%
  group_by(arc_zip_co) %>%
  summarize(Cities = n()) %>%
  filter(Cities == 1)

na_zips_replacecity = na_zips_onecity %>%
  left_join(na_cities_zips, by = "arc_zip_co") %>%
  dplyr::select(arc_zip_co, arc_city) 


for (zip in na_zips_replacecity$arc_zip_co) {
  nalox_data_addzipcity[which(nalox_data_addzipcity$arc_zip_co == zip),]$arc_city <-
    na_zips_replacecity[which(na_zips_replacecity$arc_zip_co == zip),]$arc_city
  
  nalox_data_addzipcity[which(nalox_data_addzipcity$arc_zip_co == zip),]$inccity <-
    na_zips_replacecity[which(na_zips_replacecity$arc_zip_co == zip),]$arc_city
}

#check new dataset 
nalox_data_addzipcity %>%
  filter(arc_zip_co %in% na_zips_onecity$arc_zip_co) %>%
  filter(is.na(arc_city) | is.na(inccity)) 
