# ----------------------------------------------- #
#          NC DETECT Spatial Project              #
#           Naloxone Geocoded Data                #
#      Try running rsatscan on the data set       #
#              June 6, 2019                       #
#                K. Sumner                        #
# ----------------------------------------------- #

# what this is doing: 
# reading in the naloxone shapefile (by day) and running rsatscan on it


#### ------- load the libraries ---------- ####

# load in the libraries of interest
library(rsatscan)
library(tidyverse)


#### ------- read in the data sets ---------- ####

# set working directory
setwd("C://Users//kelseyms//OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data")

#setwd("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Naloxone Geocoded Data")

# read in the cleaned naloxone data with the lat and long centroid pulled out
nalox_data0 = read_csv("./clean_nalox_data_latlong.csv")



#### ------- try running rsatscan on the data set --------- ####

# check for repeat day and census tract combos
nalox_data = nalox_data0 %>%
  group_by(unitnotf_1,fips,lat,long) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  rename("cases"="count","date"="unitnotf_1") %>%
  ungroup()

# write out as a csv
# write_csv(nalox_data,"nalox_data_aggregated.csv")

# change date variable to date format
nalox_data$date = lubridate::mdy(nalox_data$date)

# subset data to just january
nalox_data = nalox_data[which(lubridate::month(nalox_data$date)==1),]

# pull out just the variables of interest
colnames(nalox_data)
naloxCas = nalox_data %>% select(fips,cases,date)
naloxGeo = nalox_data %>% select(fips,lat,long)
naloxCas = as.data.frame(naloxCas)
naloxGeo = as.data.frame(naloxGeo)

# reset options
invisible(ss.options(reset = TRUE))

# see ss.options() for more information
ss.options(list(CaseFile="nalox.cas", PrecisionCaseTimes=3)) #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
ss.options(list(StartDate="2017/01/01",EndDate="2017/01/31"))
ss.options(list(CoordinatesFile="nalox.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3, TimeAggregationLength = 1))
ss.options(list(UseDistanceFromCenterOption="n"))
ss.options(list(MaxTemporalSizeInterpretation=1, MaxTemporalSize=1))
ss.options(list(ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "./naloxResults.txt"))

# writing files out to temporary directory
td = tempdir() # the string stored in 'td' is the temporary directory where all the SaTScan files will be
write.ss.prm(td, "nalox") # create a PRM (parameter) file in td
write.cas(naloxCas, td, "nalox") # create a Case file in td called "MedDrug.cas"
write.geo(naloxGeo, td, "nalox") # create a Coordinates file in td called "MedDrug.geo"

# running satscan
nalox = satscan(td, "nalox", sslocation = "C:\\Program Files\\SaTScan")

# look at satscan result
summary(nalox)
summary.default(nalox)
nalox$col
sp::plot(nalox$shapeclust)





#### ---------- now figure out a way to automate satscan -------- ####

# check for repeat day and census tract combos
nalox_data = nalox_data0 %>%
  group_by(unitnotf_1,fips,lat,long) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  rename("cases"="count","date"="unitnotf_1") %>%
  ungroup()

# change date variable to date format
nalox_data$date = lubridate::mdy(nalox_data$date)

# set up the first start and end dates
orig_start_date=lubridate::mdy("12/31/2016")
orig_end_date=lubridate::mdy("1/30/2017")

# start the for loop
for (i in 1:5){   # used first 30 days of year as baseline (365-30=335)
  
  # set up dates
  start_date = str_replace_all(as.character(orig_start_date+i),"-","/")
  end_date = str_replace_all(as.character(orig_end_date+i),"-","/")
  
  # subset data to just the data between the start and end dates
  subset_data = nalox_data[which(nalox_data$date >= start_date & nalox_data$date <= end_date),]
  
  # pull out just the variables of interest
  naloxCas = subset_data %>% select(fips,cases,date)
  naloxGeo = subset_data %>% select(fips,lat,long)
  naloxCas = as.data.frame(naloxCas)
  naloxGeo = as.data.frame(naloxGeo)
  
  # reset options
  invisible(ss.options(reset = TRUE))
  
  # see ss.options() for more information
  ss.options(list(CaseFile="nalox.cas", PrecisionCaseTimes=3)) #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
  ss.options(list(StartDate=start_date,EndDate=end_date))
  ss.options(list(CoordinatesFile="nalox.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3, TimeAggregationLength = 1))
  ss.options(list(UseDistanceFromCenterOption="n"))
  ss.options(list(MaxTemporalSizeInterpretation=1, MaxTemporalSize=1))
  ss.options(list(ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "./naloxResults.txt"))
  
  # writing files out to temporary directory
  td = tempdir()
  write.ss.prm(td, "nalox") # create a PRM (parameter) file in td
  write.cas(naloxCas, td, "nalox") # create a Case file 
  write.geo(naloxGeo, td, "nalox") # create a Coordinates file 
  
  # running satscan
  nalox = satscan(td, "nalox", sslocation = "C:\\Program Files\\SaTScan")
  
  
  # look at satscan result
  summary(nalox)
  summary.default(nalox)
  nalox$col
  sp::plot(nalox$shapeclust)
  
  
  #copy files from temporary directory to new directory
  newdir = paste0("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data/satscan/",as.character(i))
  #newdir = paste0("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Naloxone Geocoded Data\\satscan\\",as.character(i))
  dir.create(newdir)
  file.copy(file.path(td, list.files(td)), newdir)
  
# end the for loop  
}


















