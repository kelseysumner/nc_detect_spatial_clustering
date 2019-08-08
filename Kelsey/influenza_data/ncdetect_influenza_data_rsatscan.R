# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Zip Code Influenza Data         #
#               Run rsatscan                #
#              July 18, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

# what this is doing: 
# reading in the influenza data and doing daily rsatscan runs


#### ------- load the libraries ---------- ####

# load in tidyverse and geospatial libraries (sf)
detach("package:rsatscan",unload=T)
library(rsatscan)
library(tidyverse)


#### -------- load in the data set for cconly  -------- ####

# set working directory
setwd("C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data\\cconly")

# read in the cconly data set for now
flu_data = read_csv("clean_ILIbyZIP_cconly.csv") %>%
  mutate(visitdate = lubridate::mdy(visitdate)) %>%
  filter(state == "NC")
str(flu_data$visitdate)


#### --------- run satscan for cconly ---------- ####

# set length of study period for daily SaTScan analyses (number of days to use as baseline)

#study_length = 30
study_length = 7

#larger folder containing folders for each day
results_dir =  file.path(getwd(), paste0(study_length, "_days")) 
dir.create(results_dir)

# set up the first start and end dates
start_date = min(flu_data$visitdate)
end_date = start_date + study_length 


# create empty data frame to store all cluster information from all days
clusters_alldays = data.frame()

# run the loop until end_date reaches the last date in our data
while(end_date <= max(flu_data$visitdate)) {
  
  # subset data to just the data between the start and end dates
  subset_data = flu_data[which(flu_data$visitdate >= start_date & flu_data$visitdate <= end_date),]
  
  # pull out just the variables of interest
  fluCas = subset_data %>% dplyr::select(zip,Count,visitdate) %>% as.data.frame()
  fluGeo = subset_data %>% dplyr::select(zip,latitude,longitude) %>% as.data.frame()
  
  # reset options
  invisible(ss.options(reset = TRUE))
  
  # see ss.options() for more information
  ss.options(list(CaseFile="flu.cas", PrecisionCaseTimes=3)) #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
  ss.options(list(StartDate=str_replace_all(as.character(start_date), "-", "/"), EndDate=str_replace_all(as.character(end_date), "-", "/")))
  ss.options(list(CoordinatesFile="flu.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3, TimeAggregationLength = 1))
  ss.options(list(UseDistanceFromCenterOption="n"))
  ss.options(list(MaxTemporalSizeInterpretation=1, MaxTemporalSize=1))
  ss.options(list(ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "./fluResults.txt"))
  
  # writing files out to temporary directory
  td = tempdir()
  write.ss.prm(td, "flu") # create a PRM (parameter) file in td
  write.cas(fluCas, td, "flu") # create a Case file 
  write.geo(fluGeo, td, "flu") # create a Coordinates file 
  
  # running satscan
  #flu = satscan(td, "flu")
  flu = satscan(td, "flu", sslocation = "C:\\Program Files\\SaTScan")
  
  # look at satscan result
  summary(flu)
  summary.default(flu)
  flu$col
  #sp::plot(flu$shapeclust)
  
  clusters_alldays = rbind(clusters_alldays, flu$col)
  
  # #copy files from temporary directory to new directory
  # newdir = file.path(results_dir, as.character(end_date))  #make new folder for each individual day (within larger results_dir folder)
  # dir.create(newdir)
  # file.copy(file.path(td, list.files(td)), newdir)
  
  
  start_date = start_date + 1
  end_date = end_date + 1
  
  # clear the temp directory
  file.remove(file.path(td, list.files(td)))
  
  # end the while loop 
}

#all clusters from for loop
clusters = clusters_alldays %>%
  dplyr::select(zip = LOC_ID, lat = LATITUDE, long = LONGITUDE, radius = RADIUS, date = END_DATE, number_loc = NUMBER_LOC, p_value = P_VALUE)


write.csv(clusters, file.path(results_dir, "clusters.csv"), row.names = FALSE)

# print significant clusters
sig_clusters = clusters %>%
  filter(p_value < 0.05)

write.csv(sig_clusters, file.path(results_dir, "sig_clusters.csv"), row.names = FALSE)



#### -------- load in the data set for cc and triage notes  -------- ####

# set working directory
setwd("C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data\\ccandtriagenotes")

# read in the cconly data set for now
flu_data = read_csv("clean_ILIbyZIP_ccandtriagenotes.csv") %>%
  mutate(visitdate = lubridate::mdy(visitdate)) %>%
  filter(state == "NC")
str(flu_data$visitdate)


#### --------- run satscan for cc and triage notes ---------- ####

# set length of study period for daily SaTScan analyses (number of days to use as baseline)

#study_length = 30
study_length = 7

#larger folder containing folders for each day
results_dir =  file.path(getwd(), paste0(study_length, "_days")) 
dir.create(results_dir)

# set up the first start and end dates
start_date = min(flu_data$visitdate)
end_date = start_date + study_length 


# create empty data frame to store all cluster information from all days
clusters_alldays = data.frame()

# run the loop until end_date reaches the last date in our data
while(end_date <= max(flu_data$visitdate)) {
  
  # subset data to just the data between the start and end dates
  subset_data = flu_data[which(flu_data$visitdate >= start_date & flu_data$visitdate <= end_date),]
  
  # pull out just the variables of interest
  fluCas = subset_data %>% dplyr::select(zip,Count,visitdate) %>% as.data.frame()
  fluGeo = subset_data %>% dplyr::select(zip,latitude,longitude) %>% as.data.frame()
  
  # reset options
  invisible(ss.options(reset = TRUE))
  
  # see ss.options() for more information
  ss.options(list(CaseFile="flu.cas", PrecisionCaseTimes=3)) #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
  ss.options(list(StartDate=str_replace_all(as.character(start_date), "-", "/"), EndDate=str_replace_all(as.character(end_date), "-", "/")))
  ss.options(list(CoordinatesFile="flu.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3, TimeAggregationLength = 1))
  ss.options(list(UseDistanceFromCenterOption="n"))
  ss.options(list(MaxTemporalSizeInterpretation=1, MaxTemporalSize=1))
  ss.options(list(ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "./fluResults.txt"))
  
  # writing files out to temporary directory
  td = tempdir()
  write.ss.prm(td, "flu") # create a PRM (parameter) file in td
  write.cas(fluCas, td, "flu") # create a Case file 
  write.geo(fluGeo, td, "flu") # create a Coordinates file 
  
  # running satscan
  #flu = satscan(td, "flu")
  flu = satscan(td, "flu", sslocation = "C:\\Program Files\\SaTScan")
  
  # look at satscan result
  summary(flu)
  summary.default(flu)
  flu$col
  #sp::plot(flu$shapeclust)
  
  clusters_alldays = rbind(clusters_alldays, flu$col)
  
  # #copy files from temporary directory to new directory
  # newdir = file.path(results_dir, as.character(end_date))  #make new folder for each individual day (within larger results_dir folder)
  # dir.create(newdir)
  # file.copy(file.path(td, list.files(td)), newdir)
  
  
  start_date = start_date + 1
  end_date = end_date + 1
  
  # clear the temp directory
  file.remove(file.path(td, list.files(td)))
  
  # end the while loop 
}

#all clusters from for loop
clusters = clusters_alldays %>%
  dplyr::select(zip = LOC_ID, lat = LATITUDE, long = LONGITUDE, radius = RADIUS, date = END_DATE, number_loc = NUMBER_LOC, p_value = P_VALUE)


write.csv(clusters, file.path(results_dir, "clusters.csv"), row.names = FALSE)

# print significant clusters
sig_clusters = clusters %>%
  filter(p_value < 0.05)

write.csv(sig_clusters, file.path(results_dir, "sig_clusters.csv"), row.names = FALSE)









