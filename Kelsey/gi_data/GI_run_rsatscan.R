lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)


library(rsatscan)
library(tidyverse)
library(zipcode)
data("zipcode")

setwd("C:/Users/joyceyan/University of North Carolina at Chapel Hill/Sumner, Kelsey Marie - nc_detect_one_drive/GI visit data")

gi_data = read.csv("./GIVomDZIP_clean.csv") %>%
  filter(str_detect(ZIP, "[0-9]")) %>%
  mutate(ZIP = str_pad(ZIP, 5, c("left"), pad = "0")) %>%
  mutate(visitdate = lubridate::ymd(visitdate)) %>%
  filter(!is.na(city) & !is.na(latitude) & !is.na(longitude)) %>%
  select(cases = EDVisits, zip = ZIP, date = visitdate, city, state, latitude, longitude)

gi_data = gi_data %>%
  filter(state == "NC")

#### ---------- automate satscan -------- ####

# set length of study period for daily SaTScan analyses (number of days to use as baseline)
study_length = 30
#study_length = 7
#study_length = 10


#larger folder containing folders for each day
results_dir =  file.path(getwd(), "rsatscan", paste0(study_length, "_days")) 
dir.create(results_dir)

# set up the first start and end dates
start_date = min(gi_data$date)
end_date = start_date + study_length 

# create empty data frame to store all cluster information from all days
clusters = data.frame()



# run the loop until end_date reaches the last date in our data
while(end_date <= max(gi_data$date)) {
  
  # subset data to just the data between the start and end dates
  subset_data = gi_data[which(gi_data$date >= start_date & gi_data$date <= end_date),]
  
  # pull out just the variables of interest
  giCas = subset_data %>% select(zip,cases,date) %>% as.data.frame()
  giGeo = subset_data %>% select(zip,latitude,longitude) %>% as.data.frame()
  
  # reset options
  invisible(ss.options(reset = TRUE))
  
  # see ss.options() for more information
  ss.options(list(CaseFile="gi.cas", PrecisionCaseTimes=3)) #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
  ss.options(list(StartDate=str_replace_all(as.character(start_date), "-", "/"), EndDate=str_replace_all(as.character(end_date), "-", "/")))
  ss.options(list(CoordinatesFile="gi.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3, TimeAggregationLength = 1))
  ss.options(list(UseDistanceFromCenterOption="n"))
  ss.options(list(MaxTemporalSizeInterpretation=1, MaxTemporalSize=1))
  ss.options(list(ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "./giResults.txt"))
  
  # writing files out to temporary directory
  td = tempdir()
  write.ss.prm(td, "gi") # create a PRM (parameter) file in td
  write.cas(giCas, td, "gi") # create a Case file 
  write.geo(giGeo, td, "gi") # create a Coordinates file 
  
  # running satscan
  gi = satscan(td, "gi")
  #gi = satscan(td, "gi", sslocation = "C:\\Program Files\\SaTScan")
  
  
  # look at satscan result
  #summary(gi)
  summary.default(gi)
  gi$col
  #sp::plot(gi$shapeclust)
  
  if (!is.na(gi$col)){
    clusters = rbind(clusters, gi$col)
  }
  
  # #copy files from temporary directory to new directory
  # newdir = file.path(results_dir, as.character(end_date))  #make new folder for each individual day (within larger results_dir folder)
  # dir.create(newdir)
  # file.copy(file.path(td, list.files(td)), newdir)
  
  # clear the temp directory
  #file.remove(file.path(td, list.files(td)))
  
  start_date = start_date + 1
  end_date = end_date + 1
  
  
# end the while loop  
}



#all clusters from for loop
clusters = clusters %>%
  select(zip = LOC_ID, lat = LATITUDE, long = LONGITUDE, radius = RADIUS, date = END_DATE, number_loc = NUMBER_LOC, p_value = P_VALUE)


write.csv(clusters, file.path(results_dir, "clusters.csv"), row.names = FALSE)

# print significant clusters
sig_clusters = clusters %>%
  filter(p_value < 0.05)

write.csv(sig_clusters, file.path(results_dir, "sig_clusters.csv"), row.names = FALSE)





