# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Zip Code Influenza Data         #
#               Run rsatscan                #
#              July 18, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

# what this is doing: 
# reading in the influenza data and doing weekly rsatscan runs


#### ------- load the libraries ---------- ####

# load in tidyverse and geospatial libraries (sf)
detach("package:rsatscan",unload=T)
library(rsatscan)
library(tidyverse)
library(lubridate)


#### -------- user setup ----------------- ####

if (str_detect(tolower(Sys.info()["user"]), "kelsey")) {
  user = "kelsey"
  wd_cconly = "C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data\\cc_only"
  wd_cctriage = "C:\\Users\\kelseyms\\OneDrive - University of North Carolina at Chapel Hill\\nc_detect_one_drive\\Influenza Data\\ccandtriagenotes"
  
} else if (str_detect(tolower(Sys.info()["user"]), "joyce")) {
  user = "joyce"
  wd_cconly = "C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Influenza Data\\cc_only"
  wd_cctriage = "C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Influenza Data\\ccandtriagenotes"
  
} else {
  print("Specify working directory")
}


#### -------- load in the data set for cconly  -------- ####

# set working directory
setwd(wd_cconly)

# read in the cconly data set for now
flu_data = read_csv("clean_ILIbyZIP_cconly.csv") %>%
  filter(state == "NC") %>%
  arrange(visitdate)
str(flu_data$visitweek)


#### --------- run satscan for cconly ---------- ####

# set length of study period for daily SaTScan analyses (number of days to use as baseline)
#study_length = 30
study_length = 180

#larger folder containing folders for each day
results_dir =  file.path(getwd(), paste0(study_length, "_days_WEEKLY")) 
dir.create(results_dir)
dir.create(file.path(results_dir, "weekly_shapefiles"))

# set up the first week to end the study period on
end_date0 = min(flu_data$visitdate) + study_length
end_week = epiyear(end_date0)*100 + epiweek(end_date0)


weeks = flu_data$visitweek %>%  unique()
w = match(end_week, weeks)

# create empty data frame to store all cluster information from all weeks
clusters_allweeks = data.frame()

# create empty SpatialPolygonsDataFrame to store all significant cluster shapeclust information
# sp_sig_clusters_allweeks = sp::SpatialPolygons(list()) %>%
#   sp::SpatialPolygonsDataFrame(data.frame())


# run the loop until the last week in our data
while (w <= length(weeks)){
  
  # set up the end date as the last date of the week, and set up the start date for the study period
  end_date = flu_data[which(flu_data$visitweek == end_week),]$visitdate %>% max()
  start_date = end_date - study_length
  
  # subset data to just the data between the start and end dates
  subset_data = flu_data[which(flu_data$visitdate >= start_date & flu_data$visitdate <= end_date),]
  
  # pull out just the variables of interest
  fluCas = subset_data %>% dplyr::select(zip,Count,visitdate) %>% as.data.frame()
  fluGeo = subset_data %>% dplyr::select(zip,latitude,longitude) %>% as.data.frame()
  
  # reset options
  invisible(ss.options(reset = TRUE))
  
  # see ss.options() for more information
  ss.options(list(CaseFile="flu.cas", PrecisionCaseTimes=3, #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
                  StartDate=str_replace_all(as.character(start_date), "-", "/"), 
                  EndDate=str_replace_all(as.character(end_date), "-", "/"),
                  CoordinatesFile="flu.geo", 
                  AnalysisType=4, ModelType=2, 
                  TimeAggregationUnits=3, TimeAggregationLength = 7, #time aggregation set to 7 days for week level
                  UseDistanceFromCenterOption="n",
                  MaxTemporalSizeInterpretation=1, MaxTemporalSize=7,
                  ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "fluResults.txt"))
  
  # writing files out to temporary directory
  td = tempdir()
  write.ss.prm(td, "flu") # create a PRM (parameter) file in td
  write.cas(fluCas, td, "flu") # create a Case file 
  write.geo(fluGeo, td, "flu") # create a Coordinates file 
  
  # running satscan
  if (user == "joyce"){
    flu = satscan(td, "flu")
    
  } else if (user == "kelsey") {
    flu = satscan(td, "flu", sslocation = "C:\\Program Files\\SaTScan")
    
  } else {
    print("See user setup section")
  }
  
  # look at satscan result
  summary(flu)
  summary.default(flu)
  flu$col
  
  
  if (sum(flu$shapeclust$P_VALUE < 0.05) > 0) {
    
    # extract shapeclust info for significant clusters, if any
    sp_sig_clusters = flu$shapeclust[which(flu$shapeclust$P_VALUE < 0.05),] 
    
    # delete previously created weekly shapefile folders
    dsn_path = file.path(results_dir, "weekly_shapefiles", end_date)
    if (dir.exists(dsn_path)){
      unlink(dsn_path, recursive = T)
    }
    
    # write out shapefile for the significant clusters of the week
    rgdal::writeOGR(sp_sig_clusters, dsn = dsn_path, layer = "sp_sig_clusters", drive = "ESRI Shapefile")
  
  }
  
  #sp_sig_clusters_allweeks = rbind(sp_sig_clusters_allweeks, sp_sig_clusters)
  clusters_allweeks = rbind(clusters_allweeks, flu$col)
  
  # #copy files from temporary directory to new directory
  # newdir = file.path(results_dir, as.character(end_date))  #make new folder for each individual day (within larger results_dir folder)
  # dir.create(newdir)
  # file.copy(file.path(td, list.files(td)), newdir)
  
  
  w = w + 1
  end_week = weeks[w]
  
  # clear the temp directory
  file.remove(file.path(td, list.files(td)))
  
  # end the while loop 
}

#all clusters from for loop
clusters = clusters_allweeks %>%
  dplyr::select(zip = LOC_ID, lat = LATITUDE, long = LONGITUDE, radius = RADIUS, start_date = START_DATE, 
                end_date = END_DATE, number_loc = NUMBER_LOC, p_value = P_VALUE, observed = OBSERVED, expected = EXPECTED, ode = ODE)


write.csv(clusters, file.path(results_dir, "clusters.csv"), row.names = FALSE)

# print significant clusters
sig_clusters = clusters %>%
  filter(p_value < 0.05)

write.csv(sig_clusters, file.path(results_dir, "sig_clusters.csv"), row.names = FALSE)


# significant clusters shape info from all weeks
#rgdal::writeOGR(sp_sig_clusters_allweeks, dsn = file.path(results_dir, end_date), layer = "sp_sig_clusters_allweeks", drive = "ESRI Shapefile")



####################################################################################################

#### -------- load in the data set for cc and triage notes  -------- ####

# set working directory
setwd(wd_cctriage)

# read in the cconly data set for now
flu_data = read_csv("clean_ILIbyZIP_ccandtriagenotes.csv") %>%
  filter(state == "NC") %>%
  arrange(visitdate)
str(flu_data$visitdate)


#### --------- run satscan for cc and triage notes ---------- ####

# set length of study period for daily SaTScan analyses (number of days to use as baseline)
#study_length = 30
study_length = 180

#larger folder containing folders for each day
results_dir =  file.path(getwd(), paste0(study_length, "_days_WEEKLY")) 
dir.create(results_dir)
dir.create(file.path(results_dir, "weekly_shapefiles"))


# set up the first week to end the study period on
end_date0 = min(flu_data$visitdate) + study_length
end_week = epiyear(end_date0)*100 + epiweek(end_date0)


weeks = flu_data$visitweek %>%  unique()
w = match(end_week, weeks)

# create empty data frame to store all cluster information from all weeks
clusters_allweeks = data.frame()

# create empty SpatialPolygonsDataFrame to store all significant cluster shapeclust information
sp_sig_clusters_allweeks = sp::SpatialPolygons(list()) %>%
  sp::SpatialPolygonsDataFrame(data.frame())

# run the loop until the last week in our data
while (w <= length(weeks)){
  
  #set up the end date as the last date of the week
  end_date = flu_data[which(flu_data$visitweek == end_week),]$visitdate %>% max()
  #set up the start date for the study period
  start_date = end_date - study_length
  
  # subset data to just the data between the start and end dates
  subset_data = flu_data[which(flu_data$visitdate >= start_date & flu_data$visitdate <= end_date),]
  
  # pull out just the variables of interest
  fluCas = subset_data %>% dplyr::select(zip,Count,visitdate) %>% as.data.frame()
  fluGeo = subset_data %>% dplyr::select(zip,latitude,longitude) %>% as.data.frame()
  
  # reset options
  invisible(ss.options(reset = TRUE))
  
  # see ss.options() for more information
  ss.options(list(CaseFile="flu.cas", PrecisionCaseTimes=3, #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
                  StartDate=str_replace_all(as.character(start_date), "-", "/"), 
                  EndDate=str_replace_all(as.character(end_date), "-", "/"),
                  CoordinatesFile="flu.geo", 
                  AnalysisType=4, ModelType=2, 
                  TimeAggregationUnits=3, TimeAggregationLength = 7, #time aggregation set to 7 days for week level
                  UseDistanceFromCenterOption="n",
                  MaxTemporalSizeInterpretation=1, MaxTemporalSize=7,
                  ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "fluResults.txt"))
  
  # writing files out to temporary directory
  td = tempdir()
  write.ss.prm(td, "flu") # create a PRM (parameter) file in td
  write.cas(fluCas, td, "flu") # create a Case file 
  write.geo(fluGeo, td, "flu") # create a Coordinates file 
  
  # running satscan
  if (user == "joyce"){
    flu = satscan(td, "flu")
    
  } else if (user == "kelsey") {
    flu = satscan(td, "flu", sslocation = "C:\\Program Files\\SaTScan")
    
  } else {
    print("See user setup section")
  }
  
  # look at satscan result
  summary(flu)
  summary.default(flu)
  flu$col
  
  if (sum(flu$shapeclust$P_VALUE < 0.05) > 0) {
    
    # extract shapeclust info for significant clusters, if any
    sp_sig_clusters = flu$shapeclust[which(flu$shapeclust$P_VALUE < 0.05),] 
    
    # delete previously created weekly shapefile folders
    dsn_path = file.path(results_dir, "weekly_shapefiles", end_date)
    if (dir.exists(dsn_path)){
      unlink(dsn_path, recursive = T)
    }
    
    # write out shapefile for the significant clusters of the week
    rgdal::writeOGR(sp_sig_clusters, dsn = dsn_path, layer = "sp_sig_clusters", drive = "ESRI Shapefile")
    
  }
  
  #sp_sig_clusters_allweeks = rbind(sp_sig_clusters_allweeks, sp_sig_clusters)
  clusters_allweeks = rbind(clusters_allweeks, flu$col)
  
  # #copy files from temporary directory to new directory
  # newdir = file.path(results_dir, as.character(end_date))  #make new folder for each individual day (within larger results_dir folder)
  # dir.create(newdir)
  # file.copy(file.path(td, list.files(td)), newdir)
  
  
  w = w + 1
  end_week = weeks[w]
  
  # clear the temp directory
  file.remove(file.path(td, list.files(td)))
  
  # end the while loop 
}

#all clusters from for loop
clusters = clusters_allweeks %>%
  dplyr::select(zip = LOC_ID, lat = LATITUDE, long = LONGITUDE, radius = RADIUS, start_date = START_DATE, 
                end_date = END_DATE, number_loc = NUMBER_LOC, p_value = P_VALUE, observed = OBSERVED, expected = EXPECTED, ode = ODE)


write.csv(clusters, file.path(results_dir, "clusters.csv"), row.names = FALSE)

# print significant clusters
sig_clusters = clusters %>%
  filter(p_value < 0.05)

write.csv(sig_clusters, file.path(results_dir, "sig_clusters.csv"), row.names = FALSE)

# significant clusters shape info from all weeks
#rgdal::writeOGR(sp_sig_clusters_allweeks, dsn = file.path(results_dir, end_date), layer = "sp_sig_clusters_allweeks", drive = "ESRI Shapefile")


########### --------- misc ---------------------- ####
# check weeks are Sunday to Saturday
clusters %>% 
  mutate(start_weekday = weekdays(ymd(start_date)), end_weekday = weekdays(ymd(end_date))) %>% 
  View()








