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
detach("package:rsatscan",unload=T)
library(rsatscan)
library(tidyverse)


#### ------- read in the data sets ---------- ####

# set working directory
setwd("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data")

#setwd("C:/Users/joyceyan/University of North Carolina at Chapel Hill/Sumner, Kelsey Marie - nc_detect_one_drive/Naloxone Geocoded Data")

# read in the cleaned naloxone data with the lat and long centroid pulled out
nalox_data0 = read_csv("./clean_nalox_data_latlong.csv")


#### ---------- automate satscan -------- ####

# set length of study period for daily SaTScan analyses (number of days to use as baseline)
study_length = 30
#study_length = 7
#study_length = 10


# check for repeat day and census tract combos
nalox_data = nalox_data0 %>%
  group_by(unitnotf_1,fips,lat,long) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  rename("cases"="count","date"="unitnotf_1") %>%
  ungroup() %>%
  mutate(date = lubridate::mdy(date)) # change date variable to date format


#larger folder containing folders for each day
results_dir =  file.path(getwd(), "satscan", paste0(study_length, "_days")) 
dir.create(results_dir)

# set up the first start and end dates
start_date = min(nalox_data$date)
end_date = start_date + study_length 

# create empty data frame to store all cluster information from all days
clusters = data.frame()

# run the loop until end_date reaches the last date in our data
while(end_date <= max(nalox_data$date)) {
    
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
  ss.options(list(StartDate=str_replace_all(as.character(start_date), "-", "/"), EndDate=str_replace_all(as.character(end_date), "-", "/")))
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
  #nalox = satscan(td, "nalox")
  nalox = satscan(td, "nalox", sslocation = "C:\\Program Files\\SaTScan")
  
  
  # look at satscan result
  summary(nalox)
  summary.default(nalox)
  nalox$col
  sp::plot(nalox$shapeclust)
  
  clusters = rbind(clusters, nalox$col)
  
  #copy files from temporary directory to new directory
  newdir = file.path(results_dir, as.character(end_date))  #make new folder for each individual day (within larger results_dir folder)
  dir.create(newdir)
  file.copy(file.path(td, list.files(td)), newdir)

  
  start_date = start_date + 1
  end_date = end_date + 1
  
# end the while loop  
}

#all clusters from for loop
clusters = clusters %>%
  select(fips = LOC_ID, lat = LATITUDE, long = LONGITUDE, radius = RADIUS, date = END_DATE, number_loc = NUMBER_LOC, p_value = P_VALUE)





#add column of county names to clusters
nc_sf = tidycensus::get_acs(geography = "tract", state = "NC", 
                            variables = "B19013_001", #B19013_001 is median household income, #B01001_001 is total of males and females
                            summary_var = "B01001_001", geometry = TRUE,  
                            key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0") %>%
  rename(totpop = summary_est)

clusters_countynames = clusters %>%
  left_join(nc_sf, by = c("fips" = "GEOID")) %>%
  select(fips, lat, long, radius, date, NAME, number_loc, p_value) %>%
  mutate(county = str_extract(NAME, "(?<=,\\s)[^,]+(?=\\sCounty)")) %>% #pull characters between ", " and " County"
  select(-NAME)

# export all clusters, regardless of significance
write.csv(clusters_countynames, file.path(results_dir, "clusters.csv"))

# zip codes are not perfectly nested within census tracts/counties so cannot add

#significant clusters
sig_clusters = clusters_countynames %>%
  filter(p_value < 0.05)

# write out the significant clusters
write_csv(sig_clusters, file.path(results_dir,"sig_clusters.csv"))

# cluster data with p-values and geometries -- make into shapefile?
cluster_sf = clusters %>%
  left_join(nc_sf, by = c("fips" = "GEOID")) %>%
  select(fips, date, NAME, number_loc, p_value, lat, long, radius, geometry) %>%
  mutate(county = str_extract(NAME, "(?<=,\\s)[^,]+(?=\\sCounty)")) %>% #pull characters between ", " and " County"
  select(-NAME) %>%
  filter(p_value < 0.05)

# write out cluster_sf to a shapefile
if (sum(str_detect(list.files(results_dir), "days_satscan_sig_clusters")) > 0) {
  file.remove(file.path(results_dir, list.files(results_dir)[str_detect(list.files(results_dir), "days_satscan_sig_clusters")]))
}

sf::st_write(cluster_sf,file.path(results_dir,paste0(study_length,"days_satscan_sig_clusters.shp")))




