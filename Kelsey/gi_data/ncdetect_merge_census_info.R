# ----------------------------------------- #
#          NC DETECT Spatial Project        #
#           Vomiting/Diarrhea Data          #
#    Trying to Merge in Census Data in R    #
#            Mike Fliss's Method            #
#               May 9, 2019                 #
#                K. Sumner                  #
# ----------------------------------------- #


# what this is doing: 
# reading in the gi data set by zip code at the monthly level, merging with census data, and making spark plots from

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
setwd("/Users/kelseysumner/Desktop/NC DETECT")

# read in the dbf file for the SE region shapefile
gi_merged_data = read.dbf("Shapefiles/zip_seregion_with_gi_data/gi_data_seregion_by_crossection.dbf")

# read in the zip code gi symptom count data at the monthly level
ncd = read_csv("GI visit data/gi_case_data_by_month.csv")

# load in mike's data just for testing code
mikedata = read_csv("Materials from Mike Fliss/ncd_test_data.csv")
table(mikedata$measure, useNA="always")


#### ------ merge gi count data with census data --------- ####

# make sure the gi count data is coded correctly
str(ncd$zip_clean)
str(ncd$month)
str(ncd$ed_count_vomd_for_month)

# code the GI data so you have one observation for every month for just 2018
length(unique(ncd$zip_clean))
ncd_month_all = data.frame(zip = rep(unique(ncd$zip_clean),12))
# create a vector that is 1:12 repeated 5105 times each
month = c(rep(1,length(unique(ncd$zip_clean))),rep(2,length(unique(ncd$zip_clean))),rep(3,length(unique(ncd$zip_clean))),rep(4,length(unique(ncd$zip_clean))),rep(5,length(unique(ncd$zip_clean))),
          rep(6,length(unique(ncd$zip_clean))),rep(7,length(unique(ncd$zip_clean))),rep(8,length(unique(ncd$zip_clean))),rep(9,length(unique(ncd$zip_clean))),rep(10,length(unique(ncd$zip_clean))),
          rep(11,length(unique(ncd$zip_clean))),rep(12,length(unique(ncd$zip_clean))))
length(month)
# add the month variable to the new data frame
ncd_month_all$month = month

# edit the ncd monthly cases data set
# make a variable just for month
only_month = rep(NA,nrow(ncd))
for (i in 1:nrow(ncd)){
  only_month[i] = str_split(ncd$month_year[i],"-")[[1]][1]
}
head(only_month)
head(ncd$month_year)
ncd$month = only_month
# make a variable just for year
only_year = rep(NA,nrow(ncd))
for (i in 1:nrow(ncd)){
  only_year[i] = str_split(ncd$month_year[i],"-")[[1]][2]
}
head(only_year)
head(ncd$month_year)
ncd$year = only_year
# remove everything with the year 2019
length(which(ncd$year == 2019)) # 4745 observations, 21121 total obs in data
ncd = ncd[which(ncd$year==2018),] # 16376, looks like it worked correctly
# remove the month-year and year variables
ncd$month_year = NULL
ncd$year = NULL
# rename zip_clean to zip
ncd = rename(ncd, "zip" = "zip_clean")

# check that the month and zip code columns are coded correctly in both data sets
str(ncd$month)
ncd$month = as.numeric(ncd$month)
str(ncd_month_all$month)
str(ncd$zip)
str(ncd_month_all$zip)
ncd_month_all$zip = as.character(ncd_month_all$zip)

# now merge in the case data set with the data set for every month
ncd_merge = left_join(ncd_month_all,ncd,by=c("zip","month"))
# check the merge
length(which(!(is.na(ncd_merge$ed_count_vomd_for_month)))) # looks like everything merged in
# check that everything mergedf in the right place
ncd[which(ncd$zip=="10003" & ncd$month==1),]
ncd_month_all[which(ncd_month_all$zip=="10003" & ncd_month_all$month==1),]
# looks like the merge worked correctly
# change all the NAs to 0s
ncd_merge$ed_count_vomd_for_month[which(is.na(ncd_merge$ed_count_vomd_for_month))] = 0
length(which(ncd_merge$ed_count_vomd_for_month==0)) # 49541 (66384-16843=49541) # looks like worked correctly


# make a list of all the se region zip codes of interest from the dbf file
# zip codes for: NC, Georgia, Tennessee, SC, and Virginia

# Get population data from tidy census 
this_data = tidycensus::get_acs(geography = "zcta",
                                survey="acs5", year=2017, variables = c("B03002_001E", "B03002_003E", "B03002_004E"), 
                                key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0", output = "wide")
this_data = this_data %>%
  rename(totpop = B03002_001E, WnH = B03002_003E, BnH = B03002_004E) 


# get spatial data from tidy census
nc_sf = tidycensus::get_acs(geography = "zip code tabulation area", 
                                   variables = "B19013_001",
                                   summary_var = "B01001_001", geometry = TRUE,  
                                   key="23ce49809ba6fbffdf7a68cc93010b5e171ba5e0") %>%
                  rename(totpop = estimate)
# plot(nc_sf %>% st_geometry()) 
# note that for faster plotting, consider simplifying the spatial geometry - you don't need foot-level resolution. 
# Look at st_simplify() or https://mapshaper.org/ or rmapshaper.

# make sure the this_data is coded correctly
str(this_data$GEOID)
str(this_data$NAME)
str(this_data$totpop)
str(this_data$B03002_001M)
str(this_data$WnH)
str(this_data$B03002_003M)
str(this_data$BnH)
str(this_data$B03002_004M)

# make sure nc_sf is coded correctly
str(nc_sf$GEOID)
str(nc_sf$NAME)
str(nc_sf$variable)
str(nc_sf$totpop)
str(nc_sf$moe)
str(nc_sf$summary_est)
str(nc_sf$summary_moe)

# code zip variable to geoid in ncd_merge data
ncd_merge = rename(ncd_merge,"GEOID"="zip")

# first merge in the ncd data to this_data_seregion and nc_sf_seregion
# nc_sf_seregion
sf_merged = left_join(nc_sf,ncd_merge,by="GEOID")
table(sf_merged$month)
colnames(sf_merged)
length(unique(sf_merged$GEOID)) # 5160
sf_merged = na.omit(sf_merged)
length(unique(sf_merged$GEOID)) # 5099
table(sf_merged$month)
# this_data_seregin
this_data_merged = left_join(this_data,ncd_merge,by="GEOID")
table(this_data_merged$month)
colnames(this_data_merged)
length(unique(this_data_merged$GEOID)) # 5160
this_data_merged = na.omit(this_data_merged)
length(unique(this_data_merged$GEOID)) # 5159
table(this_data_merged$month)

# write out the sf_merged object as a shapefile
st_write(sf_merged, "month_gi_data_zip_code_level.shp")


#### --------- make plots and maps of the data --------- ####

# make a plot of the incidence by month
sf_merged %>% filter(month=="1") %>% ggplot() + geom_sf(aes(fill=ed_count_vomd_for_month)) 

# make a map of the incidence across month 12
sf_merged %>%
  filter(month=="12") %>%
  ggplot(aes(fill = ed_count_vomd_for_month)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma")

# make a plot of the incidence across all months
sf_merged_wide  = sf_merged %>% spread(month,ed_count_vomd_for_month) 
colnames(sf_merged_wide)
sf_merged_wide_months = sf_merged_wide[,c("1","2","3","4","5","6","7","8","9","10","11","12")]
sf_merged_wide_months %>% plot(max.plot=12) # down and dirty small multiples
# this runs very slowly

# look at the nc_sf data gi incidence counts
sf_merged %>% 
  ggplot()+
  geom_sf(aes(fill=ed_count_vomd_for_month)) + 
  facet_wrap(~month) +
  theme_minimal() # long-form data, ggplot style. this would be better as a rate! Hard to see differences.

# look at the nc_sf data gi incidence rates
sf_merged %>% 
  mutate(rate = ed_count_vomd_for_month / totpop * 1000) %>%
  ggplot()+
  geom_sf(aes(fill=rate)) + 
  facet_wrap(~month) +
  theme_minimal() # rates, but a few strong outliers.

# make a map of the counts scaled by place groups
sf_merged %>% 
  group_by(GEOID) %>% mutate(scaled_count = scale(ed_count_vomd_for_month)) %>% # scale counts within place groups
  ggplot()+
  geom_sf(aes(fill=scaled_count)) + 
  facet_wrap(~month) +
  theme_minimal() # down and dirty - scaling to help see the global trend.

# ignore space completely and look at relationship over time
ncd_merge %>% ggplot(aes(month, ed_count_vomd_for_month, color=ed_count_vomd_for_month, group=GEOID)) + geom_line() # ignore space - spaghetti. meh.


#### -------- look at time slopes --------- ####

# make another variable that says what the symptoms are
ncd$symptoms = rep("gi_symptoms",nrow(ncd))

# make geoid a factor
ncd$GEOID = as.character(ncd$GEOID)

# look for any missing observations in ncd
length(which(is.na(ncd$GEOID)))
length(which(is.na(ncd$month)))
length(which(is.na(ncd$ed_count_vomd_for_month)))
length(which(is.na(ncd$symptoms)))
str(ncd$symptoms)
NA_preproc(ncd)
str(ncd$symptoms)
str(ncd$GEOID)
str(ncd$month)
ncd$month = as.factor(ncd$month)
str(ncd$ed_count_vomd_for_month)
ncd = na.omit(ncd)
ncd$symptoms = as.factor(ncd$symptoms)

# check the variables
## get mode of all vars
var_mode <- sapply(ncd, mode)
## produce error if complex or raw is found
if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
## get class of all vars
var_class <- sapply(ncd, class)
## produce error if an "AsIs" object has "logical" or "character" mode
if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
  stop("matrix variables with 'AsIs' class must be 'numeric'")
}
## identify columns that needs be coerced to factors
ind1 <- which(var_mode %in% c("logical", "character"))
## coerce logical / character to factor with `as.factor`
ncd[ind1] <- lapply(ncd[ind1], as.factor)
## index of factor columns
fctr <- which(sapply(ncd, is.factor))
## factor variables that have skipped explicit conversion in step 2
## don't simply do `ind2 <- fctr[-ind1]`; buggy if `ind1` is `integer(0)`
ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
## drop unused levels
ncd[ind2] <- lapply(ncd[ind2], droplevels)
## export factor levels actually used by `lm` and `glm`
lev <- lapply(ncd[fctr], levels)
## count number of levels
nl <- lengths(lev)

# make a data frame of the incidence data
ncd_model_df = ncd %>% 
  group_by(symptoms,GEOID) %>% 
  nest() %>%
  mutate(model = map(data, ~ lm(data=.x, ed_count_vomd_for_month ~ month)),
         tidy = map(model, broom::tidy),
         slope = map_dbl(tidy, ~.x$estimate[2]))
ncd_model_df # check out this data structure
nc_sf_slopes = nc_sf %>% left_join(ncd_model_df %>% select(place, slope)) 
# having some trouble getting this to work







