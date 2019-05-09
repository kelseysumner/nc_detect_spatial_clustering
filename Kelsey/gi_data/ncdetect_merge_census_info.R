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
str(ncd$month_year)
str(ncd$ed_count_vomd_for_month)
# recode zip clean to a character
ncd$zip_clean = as.character(ncd$zip_clean)
str(ncd$zip_clean)

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

# make sure gi_merged_data is coded correctly
str(gi_merged_data$OBJECTID)
str(gi_merged_data$ZIP)
gi_merged_data$ZIP = as.character(gi_merged_data$ZIP)
str(gi_merged_data$PO_NAME)

# now subset nc_sf to just the geoids that are in the zip code dbf file
nc_sf_seregion = nc_sf %>%
  filter(nc_sf$GEOID %in% gi_merged_data$ZIP)
# check the subset
length(unique(gi_merged_data$ZIP))
length(which(nc_sf$GEOID %in% gi_merged_data$zip_clean))
length(intersect(nc_sf$GEOID,gi_merged_data$ZIP))
length(which(nc_sf$GEOID == "28671"))

# now subset this_data to just the geoid that are in the zip code dbf file
this_data_seregion = this_data %>%
  filter(this_data$GEOID %in% gi_merged_data$ZIP)
# check the subset
length(unique(gi_merged_data$ZIP))
length(which(this_data_seregion$GEOID %in% gi_merged_data$ZIP))

# check how many zip codes are in both the nc detect data and the gi merged data
length(unique(ncd$zip_clean)) # 5105 
length(intersect(ncd$zip_clean,this_data$GEOID)) # 4749
ncd = rename(ncd,"GEOID"="zip_clean")
test = anti_join(ncd,this_data,by="GEOID")
# some of these zip codes exist and are in NC so not sure why not merging

# first merge in the ncd data to this_data_seregion and nc_sf_seregion
# nc_sf_seregion
sf_merged = right_join(nc_sf_seregion,ncd,by="GEOID")
table(sf_merged$month_year)
colnames(sf_merged)
# this_data_seregin
this_data_merged = right_join(this_data_seregion,ncd,by="GEOID")
table(this_data_merged$month_year)
colnames(this_data_merged)


#### --------- make plots and maps of the data --------- ####

# make a plot of the incidence by month
sf_merged %>% filter(month_year=="1-2018") %>% ggplot() + geom_sf(aes(fill=ed_count_vomd_for_month)) 

# make a plot of the incidence across all months
# nc_sf %>% left_join(ncd %>% spread(month_year, ed_count_vomd_for_month)) %>% plot(max.plot=16) # down and dirty small multiples
# this is freezing up

# look at the nc_sf data gi incidence counts
sf_merged %>% 
  ggplot()+
  geom_sf(aes(fill=ed_count_vomd_for_month)) + 
  facet_wrap(~month_year) +
  theme_minimal() # long-form data, ggplot style. this would be better as a rate! Hard to see differences.

# look at the nc_sf data gi incidence rates
sf_merged %>% 
  mutate(rate = ed_count_vomd_for_month / totpop * 1000) %>%
  ggplot()+
  geom_sf(aes(fill=rate)) + 
  facet_wrap(~month_year) +
  theme_minimal() # rates, but a few strong outliers.

# make a map of the counts scaled by place groups
sf_merged %>% 
  group_by(GEOID) %>% mutate(scaled_count = scale(ed_count_vomd_for_month)) %>% # scale counts within place groups
  ggplot()+
  geom_sf(aes(fill=scaled_count)) + 
  facet_wrap(~month_year) +
  theme_minimal() # down and dirty - scaling to help see the global trend.

# ignore space completely and look at relationship over time
ncd %>% ggplot(aes(month_year, ed_count_vomd_for_month, color=ed_count_vomd_for_month, group=GEOID)) + geom_line() # ignore space - spaghetti. meh.


#### -------- look at time slopes --------- ####

# make another variable that says what the symptoms are
ncd$symptoms = rep("gi_symptoms",nrow(ncd))

# make geoid a factor
ncd$GEOID = as.character(ncd$GEOID)

# look for any missing observations in ncd
length(which(is.na(ncd$GEOID)))
length(which(is.na(ncd$month_year)))
length(which(is.na(ncd$ed_count_vomd_for_month)))
length(which(is.na(ncd$symptoms)))
str(ncd$symptoms)
NA_preproc(ncd)
str(ncd$symptoms)
str(ncd$GEOID)
str(ncd$month_year)
ncd$month_year = as.factor(ncd$month_year)
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
  mutate(model = map(data, ~ lm(data=.x, ed_count_vomd_for_month ~ month_year)),
         tidy = map(model, broom::tidy),
         slope = map_dbl(tidy, ~.x$estimate[2]))
ncd_model_df # check out this data structure
nc_sf_slopes = nc_sf %>% left_join(ncd_model_df %>% select(place, slope)) 
# having some trouble getting this to work







