library(tidyverse)
library(rsatscan)


head(NYCfevercas)
head(NYCfevergeo)
invisible(ss.options(reset = TRUE))

#set parameters
ss.options(list(CaseFile="NYCfever.cas", PrecisionCaseTimes=3))
ss.options(c("StartDate=2001/11/1","EndDate=2001/11/24"))
ss.options(list(CoordinatesFile="NYCfever.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3))
ss.options(list(UseDistanceFromCenterOption="y", MaxSpatialSizeInDistanceFromCenter=3, NonCompactnessPenalty=0))
ss.options(list(MaxTemporalSizeInterpretation=1, MaxTemporalSize=7))
ss.options(list(ProspectiveStartDate="2001/11/24", ReportGiniClusters="n", LogRunToHistoryFile="n"))

#check what the parameter file looks like
head(ss.options(),3)


#write the parameter file, the case file, and the geometry file
td = tempdir()
write.ss.prm(td, "NYCfever")
write.cas(NYCfevercas, td, "NYCfever")
write.geo(NYCfevergeo, td, "NYCfever")


NYCfever = satscan(td, "NYCfever")
summary(NYCfever)
summary.default(NYCfever)

NYCfever$col

sp::plot(NYCfever$shapeclust)
hist(unlist(NYCfever$llr), main = "Monte Carlo")

abline(v = NYCfever$col[, c("TEST_STAT")], col = "red")



#### med drug data - Replicate Subset_7Day analysis
setwd("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Opioid Overdose Data\\SaTScan\\NC and nearby states")
MedDrugCas = read.table("Cases.cas", col.names = c("zip", "cases", "date"))
MedDrugGeo = read.table("Coordinates.geo", col.names = c("zip", "lat", "long"))
invisible(ss.options(reset = TRUE))

ss.options(list(CaseFile="MedDrug.cas", PrecisionCaseTimes=3)) #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
ss.options(list(StartDate="2018/1/1",EndDate="2019/5/9"))
ss.options(list(CoordinatesFile="MedDrug.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3, TimeAggregationLength = 7))
ss.options(list(UseDistanceFromCenterOption="n"))
ss.options(list(MaxTemporalSizeInterpretation=0, MaxTemporalSize=50))
ss.options(list(ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "./MedDrugRes.txt"))


td = tempdir()
write.ss.prm(td, "MedDrug")
write.cas(MedDrugCas, td, "MedDrug")
write.geo(MedDrugGeo, td, "MedDrug")

MedDrug = satscan(td, "MedDrug")
summary(MedDrug)
summary.default(MedDrug)


MedDrug$col #this matches with the "Subset_Prospective_7 Days.txt" SaTScan text output file!!

sp::plot(MedDrug$shapeclust)


#MedDrug - 1 day, look back 30 days, 4/9/2019-5/9/2019
setwd("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Opioid Overdose Data\\SaTScan\\NC and nearby states\\30day_5_9_2019")

subsetmeddrug_30days = read.csv("30Days_data.csv")

# subsetmeddrug_30days = subsetmeddrug %>% filter(between(as.Date(visitdate), as.Date("2019-04-09"), as.Date("2019-05-09")))

subsetmeddrug30_cas = subsetmeddrug_30days %>%
  select(zip, Count, visitdate)
subsetmeddrug30_geo = subsetmeddrug_30days %>%
  select(zip, latitude, longitude)
 
# write.csv(subsetmeddrug_30days, "./30days_data.csv")

 
invisible(ss.options(reset = TRUE))
ss.options(list(CaseFile = "30days_Cases.cas", PrecisionCaseTimes = 3, StartDate = "2019/4/9", EndDate = "2019/5/9",
                CoordinatesFile = "30days_Coordinates.geo", AnalysisType = 4, ModelType = 2, TimeAggregationUnits = 3, TimeAggregationLength = 1,
                UseDistanceFromCenterOption = "n",
                MaxTemporalSizeInterpretation = 1, MaxTemporalSize = 1,
                ReportGiniClusters = "n", LogRunToHistoryFile = "y", ResultsFile = "30days_res.txt")) 

td = "C:/Users/joyceyan/University of North Carolina at Chapel Hill/Sumner, Kelsey Marie - nc_detect_one_drive/Opioid Overdose Data/SaTScan/NC and nearby states/30day_5_9_2019/rsatscan_1daymax"
write.ss.prm(td, "30days_5_9")
write.cas(subsetmeddrug30_cas, td, "30days_Cases")
write.cas(subsetmeddrug30_geo, td, "30days_Coordinates")

subset30 = satscan(td, "30days_5_9")
summary(subset30)

subset30$col
