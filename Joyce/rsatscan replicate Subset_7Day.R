library(tidyverse)
library(rsatscan)


#### med drug data - Replicate Subset_7Day analysis
setwd("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Opioid Overdose Data/SaTScan/NC and nearby states")

#setwd("C:\\Users\\joyceyan\\University of North Carolina at Chapel Hill\\Sumner, Kelsey Marie - nc_detect_one_drive\\Opioid Overdose Data\\SaTScan\\NC and nearby states")

MedDrugCas = read.table("Cases.cas", col.names = c("zip", "cases", "date"))
MedDrugGeo = read.table("Coordinates.geo", col.names = c("zip", "lat", "long"))
invisible(ss.options(reset = TRUE))

# see ss.options() for more information
ss.options(list(CaseFile="MedDrug.cas", PrecisionCaseTimes=3)) #0 - None, 1 = Year, 2 = Month, 3 = Day, 4 = Generic
ss.options(list(StartDate="2018/1/1",EndDate="2019/5/9"))
ss.options(list(CoordinatesFile="MedDrug.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3, TimeAggregationLength = 7))
ss.options(list(UseDistanceFromCenterOption="n"))
ss.options(list(MaxTemporalSizeInterpretation=0, MaxTemporalSize=50))
ss.options(list(ProspectiveStartDate="2019/5/9", ReportGiniClusters="n", LogRunToHistoryFile="y", ResultsFile = "./MedDrugRes.txt"))


td = tempdir() # the string stored in 'td' is the temporary directory where all the SaTScan files will be
write.ss.prm(td, "MedDrug") # create a PRM (parameter) file in td
write.cas(MedDrugCas, td, "MedDrug") # create a Case file in td called "MedDrug.cas"
write.geo(MedDrugGeo, td, "MedDrug") # create a Coordinates file in td called "MedDrug.geo"

MedDrug = satscan(td, "MedDrug")
summary(MedDrug)
summary.default(MedDrug)

# see the cluster information
MedDrug$col #<------ this info matches with the "Subset_Prospective_7 Days.txt" SaTScan text output file!


# can plot the clusters
sp::plot(MedDrug$shapeclust)
