How to Automate SaTScan and Automate Generation of Weekly Cluster Maps:


1. ncdetect_influenza_data_cleaning.R

This script reads in the data set, matches ZIP codes to latitude and longitude values, gets rid of rows with invalid ZIP codes, and assigns a "week and year" index to each row in order to categorize rows into weeks.



2. ncdetect_influenza_data_rsatscan_weekly.R

This script reads in the cleaned data set, limits the data set to NC visits only, and runs SaTScan on the data to find weekly clusters based on a 180-day baseline.  

If any week has significant clusters, then a shapefile is outputted for those significant clusters, saved in a folder named using the date of the last day of the week.

The weekly clusters from all SaTScan runs are outputted as a CSV file, as are the weekly clusters with p-values < 0.05.



3. ncdetect_influenza_data_make_maps.R

This script reads in the shapefiles for the significant weekly clusters, and generates a map for each week with one or more significant clusters.

Spatial data from tidycensus is used to get the latitude and longitude values of NC counties, in order to generate a basemap over which the clusters are plotted.

Each map is saved as a JPG file, named using the date of the last day of the week.

