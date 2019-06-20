library(tidyverse)

#setwd("C:/Users/kelseyms/OneDrive - University of North Carolina at Chapel Hill/nc_detect_one_drive/Naloxone Geocoded Data")
setwd("C:/Users/joyceyan/University of North Carolina at Chapel Hill/Sumner, Kelsey Marie - nc_detect_one_drive/Naloxone Geocoded Data")

#study_length = 30 # use ymd
#study_length = 7 # use mdy

for (study_length in c(7, 30)){
  results_dir =  file.path(getwd(), "satscan", paste0(study_length, "_days")) 
  
  
  #### ---------- Plots and Tables for Days with High Cluster Counts ------------ #####
  
  sig_clusters = read.csv(file.path(results_dir, "sig_clusters.csv"))
  
  # are there particular days with high cluster counts?
  # highest count is 1 cluster on the same day
  sig_clusters %>% group_by(date) %>% summarize(Count = n()) %>% arrange(desc(Count))
  
  
  sig_clusters_plot = sig_clusters %>%
    mutate(date = lubridate::ymd(date)) %>%
    filter(date >= "2017-01-01" & date <= "2017-12-31") %>%
    group_by(date) %>%
    summarize(number_loc = sum(number_loc), radius = sum(radius))
  
  #are there particular days with large radii or large number of locations?
  top10_numloc = sig_clusters_plot %>%
    arrange(desc(number_loc)) %>% 
    select(date, number_loc) %>% 
    top_n(10)
  
  top10_radius =  sig_clusters_plot %>%
    arrange(desc(radius)) %>% 
    select(date, radius) %>% 
    top_n(10)
  
  sig_clusters_plot  %>%
    gather(key = "size_type", value = "size_value", -date) %>%
    ggplot(aes(x = date, y = size_value, text = date, color = size_type)) +
    geom_point() +
    theme_bw() +
    scale_x_date(labels = scales::date_format("%b"), breaks = seq.Date(min(sig_clusters_plot$date), max(sig_clusters_plot$date), by = "1 month")) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "", y = "", title = paste0("Significant Daily Clusters - ", study_length, " Day Baseline")) +
    scale_color_manual(name = "",  labels = c("Number of\nLocations", "Radius (km)"), values = c("cornflowerblue", "navyblue"))
  
  ggsave(file.path(results_dir, paste0("nalox_sig_clusters_plot_", study_length, "day.png")), width = 6, height = 4)
}