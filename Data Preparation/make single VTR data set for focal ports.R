
library(readr)
library(tidyr)
library(dplyr)

##### read in ports that are consistently sampled in VTR data

ports <- read_csv("Z:/COCA-conf/Output/ports sampled every year 1996-2015.csv")


##### generate list of VTR files in folder and remove the folder with incomplete files

my.files <- list.files("Z:/COCA-conf/VTR and permit", pattern = ".csv")


##### read in separate VTR data files and filter data to keep only records from ports sampled every year

all.vtr <- NULL

for (d in 1:length(my.files)){
  
  vtr.data <- read.csv(paste("Z:/COCA-conf/VTR and permit/", my.files[d], sep = "")) %>%
    filter(PORT_CODE %in% ports$PORT_CODE)
  
  all.vtr <- rbind(all.vtr, vtr.data)
  
}

saveRDS(all.vtr, file = "Z:/COCA-conf/VTR and permit/VTR_data_from_focal_ports.rds")
