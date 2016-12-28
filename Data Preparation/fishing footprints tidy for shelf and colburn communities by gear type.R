## load libraries, communities, gear types, and spatial templates

library(raster)
library(spatstat) 
library(maptools)
library(rgdal)
library(rgeos)
library(tidyverse)
library(stringr)
library(purrr)

communities <- read_csv("Z:/COCA-conf/Output/communities and ports meeting selection criteria.csv") %>%
  dplyr::select(STATE, PORT, PORT_CODE, JGS.COMMUNITY)

sst <- raster("Z:/COCA-conf/GIS/sst.template.tif")

filter <- readOGR(dsn = "Z:/COCA-conf/GIS", 
                  layer = "filter_vtr_1km_coastline_buffer",
                  verbose = FALSE)

gear.types <- read.csv("Z:/COCA-conf/Output/Aggregated_Gear_Codes_from_BK.csv") %>%
  drop_na(COST_ID) %>%
  dplyr::select(GEAR_CODE, COST_ID) %>%
  filter(!duplicated(GEAR_CODE))




## Load VTR data, filter years, join with community and gear data, and summarize

vtr.data <- readRDS("Z:/COCA-conf/VTR and permit/VTR_data_from_focal_ports.rds") %>%
  drop_na(CALC_LAT_DEG) %>%
  mutate(LAT_JGS = CALC_LAT_DEG + CALC_LAT_MIN/60, LON_JGS = -1*(CALC_LON_DEG + CALC_LON_MIN/60)) %>%
  filter(VTR_YEAR > 2010, VTR_YEAR <2016) %>%
  left_join(gear.types) %>%
  left_join(communities) %>%
  drop_na(COST_ID) %>%
  group_by(SUB_TRIP_ID, LAT_JGS, LON_JGS, JGS.COMMUNITY, COST_ID, VP_NUM) %>% # COST_ID is BRIAN KENNEDY'S GEAR TYPE CLASSIFICATION
  summarize(KEPT = sum(KEPT, na.rm = T)) %>%
  filter(LAT_JGS < 90) # filter outliers




## Filter locations that are within 1km of Natural Earth ocean and assign pixel IDs to locations

locations <- SpatialPoints(coords = vtr.data[, c("LON_JGS", "LAT_JGS")])
proj4string(locations) <- proj4string(sst)

ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same

vtr.data$ID <- ids$ID # assign raster cell number to lat lons
  
vtr.data <- drop_na(vtr.data, ID) # drop records outside filter




## Generate fishing footprints by community and gear type

vtr.data <- group_by(vtr.data, JGS.COMMUNITY, COST_ID) %>%
  nest()

MakeCommunityGearFootprints <- function(df) {
  
  footprint <- group_by(df, ID) %>%
    summarize(KEPT = sum(KEPT, na.rm = TRUE)) %>%
    drop_na()
  
  raster.kept <- sst
  raster.kept[] <- NA
  raster.kept[footprint$ID] <- footprint$KEPT
  
  total.kept <- sum(footprint$KEPT)
  footprint$PROPORTION_OF_TOTAL <- footprint$KEPT/total.kept
  
  raster.proportion <- sst
  raster.proportion[] <- NA
  raster.proportion[footprint$ID] <- footprint$PROPORTION_OF_TOTAL
  
  out <- list(footprint, raster.kept, raster.proportion)
  names(out) <- c("JGS.DATA", "JGS.KEPT", "JGS.PROPORTION")
  out
  
}

vtr.data <- vtr.data %>%
  mutate(JGS.COMMUNITY.GEAR.FOOTPRINTS = purrr::map(vtr.data$data, possibly(MakeCommunityGearFootprints, NA)))




## Make footprints that meet 3 unique vessel ID requirement for community and gear type

MakeNOAASafeCommunityGearFootprints <- function(df, list.column) {
  
  footprint.safe <- group_by(df, ID) %>%
    summarize(N.VESSELS = n_distinct(VP_NUM, na.rm = TRUE)) %>%
    drop_na() %>%
    filter(N.VESSELS > 2) %>%
    mutate(MEETS_CONFIDENTIALITY = 1)
  
  safe <- sst
  safe[] <- NA
  safe[footprint.safe$ID] <- footprint.safe$MEETS_CONFIDENTIALITY
  
  safe.raster.kept <- safe * list.column[[2]] 
  safe.raster.proportion <- safe * list.column[[3]] 
  
  out <- list(footprint.safe, safe.raster.kept, safe.raster.proportion)
  names(out) <- c("JGS.SAFE.DATA", "JGS.SAFE.KEPT", "JGS.SAFE.PROPORTION")
  out
  
}

vtr.data <- vtr.data %>%
  mutate(JGS.NOAA.SAFE.COMMUNITY.GEAR.FOOTPRINTS = purrr::map2(vtr.data$data, 
                                                               vtr.data$JGS.COMMUNITY.GEAR.FOOTPRINTS, 
                                                               possibly(MakeNOAASafeCommunityGearFootprints, NA)))

saveRDS(vtr.data, "Z:/COCA-conf/GIS/footprints/VTR fishing footprints by community and gear type 2011-2015.rds")





## Generate shelfwide fishing footprints by gear type

shelf.data <- dplyr::select(vtr.data, COST_ID:data) %>%
  unnest() %>%
  group_by(COST_ID) %>%
  nest()

shelf.data <- shelf.data %>%
  mutate(JGS.SHELF.GEAR.FOOTPRINTS = purrr::map(shelf.data$data, 
                                                possibly(MakeCommunityGearFootprints, NA)))

shelf.data <- shelf.data %>%
  mutate(JGS.NOAA.SAFE.SHELF.GEAR.FOOTPRINTS = purrr::map2(shelf.data$data,
                                                           shelf.data$JGS.SHELF.GEAR.FOOTPRINTS, 
                                                           possibly(MakeNOAASafeCommunityGearFootprints, NA)))

saveRDS(shelf.data, "Z:/COCA-conf/GIS/footprints/VTR fishing footprints shelfwide by gear type 2011-2015.rds")


