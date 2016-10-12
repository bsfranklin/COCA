
##### load libraries 

library(raster)
library(spatstat) 
library(maptools)
library(rgdal)
library(rgeos)
library(dplyr)


sst <- raster("Z:/COCA-conf/GIS/sst.template.tif")

sp.filter <- readOGR(dsn = "Z:/COCA-conf/GIS", 
                 layer = "filter_vtr_1km_coastline_buffer",
                 verbose = FALSE)
  
ocean <- readOGR(dsn = "Z:/COCA-conf/GIS", 
                 layer = "ocean",
                 verbose = FALSE)

gear <- read.csv("Z:/COCA-conf/Output/Aggregated_Gear_Codes_from_BK.csv") 

ports <- read.csv("Z:/COCA-conf/Output/ports sampled every year 1996-2015.csv")

##### read in vtr data, trim to base period (2011-2014), generate dec deg locs, join with Brian Kennedy's gear codes,
##### and summarise kept catch

vtr.data <- readRDS("Z:/COCA-conf/VTR and permit/VTR_data_from_focal_ports.rds") %>%
  filter(VTR_YEAR > 2010, VTR_YEAR < 2015, !is.na(CALC_LAT_DEG), TRIP_CATG == 1) %>%
  mutate(LAT_JGS = CALC_LAT_DEG + CALC_LAT_MIN/60, LON_JGS = -1*(CALC_LON_DEG + CALC_LON_MIN/60)) %>%
  left_join(gear) %>%
  group_by(SUB_TRIP_ID, LAT_JGS, LON_JGS, PORT_CODE, GEAR_KENNEDY, COST_ID) %>%
  summarise(KEPT_GEAR = sum(KEPT, na.rm = TRUE))



##### make into spatial points df

locations <- SpatialPointsDataFrame(coords=vtr.data[, c(3,2)], data=vtr.data[,c(1,4:7)])
proj4string(locations) <- proj4string(sst)



##### determine which cell each point falls within

ids <- over(locations, sp.filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons



##### summarize kept catch within each cell

footprints.abs <- filter(locations@data, !is.na(COST_ID)) %>%
  group_by(ID, PORT_CODE, GEAR_KENNEDY, COST_ID) %>%
  summarise(KEPT_IN_ID = sum(KEPT_GEAR, na.rm = TRUE)) 

footprints.prop <- group_by(footprints.abs, PORT_CODE, GEAR_KENNEDY) %>%
  summarise(KEPT_TOTAL = sum(KEPT_IN_ID, na.rm = TRUE)) %>%
  right_join(footprints.abs) %>%
  mutate(PROP_IN_ID = KEPT_IN_ID/KEPT_TOTAL)
  


##### loop across ports and gear types and save kept catch results to a raster stack

kept.stack <- stack()

for (g in 1:7){
  
  for (y in 1:nrow(ports)){
  
    footprint <- footprints.abs[(footprints.abs$PORT_CODE == ports$PORT_CODE[y]) & footprints.abs$COST_ID == g,]
    raster.kept <- sst
    raster.kept[] <- 0
    raster.kept[footprint$ID] <- footprint$KEPT_IN_ID
    kept.stack <- stack(kept.stack, raster.kept)
  
  }
  
}

layer.names <- paste(rep(paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_KEPT_CATCH_COST_ID_", sep=""), 7), 
                 paste(rep(1:7, each = nrow(ports), sep ="")), sep = "")

names(kept.stack) <- layer.names

writeRaster(kept.stack, "Z:/COCA-conf/GIS/footprints/KEPT_CATCH_BY_PORT_AND_GEAR_TYPE_2011-2014.grd", overwrite=T)



##### loop across ports and gear types and save proportion kept catch in each ID results to a raster stack

prop.stack <- stack()

for (g in 1:7){
  
  for (y in 1:nrow(ports)){
    
    footprint <- footprints.prop[(footprints.prop$PORT_CODE == ports$PORT_CODE[y]) & footprints.prop$COST_ID == g,]
    raster.prop <- sst
    raster.prop[] <- 0
    raster.prop[footprint$ID] <- footprint$PROP_IN_ID
    prop.stack <- stack(prop.stack, raster.prop)
    
  }
  
}

layer.names <- paste(rep(paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_PROPORTION_KEPT_CATCH_COST_ID_", sep=""), 7), 
                     paste(rep(1:7, each = nrow(ports), sep ="")), sep = "")

names(prop.stack) <- layer.names

writeRaster(prop.stack, "Z:/COCA-conf/GIS/footprints/PROPORTION_KEPT_CATCH_BY_PORT_AND_GEAR_TYPE_2011-2014.grd", overwrite=T)



##### example of how to pull data for particular port

i <- grep("PORTLAND_ME", names(prop.stack))
portland <- prop.stack[[i]]
plot(portland[[6]])
plot(ocean, add=T)
portland[portland == 0] <- NA




################################################
################################################

###### shelf wide summaries by gear type

vtr.data.shelf <- readRDS("Z:/COCA-conf/VTR and permit/VTR_data_from_focal_ports.rds") %>%
  filter(VTR_YEAR > 2010, VTR_YEAR < 2015, !is.na(CALC_LAT_DEG), TRIP_CATG == 1) %>%
  mutate(LAT_JGS = CALC_LAT_DEG + CALC_LAT_MIN/60, LON_JGS = -1*(CALC_LON_DEG + CALC_LON_MIN/60)) %>%
  left_join(gear) %>%
  group_by(SUB_TRIP_ID, LAT_JGS, LON_JGS, GEAR_KENNEDY, COST_ID) %>%
  summarise(KEPT_GEAR = sum(KEPT, na.rm = TRUE))



##### make into spatial points df

locations <- SpatialPointsDataFrame(coords=vtr.data.shelf[, c(3,2)], data=vtr.data.shelf[,c(1,4:6)])
proj4string(locations) <- proj4string(sst)



##### determine which cell each point falls within

ids <- over(locations, sp.filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons



##### summarize kept catch within each cell

footprints.abs <- filter(locations@data, !is.na(COST_ID)) %>%
  group_by(ID, GEAR_KENNEDY, COST_ID) %>%
  summarise(KEPT_IN_ID = sum(KEPT_GEAR, na.rm = TRUE)) 

footprints.prop <- group_by(footprints.abs, GEAR_KENNEDY) %>%
  summarise(KEPT_TOTAL = sum(KEPT_IN_ID, na.rm = TRUE)) %>%
  right_join(footprints.abs) %>%
  mutate(PROP_IN_ID = KEPT_IN_ID/KEPT_TOTAL)



##### loop across ports and gear types and save kept catch results to a raster stack

kept.stack <- stack()

for (g in 1:7){
  
    footprint <- footprints.abs[footprints.abs$COST_ID == g,]
    raster.kept <- sst
    raster.kept[] <- 0
    raster.kept[footprint$ID] <- footprint$KEPT_IN_ID
    kept.stack <- stack(kept.stack, raster.kept)
  
}

layer.names <- paste(rep(paste("SHELFWIDE_KEPT_CATCH_COST_ID_", sep=""), 7), seq(1, 7), sep="")

names(kept.stack) <- layer.names

writeRaster(kept.stack, "Z:/COCA-conf/GIS/footprints/KEPT_CATCH_SHELFWIDE_BY_GEAR_TYPE_2011-2014.grd", overwrite=T)



##### loop across ports and gear types and save proportion kept catch in each ID results to a raster stack

prop.stack <- stack()

for (g in 1:7){
  
    footprint <- footprints.prop[footprints.prop$COST_ID == g,]
    raster.prop <- sst
    raster.prop[] <- 0
    raster.prop[footprint$ID] <- footprint$PROP_IN_ID
    prop.stack <- stack(prop.stack, raster.prop)
  
}

layer.names <- paste(rep(paste("SHELFWIDE_PROPORTION_KEPT_CATCH_COST_ID_", sep=""), 7), seq(1, 7), sep="")

names(prop.stack) <- layer.names

writeRaster(prop.stack, "Z:/COCA-conf/GIS/footprints/PROPORTION_KEPT_CATCH_SHELFWIDE_BY_GEAR_TYPE_2011-2014.grd", overwrite=T)


#####



############################################################################
############################################################################



##### make layers that are safe for visuals (at least 3 VP_NUMs represented in each cell)


##### read in vtr data, trim to base period (2011-2014), generate dec deg locs, join with Brian Kennedy's gear codes,
##### and summarise kept catch

vtr.data <- readRDS("Z:/COCA-conf/VTR and permit/VTR_data_from_focal_ports.rds") %>%
  filter(VTR_YEAR > 2010, VTR_YEAR < 2015, !is.na(CALC_LAT_DEG), TRIP_CATG == 1) %>%
  mutate(LAT_JGS = CALC_LAT_DEG + CALC_LAT_MIN/60, LON_JGS = -1*(CALC_LON_DEG + CALC_LON_MIN/60)) %>%
  left_join(gear) %>%
  group_by(VP_NUM, LAT_JGS, LON_JGS, PORT_CODE, GEAR_KENNEDY, COST_ID) %>%
  summarise(KEPT_GEAR = sum(KEPT, na.rm = TRUE))



##### make into spatial points df

locations <- SpatialPointsDataFrame(coords=vtr.data[, c(3,2)], data=vtr.data[,c(1,4:7)])
proj4string(locations) <- proj4string(sst)



##### determine which cell each point falls within

ids <- over(locations, sp.filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons



##### summarize kept catch within each cell

footprints.abs <- filter(locations@data, !is.na(COST_ID)) %>%
  group_by(ID, PORT_CODE, GEAR_KENNEDY, COST_ID) %>%
  summarise(N_VESSELS = n(), KEPT_IN_ID = sum(KEPT_GEAR, na.rm = TRUE)) 

footprints.prop <- group_by(footprints.abs, PORT_CODE, GEAR_KENNEDY, COST_ID) %>%
  summarise(KEPT_TOTAL = sum(KEPT_IN_ID, na.rm = TRUE)) %>%
  right_join(footprints.abs) %>%
  mutate(PROP_IN_ID = KEPT_IN_ID/KEPT_TOTAL)



##### loop across ports and gear types and save kept catch results to a raster stack

kept.stack <- stack()

for (g in 1:7){
  
  for (y in 1:nrow(ports)){
    
    footprint <- filter(footprints.abs, PORT_CODE == ports$PORT_CODE[y], COST_ID == g, N_VESSELS > 2)
    raster.kept <- sst
    raster.kept[] <- 0
    raster.kept[footprint$ID] <- footprint$KEPT_IN_ID
    kept.stack <- stack(kept.stack, raster.kept)
    
  }
  
}

layer.names <- paste(rep(paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_NOAA_PUBLIC_SAFE_KEPT_CATCH_COST_ID_", sep=""), 7), 
                     paste(rep(1:7, each = nrow(ports), sep ="")), sep = "")

names(kept.stack) <- layer.names

writeRaster(kept.stack, "Z:/COCA-conf/GIS/footprints/NOAA_PUBLIC_SAFE_KEPT_CATCH_BY_PORT_AND_GEAR_TYPE_2011-2014.grd", overwrite=T)



##### loop across ports and gear types and save proportion kept catch in each ID results to a raster stack

prop.stack <- stack()

for (g in 1:7){
  
  for (y in 1:nrow(ports)){
    
    footprint <- filter(footprints.prop, PORT_CODE == ports$PORT_CODE[y], COST_ID == g, N_VESSELS > 2)
    raster.prop <- sst
    raster.prop[] <- 0
    raster.prop[footprint$ID] <- footprint$PROP_IN_ID
    prop.stack <- stack(prop.stack, raster.prop)
    
  }
  
}

layer.names <- paste(rep(paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_NOAA_PUBLIC_SAFE_PROPORTION_KEPT_CATCH_COST_ID_", sep=""), 7), 
                     paste(rep(1:7, each = nrow(ports), sep ="")), sep = "")

names(prop.stack) <- layer.names

writeRaster(prop.stack, "Z:/COCA-conf/GIS/footprints/NOAA_PUBLIC_SAFE_PROPORTION_KEPT_CATCH_BY_PORT_AND_GEAR_TYPE_2011-2014.grd", overwrite=T)



################################################
################################################

###### shelf wide summaries by gear type trimmed to cells with 3 or more vessels

vtr.data.recs <- readRDS("Z:/COCA-conf/VTR and permit/VTR_data_from_focal_ports.rds") %>%
  filter(VTR_YEAR > 2010, VTR_YEAR < 2015, !is.na(CALC_LAT_DEG), TRIP_CATG == 1) %>%
  mutate(LAT_JGS = CALC_LAT_DEG + CALC_LAT_MIN/60, LON_JGS = -1*(CALC_LON_DEG + CALC_LON_MIN/60)) %>%
  left_join(gear) %>%
  group_by(VP_NUM, LAT_JGS, LON_JGS, GEAR_KENNEDY, COST_ID) %>%
  summarise(KEPT_GEAR = sum(KEPT, na.rm = TRUE))



##### make into spatial points df

locations <- SpatialPointsDataFrame(coords=vtr.data.recs[, c(3,2)], data=vtr.data.recs[,c(1,4:6)])
proj4string(locations) <- proj4string(sst)



##### determine which cell each point falls within

ids <- over(locations, sp.filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons



##### summarize kept catch within each cell

footprints.abs <- filter(locations@data, !is.na(COST_ID)) %>%
  group_by(ID, GEAR_KENNEDY, COST_ID) %>%
  summarise(N_VESSELS = n(), KEPT_IN_ID = sum(KEPT_GEAR, na.rm = TRUE)) 

footprints.prop <- group_by(footprints.abs, GEAR_KENNEDY, COST_ID) %>%
  summarise(KEPT_TOTAL = sum(KEPT_IN_ID, na.rm = TRUE)) %>%
  right_join(footprints.abs) %>%
  mutate(PROP_IN_ID = KEPT_IN_ID/KEPT_TOTAL)



##### trim data set to cells that have at least 3 vessels
##### loop across ports and gear types and save kept catch results to a raster stack

kept.stack <- stack()

for (g in 1:7){
  
  footprint <- filter(footprints.abs, COST_ID == g, N_VESSELS > 2)
  raster.kept <- sst
  raster.kept[] <- 0
  raster.kept[footprint$ID] <- footprint$KEPT_IN_ID
  kept.stack <- stack(kept.stack, raster.kept)
  
}

layer.names <- paste(rep(paste("NOAA_PUBLIC_SAFE_SHELFWIDE_KEPT_CATCH_COST_ID_", sep=""), 7), seq(1, 7), sep="")

names(kept.stack) <- layer.names

writeRaster(kept.stack, "Z:/COCA-conf/GIS/footprints/NOAA_PUBLIC_SAFE_KEPT_CATCH_SHELFWIDE_BY_GEAR_TYPE_2011-2014.grd", overwrite=T)



##### loop across ports and gear types and save proportion kept catch in each ID results to a raster stack

prop.stack <- stack()

for (g in 1:7){
  
  footprint <- filter(footprints.prop, COST_ID == g, N_VESSELS > 2)
  raster.prop <- sst
  raster.prop[] <- 0
  raster.prop[footprint$ID] <- footprint$PROP_IN_ID
  prop.stack <- stack(prop.stack, raster.prop)
  
}

layer.names <- paste(rep(paste("NOAA_PUBLIC_SAFE_SHELFWIDE_PROPORTION_KEPT_CATCH_COST_ID_", sep=""), 7), seq(1, 7), sep="")

names(prop.stack) <- layer.names

writeRaster(prop.stack, "Z:/COCA-conf/GIS/footprints/NOAA_PUBLIC_SAFE_PROPORTION_KEPT_CATCH_SHELFWIDE_BY_GEAR_TYPE_2011-2014.grd", overwrite=T)


