
##### load libraries and spatial templates

library(raster)
library(spatstat) 
library(maptools)
library(rgdal)
library(rgeos)
library(tidyverse)
library(stringr)

ports <- read.csv("Z:/COCA-conf/Output/communities and ports meeting selection criteria.csv")

sst <- raster("Z:/COCA-conf/GIS/sst.template.tif")

filter <- readOGR(dsn = "Z:/COCA-conf/GIS", 
                 layer = "filter_vtr_1km_coastline_buffer",
                 verbose = FALSE)
  
ocean <- readOGR(dsn = "Z:/COCA-conf/GIS", 
                 layer = "ocean",
                 verbose = FALSE)

closed_areas <- readOGR(dsn = "Z:/COCA-conf/GIS/NMFS_closed_areas",
                        layer = "closed_areas",
                        verbose = FALSE)

gear.types <- read.csv("Z:/COCA-conf/Output/Aggregated_Gear_Codes_from_BK.csv") %>%
  drop_na(COST_ID) %>%
  dplyr::select(GEAR_CODE, COST_ID) %>%
  filter(!duplicated(GEAR_CODE))
  

##### approach for filtering locations that are within 1km of Natural Earth ocean and assigning ID

catch.locs.by.port <- NULL

vtr.file.endings <- c("2011 VTR", "2012 VTR", "2013 VTR", "2014-2015_VTR_Trips_UPD_AUG_2016")

for (d in 1:4){
  
  vtr.data <- read.csv(paste("Z:/COCA-conf/VTR and permit/Mills_", vtr.file.endings[d], ".csv", sep=""))
  
  my.ports <- match(vtr.data$PORT_CODE, ports$PORT_CODE)
  my.data <- vtr.data[!is.na(my.ports),]
  my.data$LAT_JGS <- my.data$CALC_LAT_DEG + my.data$CALC_LAT_MIN/60
  my.data$LON_JGS <- -1*(my.data$CALC_LON_DEG + my.data$CALC_LON_MIN/60)
  
  temp <- aggregate(KEPT ~ SUB_TRIP_ID + LAT_JGS + LON_JGS + PORT_CODE + VTR_YEAR + GEAR_CODE, my.data, sum)
  catch.locs.by.port <- rbind(catch.locs.by.port, temp)
  
}

catch.locs.by.port <- catch.locs.by.port[catch.locs.by.port$LAT_JGS < 90,] # latitudes greater than 90 create error
locations <- SpatialPointsDataFrame(coords = catch.locs.by.port[, c(3,2)], data = catch.locs.by.port[, c(1,4:7)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons
locations@data <- left_join(locations@data, gear.types)

footprints.all <- aggregate(KEPT ~ ID + PORT_CODE + COST_ID, locations, sum)


##### loop across ports and gear types and save kept catch results to a raster stack

kept.stack <- stack()

for (y in 1:length(ports$PORT_CODE)){
  
  for (g in 1:7){ # there are 7 general gear types
    
    footprint <- filter(footprints.all, PORT_CODE == ports$PORT_CODE[y],
                        COST_ID == g)
    raster.kept <- sst
    raster.kept[] <- NA
    raster.kept[footprint$ID] <- footprint$KEPT
    kept.stack <- stack(kept.stack, raster.kept)
  
  }
  
}

layer.names <- paste(rep(str_c(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_GEAR_TYPE_"), each = 7), seq(1,7), sep="")
names(kept.stack) <- layer.names
writeRaster(kept.stack, "Z:/COCA-conf/GIS/footprints/KEPT_CATCH_BY_PORT_AND_GEAR_TYPE_2011-2015.grd", overwrite=T)



##### loop across ports and save proportion results to a raster stack

proportion.stack <- stack()

for (y in 1:length(ports$PORT_CODE)){
  
  for (g in 1:7){ # there are 7 general gear types
    
    footprint <- filter(footprints.all, PORT_CODE == ports$PORT_CODE[y],
                        COST_ID == g)
    
    total.kept <- sum(footprint$KEPT)   
  
    footprint$PROPORTION_OF_TOTAL <- footprint$KEPT/total.kept
    raster.proportion <- sst
    raster.proportion[] <- NA
    raster.proportion[footprint$ID] <- footprint$PROPORTION_OF_TOTAL
    proportion.stack <- stack(proportion.stack, raster.proportion)
  
  }
  
}

layer.names <- paste(rep(str_c(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_GEAR_TYPE_"), each = 7), seq(1,7), sep="")
names(proportion.stack) <- layer.names
writeRaster(proportion.stack, "Z:/COCA-conf/GIS/footprints/PROPORTION_KEPT_CATCH_BY_PORT_AND_GEAR_TYPE_2011-2015.grd", overwrite=T)



##### generate shelfwide footprints by gear type

shelf.kept <- stack()

for (g in 1:7){
  
  gear.stack <- kept.stack[[seq(g, 7 * nrow(ports), 7)]]
  gear.total <- sum(gear.stack, na.rm = T)
  shelf.kept <- stack(shelf.kept, gear.total)
  
}

layer.names <- paste(rep(str_c("SHELFWIDE_GEAR_TYPE_"), each = 7), seq(1,7), sep="")
names(shelf.kept) <- layer.names
writeRaster(shelf.kept, "Z:/COCA-conf/GIS/footprints/KEPT_CATCH_SHELFWIDE_BY_GEAR_TYPE_2011-2015.grd", overwrite=T)



#### generate shelfwide proportion catch footprints

shelf.prop <- stack()

for (g in 1:7){
  
  gear.stack <- kept.stack[[seq(g, 7 * nrow(ports), 7)]]
  gear.total <- sum(gear.stack, na.rm = T)
  layer.total <- cellStats(gear.total, stat = "sum", na.rm = T)
  gear.prop <- gear.total/layer.total
  shelf.prop <- stack(shelf.prop, gear.prop)
  
}

layer.names <- paste(rep(str_c("SHELFWIDE_GEAR_TYPE_"), each = 7), seq(1,7), sep="")
names(shelf.prop) <- layer.names
writeRaster(shelf.prop, "Z:/COCA-conf/GIS/footprints/PROPORTION_KEPT_CATCH_SHELFWIDE_BY_GEAR_TYPE_2011-2015.grd", overwrite=T)






##### make footprints that meet 3 unique vessel ID requirement for port and gear type


catch.locs.by.port <- NULL

vtr.file.endings <- c("2011 VTR", "2012 VTR", "2013 VTR", "2014-2015_VTR_Trips_UPD_AUG_2016")

for (d in 1:4){
  
  vtr.data <- read.csv(paste("Z:/COCA-conf/VTR and permit/Mills_", vtr.file.endings[d], ".csv", sep=""))
  
  my.ports <- match(vtr.data$PORT_CODE, ports$PORT_CODE)
  my.data <- vtr.data[!is.na(my.ports),]
  my.data$LAT_JGS <- my.data$CALC_LAT_DEG + my.data$CALC_LAT_MIN/60
  my.data$LON_JGS <- -1*(my.data$CALC_LON_DEG + my.data$CALC_LON_MIN/60)
  
  temp <- aggregate(KEPT ~ SUB_TRIP_ID + LAT_JGS + LON_JGS + PORT_CODE + VP_NUM + GEAR_CODE, my.data, sum)
  catch.locs.by.port <- rbind(catch.locs.by.port, temp)
  
}

catch.locs.by.port <- catch.locs.by.port[catch.locs.by.port$LAT_JGS < 90,] # latitudes greater than 90 create error
locations <- SpatialPointsDataFrame(coords = catch.locs.by.port[, c(3,2)], data = catch.locs.by.port[, c(1,4:7)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons
locations@data <- left_join(locations@data, gear.types)

vp.nums.all <- group_by(locations@data, COST_ID, ID, PORT_CODE) %>%
  summarize(N_VP_NUMS_IN_PIXEL = n_distinct(VP_NUM)) %>%
  filter(N_VP_NUMS_IN_PIXEL > 2) %>%
  mutate(MEETS_CONFIDENTIALITY = 1)

vp.num.stack <- stack()

for (y in 1:length(ports$PORT_CODE)){
  
  for (g in 1:7){
    
    vp.nums <- filter(vp.nums.all, PORT_CODE == ports$PORT_CODE[y], COST_ID == g)
    raster.vp.nums <- sst
    raster.vp.nums[] <- NA
    raster.vp.nums[vp.nums$ID] <- vp.nums$MEETS_CONFIDENTIALITY
    vp.num.stack <- stack(vp.num.stack, raster.vp.nums)
  
  }
  
}

kept.catch.safe <- stack(vp.num.stack * kept.stack)
layer.names <- paste(rep(str_c("NOAA_SAFE_", ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_GEAR_TYPE_"), each = 7), seq(1,7), sep="")
names(kept.catch.safe) <- layer.names
writeRaster(kept.catch.safe, "Z:/COCA-conf/GIS/footprints/NOAA_SAFE_KEPT_CATCH_BY_PORT_AND_GEAR_TYPE_2011-2015.grd", overwrite=T)

proportion.catch.safe <- stack(vp.num.stack * proportion.stack)
layer.names <- paste(rep(str_c("NOAA_SAFE_", ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_GEAR_TYPE_"), each = 7), seq(1,7), sep="")
names(proportion.catch.safe) <- layer.names
writeRaster(proportion.catch.safe, "Z:/COCA-conf/GIS/footprints/NOAA_SAFE_PROPORTION_KEPT_CATCH_BY_PORT_AND_GEAR_TYPE_2011-2015.grd", overwrite=T)




##### make footprints that meet 3 unique vessel ID requirement shelfwide by gear type

vp.nums.shelf <- group_by(locations@data, COST_ID, ID) %>%
  summarize(N_VP_NUMS_IN_PIXEL = n_distinct(VP_NUM)) %>%
  filter(N_VP_NUMS_IN_PIXEL > 2) %>%
  mutate(MEETS_CONFIDENTIALITY = 1)

vp.num.shelf.stack <- stack()

for (g in 1:7){
    
    vp.nums <- filter(vp.nums.shelf, COST_ID == g)
    raster.vp.nums <- sst
    raster.vp.nums[] <- NA
    raster.vp.nums[vp.nums$ID] <- vp.nums$MEETS_CONFIDENTIALITY
    vp.num.shelf.stack <- stack(vp.num.shelf.stack, raster.vp.nums)
  
}

kept.shelf.safe <- stack(vp.num.shelf.stack * shelf.kept)
layer.names <- paste(rep(str_c("NOAA_SAFE_SHELFWIDE_GEAR_TYPE_"), each = 7), seq(1,7), sep="")
names(kept.shelf.safe) <- layer.names
writeRaster(kept.shelf.safe, "Z:/COCA-conf/GIS/footprints/NOAA_SAFE_KEPT_CATCH_SHELFWIDE_BY_GEAR_TYPE_2011-2015.grd", overwrite=T)

proportion.shelf.safe <- stack(vp.num.shelf.stack * shelf.prop)
layer.names <- paste(rep(str_c("NOAA_SAFE_SHELFWIDE_GEAR_TYPE_"), each = 7), seq(1,7), sep="")
names(proportion.shelf.safe) <- layer.names
writeRaster(proportion.shelf.safe, "Z:/COCA-conf/GIS/footprints/NOAA_SAFE_PROPORTION_KEPT_CATCH_SHELFWIDE_BY_GEAR_TYPE_2011-2015.grd", overwrite=T)

















###################################################################################################

##### EVERYTHING BELOW HERE IS JUNK...BUT IT MIGHT GET YOU STARTED ON SOME OTHER INTERESTING THINGS

###################################################################################################


trips.stack <- stack()

for (y in 1:length(ports$PORT_CODE)){
  
  trips <- trips.all[trips.all$PORT_CODE == ports$PORT_CODE[y],]
  raster.trips <- sst
  raster.trips[] <- NA
  raster.trips[trips$ID] <- trips$SUB_TRIP_ID
  trips.stack <- stack(trips.stack, raster.trips)
  
}

layer.names <- paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "TOTAL_SUB_TRIPS", sep="")
names(trips.stack) <- layer.names

writeRaster(trips.stack, "Z:/COCA-conf/GIS/TOTAL_SUB_TRIPS_BY_PORT.grd", overwrite=T)


#####

vhp.stack <- stack()

for (y in 1:length(ports$PORT_CODE)){
  
  vhp <- vhps.all[vhps.all$PORT_CODE == ports$PORT_CODE[y],]
  raster.vhps <- sst
  raster.vhps[] <- NA
  raster.vhps[vhp$ID] <- vhp$VHP
  vhp.stack <- stack(vhp.stack, raster.vhps)
  
}

layer.names <- paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_MEAN_VHP_BY_SUB_TRIP_ID", sep="")
names(vhp.stack) <- layer.names

writeRaster(vhp.stack, "Z:/COCA-conf/GIS/MEAN_VHP_BY_SUB_TRIP_ID_AND_PORT.grd", overwrite=T)


#####

gtons.stack <- stack()

for (y in 1:length(ports$PORT_CODE)){
  
  gtons <- gtons.all[gtons.all$PORT_CODE == ports$PORT_CODE[y],]
  raster.gtons <- sst
  raster.gtons[] <- NA
  raster.gtons[gtons$ID] <- gtons$GTON
  gtons.stack <- stack(gtons.stack, raster.gtons)
  
}

layer.names <- paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_MEAN_GTONS_BY_SUB_TRIP_ID", sep="")
names(gtons.stack) <- layer.names

writeRaster(gtons.stack, "Z:/COCA-conf/GIS/MEAN_GTONS_BY_SUB_TRIP_ID_AND_PORT.grd", overwrite=T)



##### summarize # species taken per pixel


catch.locs.by.port <- NULL

for (d in 1996:2015){
  
  vtr.data <- read.csv(paste("Mills_", d, " VTR.csv", sep=""))
  
  my.ports <- match(vtr.data$PORT_CODE, ports$PORT_CODE)
  my.data <- vtr.data[!is.na(my.ports),]
  my.data$LAT_JGS <- my.data$CALC_LAT_DEG + my.data$CALC_LAT_MIN/60
  my.data$LON_JGS <- -1*(my.data$CALC_LON_DEG + my.data$CALC_LON_MIN/60)
  
  temp <- aggregate(KEPT ~ SUB_TRIP_ID + LAT_JGS + LON_JGS + PORT_CODE + SVSPP + VTR_YEAR, my.data, sum)
  catch.locs.by.port <- rbind(catch.locs.by.port, temp)
  
}

catch.locs.by.port <- catch.locs.by.port[catch.locs.by.port$LAT_JGS<90,] # latitudes greater than 90 create error
locations <- SpatialPointsDataFrame(coords=catch.locs.by.port[, c(3,2)], data=catch.locs.by.port[,c(1,4:7)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons

spp.list <- aggregate(KEPT ~ ID + PORT_CODE + SVSPP, locations, length) #number of sub trips in each pixel where a spp was caught
spp.all <- aggregate(SVSPP ~ ID + PORT_CODE, spp.list, length) # number of species caught in each pixel


#####

spp.stack <- stack()

for (y in 1:length(ports$PORT_CODE)){
  
  spp <- spp.all[spp.all$PORT_CODE == ports$PORT_CODE[y],]
  raster.spp <- sst
  raster.spp[] <- NA
  raster.spp[spp$ID] <- spp$SVSPP
  spp.stack <- stack(spp.stack, raster.spp)
  
}

layer.names <- paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_KEPT_CATCH_SPECIES_RICHNESS", sep="")
names(spp.stack) <- layer.names

writeRaster(spp.stack, "Z:/COCA-conf/GIS/KEPT_CATCH_SPECIES_RICHNESS_BY_PORT.grd", overwrite=T)



##### summarize duration of trips for catch in each pixel

catch.locs.by.port <- NULL

for (d in 1996:2015){
  
  vtr.data <- read.csv(paste("Mills_", d, " VTR.csv", sep=""))
  
  my.ports <- match(vtr.data$PORT_CODE, ports$PORT_CODE)
  my.data <- vtr.data[!is.na(my.ports),]
  my.data$LAT_JGS <- my.data$CALC_LAT_DEG + my.data$CALC_LAT_MIN/60
  my.data$LON_JGS <- -1*(my.data$CALC_LON_DEG + my.data$CALC_LON_MIN/60)
  
  library(timeDate) # convert date fields to datetime fields
  
  my.data$DATETIME_SAIL<- strptime(my.data$DATE_SAIL, format = "%m/%d/%Y")
  my.data$DATETIME_LAND<- strptime(my.data$DATE_LAND, format = "%m/%d/%Y")
  my.data$TRIP_DAYS<- (as.integer(my.data$DATETIME_LAND - my.data$DATETIME_SAIL)/86400) + 1
  my.data$DAY_OF_YEAR <- (my.data$DATETIME_SAIL)$yday + 1
  
  temp <- aggregate(TRIP_DAYS ~ SUB_TRIP_ID + LAT_JGS + LON_JGS + PORT_CODE + VTR_YEAR, my.data, min)
  catch.locs.by.port <- rbind(catch.locs.by.port, temp)
  
}

catch.locs.by.port <- catch.locs.by.port[catch.locs.by.port$LAT_JGS<90,] # latitudes greater than 90 create error
locations <- SpatialPointsDataFrame(coords=catch.locs.by.port[, c(3,2)], data=catch.locs.by.port[,c(1,4:6)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons
#locations

dur.all <- aggregate(TRIP_DAYS ~ ID + PORT_CODE, locations, mean) # mean TRIP_DAYS for all sub trips within pixel

dur.stack <- stack()

for (y in 1:length(ports$PORT_CODE)){
  
  dur <- dur.all[dur.all$PORT_CODE == ports$PORT_CODE[y],]
  raster.dur <- sst
  raster.dur[] <- NA
  raster.dur[dur$ID] <- dur$TRIP_DAYS
  dur.stack <- stack(dur.stack, raster.spp)
  
}

layer.names <- paste(ports$PORT_CODE, "_", ports$PORT, "_", ports$STATE, "_MEAN_TRIP_DAYS_BY_SUB_TRIP_ID", sep="")
names(dur.stack) <- layer.names

writeRaster(dur.stack, "Z:/COCA-conf/GIS/MEAN_TRIP_DAYS_BY_SUB_TRIP_ID_AND_PORT.grd", overwrite=T)










