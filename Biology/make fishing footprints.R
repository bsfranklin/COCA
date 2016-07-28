<<<<<<< HEAD
load("I:/jschuetz/Documents/SESYNC/R/SESYNC.RData")


##### load libraries and spatial templates

library(raster)
library(spatstat) 
library(maptools)
library(rgdal)
library(rgeos)

sst <- raster("I:/jschuetz/Documents/SESYNC/GIS/sst.template.tif")

filter <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/GIS", 
                 layer = "filter_vtr_1km_coastline_buffer",
                 verbose = FALSE)
  
ocean <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/GIS", 
                 layer = "ocean",
                 verbose = FALSE)



##### approach for filtering locations that are within 1km of Natural Earth ocean and assigning ID

vtr.data$LAT_JGS <- vtr.data$CALC_LAT_DEG + vtr.data$CALC_LAT_MIN/60
vtr.data$LON_JGS <- -1*(vtr.data$CALC_LON_DEG + vtr.data$CALC_LON_MIN/60)

my.data <- vtr.data[vtr.data$SVSPP == 301 & vtr.data$STATE_CODE == 22,]
port.locs <- port.locs[port.locs$STATE == "ME", ]



##### summarize kept catch 

catch.locs.by.port <- aggregate(KEPT ~ SUB_TRIP_ID + LAT_JGS + LON_JGS + PORT_CODE + VTR_YEAR, my.data, sum)
catch.locs.by.port <- catch.locs.by.port[catch.locs.by.port$LAT_JGS<90,] # latitudes greater than 90 create error

locations <- SpatialPointsDataFrame(coords=catch.locs.by.port[, c(3,2)], data=catch.locs.by.port[,c(1,4,5,6)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons
footprints.all <- aggregate(KEPT ~ ID + PORT_CODE, locations, sum)




##### loop across ports and save kept catch results to a raster stack

kept.stack <- stack()

for (y in 1:length(port.locs$PORT_CODE)){
  
  footprint <- footprints.all[footprints.all$PORT_CODE == port.locs$PORT_CODE[y],]
  raster.kept <- sst
  raster.kept[] <- 0
  raster.kept[footprint$ID] <- footprint$KEPT
  kept.stack <- stack(kept.stack, raster.kept)
  
}

layer.names <- paste(port.locs$PORT_CODE, "_", port.locs$PORT_NAME, "_", port.locs$STATE, "_KEPT_CATCH", sep="")
names(kept.stack) <- layer.names

writeRaster(kept.stack, "I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_KEPT_CATCH_BY_MAINE_PORT.grd", overwrite=T)




#####

catch.locs.by.port <- NULL
catch.locs.by.port <- aggregate(KEPT ~ SUB_TRIP_ID + LAT_JGS + LON_JGS + PORT_CODE + VP_NUM + VTR_YEAR, my.data, sum)
catch.locs.by.port <- catch.locs.by.port[catch.locs.by.port$LAT_JGS<90,] # latitudes greater than 90 create error

locations <- SpatialPointsDataFrame(coords=catch.locs.by.port[, c(3,2)], data=catch.locs.by.port[,c(1,4:7)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons

vp.nums.all <- aggregate(SUB_TRIP_ID ~ ID + PORT_CODE + VP_NUM, locations, length) # sub trips by each vessel within pixel
trips.all <- aggregate(SUB_TRIP_ID ~ ID + PORT_CODE, vp.nums.all, sum) # total sub trips within pixel





trips.stack <- stack()

for (y in 1:length(port.locs$PORT_CODE)){
  
  trips <- trips.all[trips.all$PORT_CODE == port.locs$PORT_CODE[y],]
  raster.trips <- sst
  raster.trips[] <- 0
  raster.trips[trips$ID] <- trips$SUB_TRIP_ID
  trips.stack <- stack(trips.stack, raster.trips)
  
}

layer.names <- paste(port.locs$PORT_CODE, "_", port.locs$PORT, "_", port.locs$STATE, "TOTAL_SUB_TRIPS", sep="")
names(trips.stack) <- layer.names

writeRaster(trips.stack, "I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_TOTAL_SUB_TRIPS_BY_MAINE_PORT.grd", overwrite=T)



##### calculate lbs of kept catch per trip in each pixel

kept.per.trip<-kept.stack/trips.stack

writeRaster(kept.per.trip, "I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_CATCH_PER_TRIP_MAINE_PORTS.grd")


##### make plots

my.palette <- colorRampPalette(c("gray95","brown"))(n=100)     # Create color palette with 100 colors
plot(fall.2015.biomass[[1]], col=my.palette)
plot(ocean, add=T, border="gray50")




##### which cells contain maine ports?


locations <- SpatialPointsDataFrame(coords=port.locs[, c(5,6)], data=port.locs[,c(1:4)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons

cells <- aggregate(PORT_NAME ~ ID, locations, length) # sub trips by each vessel within pixel

port.cells <- sst
port.cells[] <- NA
port.cells[cells$ID] <- cells$PORT_NAME


##### calculate distance to cells containing maine ports

d <- as.vector(raster::distance(port.cells))

##### generate regression model to predict catch per trip using distance to port and predicted lobster biomass

y <- as.vector(kept.per.trip)
y[is.nan(y)] <- NA

bio <- raster("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.2015_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
l <- as.vector(bio[[1]])

my.data <- data.frame(y,d,l)

library(mgcv)
mod <- gam(y ~ d + l, family="gaussian", na.action=na.omit)


##### predict CPUE using d and future lobster forecast...this ends up being a sucky way to predict CPUE

bio <- raster("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.1C_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
l <- bio[[1]]
d <- distance(port.cells)

vs <- stack(l,d)
names(vs) <- c("l", "d")

ps <- predict(vs, mod, na.rm=T)

foot <- kept.per.trip*0
out<-ps+foot

=======
load("I:/jschuetz/Documents/SESYNC/R/SESYNC.RData")


##### load libraries and spatial templates

library(raster)
library(spatstat) 
library(maptools)
library(rgdal)
library(rgeos)

sst <- raster("I:/jschuetz/Documents/SESYNC/GIS/sst.template.tif")

filter <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/GIS", 
                 layer = "filter_vtr_1km_coastline_buffer",
                 verbose = FALSE)
  
ocean <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/GIS", 
                 layer = "ocean",
                 verbose = FALSE)



##### approach for filtering locations that are within 1km of Natural Earth ocean and assigning ID

vtr.data$LAT_JGS <- vtr.data$CALC_LAT_DEG + vtr.data$CALC_LAT_MIN/60
vtr.data$LON_JGS <- -1*(vtr.data$CALC_LON_DEG + vtr.data$CALC_LON_MIN/60)

my.data <- vtr.data[vtr.data$SVSPP == 301 & vtr.data$STATE_CODE == 22,]
port.locs <- port.locs[port.locs$STATE == "ME", ]



##### summarize kept catch 

catch.locs.by.port <- aggregate(KEPT ~ SUB_TRIP_ID + LAT_JGS + LON_JGS + PORT_CODE + VTR_YEAR, my.data, sum)
catch.locs.by.port <- catch.locs.by.port[catch.locs.by.port$LAT_JGS<90,] # latitudes greater than 90 create error

locations <- SpatialPointsDataFrame(coords=catch.locs.by.port[, c(3,2)], data=catch.locs.by.port[,c(1,4,5,6)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons
footprints.all <- aggregate(KEPT ~ ID + PORT_CODE, locations, sum)




##### loop across ports and save kept catch results to a raster stack

kept.stack <- stack()

for (y in 1:length(port.locs$PORT_CODE)){
  
  footprint <- footprints.all[footprints.all$PORT_CODE == port.locs$PORT_CODE[y],]
  raster.kept <- sst
  raster.kept[] <- 0
  raster.kept[footprint$ID] <- footprint$KEPT
  kept.stack <- stack(kept.stack, raster.kept)
  
}

layer.names <- paste(port.locs$PORT_CODE, "_", port.locs$PORT_NAME, "_", port.locs$STATE, "_KEPT_CATCH", sep="")
names(kept.stack) <- layer.names

writeRaster(kept.stack, "I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_KEPT_CATCH_BY_MAINE_PORT.grd", overwrite=T)




#####

catch.locs.by.port <- NULL
catch.locs.by.port <- aggregate(KEPT ~ SUB_TRIP_ID + LAT_JGS + LON_JGS + PORT_CODE + VP_NUM + VTR_YEAR, my.data, sum)
catch.locs.by.port <- catch.locs.by.port[catch.locs.by.port$LAT_JGS<90,] # latitudes greater than 90 create error

locations <- SpatialPointsDataFrame(coords=catch.locs.by.port[, c(3,2)], data=catch.locs.by.port[,c(1,4:7)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons

vp.nums.all <- aggregate(SUB_TRIP_ID ~ ID + PORT_CODE + VP_NUM, locations, length) # sub trips by each vessel within pixel
trips.all <- aggregate(SUB_TRIP_ID ~ ID + PORT_CODE, vp.nums.all, sum) # total sub trips within pixel





trips.stack <- stack()

for (y in 1:length(port.locs$PORT_CODE)){
  
  trips <- trips.all[trips.all$PORT_CODE == port.locs$PORT_CODE[y],]
  raster.trips <- sst
  raster.trips[] <- 0
  raster.trips[trips$ID] <- trips$SUB_TRIP_ID
  trips.stack <- stack(trips.stack, raster.trips)
  
}

layer.names <- paste(port.locs$PORT_CODE, "_", port.locs$PORT, "_", port.locs$STATE, "TOTAL_SUB_TRIPS", sep="")
names(trips.stack) <- layer.names

writeRaster(trips.stack, "I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_TOTAL_SUB_TRIPS_BY_MAINE_PORT.grd", overwrite=T)



##### calculate lbs of kept catch per trip in each pixel

kept.per.trip<-kept.stack/trips.stack

writeRaster(kept.per.trip, "I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_CATCH_PER_TRIP_MAINE_PORTS.grd")


##### make plots

my.palette <- colorRampPalette(c("gray95","brown"))(n=100)     # Create color palette with 100 colors
plot(fall.2015.biomass[[1]], col=my.palette)
plot(ocean, add=T, border="gray50")




##### which cells contain maine ports?


locations <- SpatialPointsDataFrame(coords=port.locs[, c(5,6)], data=port.locs[,c(1:4)])
proj4string(locations) <- proj4string(sst)
ids <- over(locations, filter)
ids <- ids + 1 # add 1 so that polygon feature ids and cell numbers in raster template are the same
locations@data$ID <- ids$ID # assign raster cell number to lat lons

cells <- aggregate(PORT_NAME ~ ID, locations, length) # sub trips by each vessel within pixel

port.cells <- sst
port.cells[] <- NA
port.cells[cells$ID] <- cells$PORT_NAME


##### calculate distance to cells containing maine ports

d <- as.vector(raster::distance(port.cells))

##### generate regression model to predict catch per trip using distance to port and predicted lobster biomass

y <- as.vector(kept.per.trip)
y[is.nan(y)] <- NA

bio <- raster("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.2015_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
l <- as.vector(bio[[1]])

my.data <- data.frame(y,d,l)

library(mgcv)
mod <- gam(y ~ d + l, family="gaussian", na.action=na.omit)


##### predict CPUE using d and future lobster forecast...this ends up being a sucky way to predict CPUE

bio <- raster("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.1C_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
l <- bio[[1]]
d <- distance(port.cells)

vs <- stack(l,d)
names(vs) <- c("l", "d")

ps <- predict(vs, mod, na.rm=T)

foot <- kept.per.trip*0
out<-ps+foot

>>>>>>> a1c716e04e48486c1a61175bccf99e223cac9f91
cellStats(out,sum)