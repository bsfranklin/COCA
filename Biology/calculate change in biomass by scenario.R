library(raster)
library(dismo)
library(spatstat) 
library(maptools)
library(rgdal)
library(rgeos)


##### load projected grid and generate areas for each polygon

eq.area.filter <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/GIS", 
                  layer = "proj_filter_vtr_1km_coastline_buffer",
                  verbose = FALSE)

areas <- gArea(eq.area.filter, byid=TRUE)/1000000 

##### load ocean for plotting

ocean <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/GIS", 
                 layer = "ocean",
                 verbose = FALSE)


##### load predictions/projections for multiple scenarios and address multiple biological assumptions

spring.2015.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.2015_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
spring.1C.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.1C_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
spring.2C.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.2C_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
spring.1C.no.dispersal <- min(spring.2015.prediction.stack[[1]], spring.1C.prediction.stack[[1]])
spring.2C.no.dispersal <- min(spring.2015.prediction.stack[[1]], spring.1C.prediction.stack[[1]], spring.2C.prediction.stack[[1]])

##### load fishing footprint for maine ports and mask a stack of scenarios

footprint <- sum(stack("I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_TOTAL_SUB_TRIPS_BY_MAINE_PORT.grd"))
footprint[footprint>0] <- 1
footprint[footprint==0] <- NA

scenarios <- stack(spring.2015.prediction.stack[[1]], 
                              spring.1C.prediction.stack[[1]],
                              spring.1C.no.dispersal,
                              spring.2C.prediction.stack[[1]],
                              spring.2C.no.dispersal)

scenarios.footprint <- scenarios * footprint

##### back-calculate predicted biomass values per tow

exp.scenarios.footprint <- as.vector(exp(scenarios.footprint[[1]]))

##### generate index of biomass that accounts for polygon areas

total <- exp.scenarios.footprint[eq.area.filter@data$ID]*areas
out <- scenarios.footprint[[1]]
out[eq.area.filter@data$ID] <- total
plot(out)

##### project results to equal area coordinate system

p <- "+proj=aea +lat_1=30 +lat_2=45 +lat_0=37.5 +lon_0=-71 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

eq.area.exp.scenarios <- projectRaster(exp.scenarios.footprint, crs=p)
eq.area.ocean <- spTransform(ocean, CRS(p))


##### sum biomass predictions/projections across fishing footprint and scale relative to

cellStats(exp.scenarios.footprint, sum)
cellStats(eq.area.exp.scenarios, sum)/cellStats(eq.area.exp.scenarios[[1]], sum)

plot(eq.area.e.scenarios[[1]])
plot(eq.area.ocean, add=T)







library(gbm)

load("I:/jschuetz/Documents/SESYNC/Output/models and evaluations/SVSPP_301_FALL_SPRING_LOG_BIOMASS_GBM")
gbm.plot(model, n.trees=model$n.trees)
summary(model)
print.gbm(model)
