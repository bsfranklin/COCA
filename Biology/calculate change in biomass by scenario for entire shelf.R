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


##### load predictions/projections for multiple scenarios and address multiple biological assumptions

spring.2015.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.2015_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
spring.0.5C.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.0.5C_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
spring.1C.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.1C_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
spring.2C.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/Output/predictions/spring.2C_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
spring.0.5C.no.dispersal <- min(spring.2015.prediction.stack[[1]], spring.0.5C.prediction.stack[[1]])
spring.1C.no.dispersal <- min(spring.0.5C.no.dispersal, spring.1C.prediction.stack[[1]])
spring.2C.no.dispersal <- min(spring.1C.no.dispersal, spring.2C.prediction.stack[[1]])


##### load stack of log(biomass + 1) predictions

scenarios <- stack(spring.2015.prediction.stack[[1]], 
                   spring.0.5C.prediction.stack[[1]],
                   spring.0.5C.no.dispersal,
                   spring.1C.prediction.stack[[1]],
                   spring.1C.no.dispersal,
                   spring.2C.prediction.stack[[1]],
                   spring.2C.no.dispersal)



##### build a raster stack describing back-transformed predictions of biomass per tow (kg/tow) 

kg.tow.stack <- (exp(scenarios)) - 1


##### build a raster stack describing area-weighted biomass ((kg/tow)*area)

kg.tow.area.stack <- stack()

for (i in 1:nlayers(scenarios)){
  
  ##### back-transform predictions to original biomass values (kg/tow)

  original <- as.vector(exp(scenarios[[i]])) - 1

  ##### generate index of biomass that accounts for polygon areas (kg/tow)*km2

  my.calc <- original[eq.area.filter@data$ID + 1] * areas
  kg.tow.area <- scenarios[[1]]*0
  kg.tow.area[eq.area.filter@data$ID + 1] <- my.calc
  kg.tow.area.stack <- stack(kg.tow.area.stack, kg.tow.area)
  
}


##### project results to equal area coordinate system

p <- "+proj=aea +lat_1=30 +lat_2=45 +lat_0=37.5 +lon_0=-71 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

eq.area.kg.tow.stack <- projectRaster(kg.tow.stack, crs=p)
s.names <- c("2015", "+0.5C full dispersal", "+0.5C no dispersal", "+1C full dispersal", "+1C no dispersal", "+2C full dispersal", "+2C no dispersal")
names(eq.area.kg.tow.stack) <- s.names
writeRaster(eq.area.kg.tow.stack, "I:/jschuetz/Documents/SESYNC/Output/NE_shelf_kg_per_tow_SVSPP_301_spring_scenarios.grd", overwrite=T)

eq.area.kg.tow.area.stack <- projectRaster(kg.tow.area.stack, crs=p)
names(eq.area.kg.tow.area.stack) <- s.names
writeRaster(eq.area.kg.tow.area.stack, "I:/jschuetz/Documents/SESYNC/Output/NE_shelf_biomass_index_SVSPP_301_spring_scenarios.grd", overwrite=T)


##### sum biomass predictions/projections across fishing footprint and scale relative to current biomass

rate <- data.frame(cellStats(eq.area.kg.tow.stack, mean)/cellStats(eq.area.kg.tow.stack[[1]], mean))
for.gams <- cbind(rate, s.names)
names(for.gams) <- c("relative catch per tow within NE shelf", "scenario")
write.csv(for.gams, "C:/Users/jschuetz/Documents/Sandbox/COCA/NE_shelf_kg_per_tow_SVSPP_301_spring_scenarios_for_GAMS.csv", row.names=F)


##### sum biomass predictions/projections across fishing footprint and scale relative to current biomass

available <- data.frame(cellStats(eq.area.kg.tow.area.stack, sum)/cellStats(eq.area.kg.tow.area.stack[[1]], sum))
for.gams <- cbind(available, s.names)
names(for.gams) <- c("relative biomass within NE shelf", "scenario")
write.csv(for.gams, "C:/Users/jschuetz/Documents/Sandbox/COCA/NE_shelf_biomass_SVSPP_301_spring_scenarios_for_GAMS.csv", row.names=F)


##### difference



