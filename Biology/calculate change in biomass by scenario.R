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


##### build stack of scenarios describing biomass extrapolated over pixel areas

out.stack <- stack()

for (l in 1:5){
  
  ##### back-calculate predicted biomass values per tow

  exp.scenarios.footprint <- as.vector(exp(scenarios.footprint[[l]]))

  ##### generate index of biomass that accounts for polygon areas

  total <- exp.scenarios.footprint[eq.area.filter@data$ID]*areas
  out <- scenarios.footprint[[1]]
  out[eq.area.filter@data$ID] <- total
  out.stack <- stack(out.stack, out)
  
}


##### project results to equal area coordinate system

p <- "+proj=aea +lat_1=30 +lat_2=45 +lat_0=37.5 +lon_0=-71 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

eq.area.exp.scenarios <- projectRaster(out.stack, crs=p)
s.names <- c("2015", "+1C full dispersal", "+1C no dispersal", "+2C full dispersal", "+2C no dispersal")
names(eq.area.exp.scenarios) <- s.names
writeRaster(eq.area.exp.scenarios, "I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_spring_scenarios.grd", overwrite=T)


##### sum biomass predictions/projections across fishing footprint and scale relative to current

availability <- data.frame(cellStats(eq.area.exp.scenarios, sum)/cellStats(eq.area.exp.scenarios[[1]], sum))
for.gams <- cbind(availability, s.names)
names(for.gams) <- c("relative availability within ME", "scenario")
write.csv(for.gams, "C:/Users/jschuetz/Documents/Sandbox/COCA/SVSPP_301_spring_scenarios_for_GAMS.csv", row.names=F)




