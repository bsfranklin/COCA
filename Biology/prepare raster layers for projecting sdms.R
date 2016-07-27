load("I:/jschuetz/Documents/SESYNC/ForJS/trawl.model.dat.adj.02102016.Rdata")

##### rename trawl data

trawl.data <- trawl.dat.full.reduced2
rm(trawl.dat.full.reduced2)




##### make ocean floor layers

library(raster)
library(spatstat) 
library(maptools)
library(rgdal)
library(rgeos)
library(sp)

sst <- raster("Z:/COCA-conf/GIS/sst.template.tif")
sst[] <- 1

sed <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/ForJS", 
                  layer = "TNC_benthicsediment",
                  verbose = FALSE)

sed.size <- rasterize(sed, sst, field="GRIDCODE", background=NA)
sed.type <- rasterize(sed, sst, field="GRPSED", background=NA)

bathy <- raster("I:/jschuetz/Documents/SESYNC/ForJS/NEShelf_Etopo1_bathy.tiff")
depth <- projectRaster(bathy, sst)

tri <- terrain(bathy, opt='TRI', unit='degrees', neighbors=8)
tri <- projectRaster(tri, sst)




##### calculate day of year in trawl data for establishing spring and fall prediction days

library(timeDate) # convert date fields to datetime fields

trawl.data$DATETIME<- strptime(trawl.data$DATE, format = "%Y-%m-%d")
trawl.data$DAY_OF_YEAR <- (trawl.data$DATETIME)$yday + 1
spring.day <- mean(trawl.data$DAY_OF_YEAR[trawl.data$SEASON=="SPRING"])
fall.day <- mean(trawl.data$DAY_OF_YEAR[trawl.data$SEASON=="FALL"])




##### make fall prediction stack

all.sst <- stack("Z:/Mills Lab/Climate Data/NOAA OISST/EC_sst_1981_2015_OISST-V2-AVHRR_agg_combined.nc")

fall.p <- grep("X2015.10.04", names(all.sst))

DAILYMU.OISST <- mean(all.sst[[fall.p]])
d30MU.OISST <- mean(all.sst[[(fall.p-29):fall.p]])
d180MU.OISST <- mean(all.sst[[(fall.p-179):fall.p]])
d365MU.OISST <- mean(all.sst[[(fall.p-364):fall.p]])
d1825MU.OISST <- mean(all.sst[[(fall.p-1824):fall.p]])
d3650MU.OISST <- mean(all.sst[[(fall.p-3649):fall.p]])

Ts <- raster::rotate(stack(DAILYMU.OISST, d30MU.OISST, d180MU.OISST, d365MU.OISST, d1825MU.OISST, d3650MU.OISST))
random <- Ts[[1]]*0
season <- random+1

fall.2015.prediction.stack <- stack(depth, tri, sed.size, sed.type, Ts, season, random)
names(fall.2015.prediction.stack) <- c("DEPTH", "TRI", "SED.SIZE", "SED.TYPE",
                                       "DAILYMU.OISST", "d30MU.OISST", "d180MU.OISST",
                                       "d365MU.OISST", "d1825MU.OISST", "d3650MU.OISST", "SEASON", "RANDOM")

writeRaster(fall.2015.prediction.stack, "I:/jschuetz/Documents/SESYNC/GIS/fall.2015.prediction.stack.grd", overwrite=T)



##### make spring prediction stack

spring.p <- grep("2015.03.31", names(all.sst))

DAILYMU.OISST <- mean(all.sst[[spring.p]])
d30MU.OISST <- mean(all.sst[[(spring.p-29):spring.p]])
d180MU.OISST <- mean(all.sst[[(spring.p-179):spring.p]])
d365MU.OISST <- mean(all.sst[[(spring.p-364):spring.p]])
d1825MU.OISST <- mean(all.sst[[(spring.p-1824):spring.p]])
d3650MU.OISST <- mean(all.sst[[(spring.p-3649):spring.p]])

Ts <- raster::rotate(stack(DAILYMU.OISST, d30MU.OISST, d180MU.OISST, d365MU.OISST, d1825MU.OISST, d3650MU.OISST))
season <- season+1

spring.2015.prediction.stack <- stack(depth, tri, sed.size, sed.type, Ts, season, random)
names(spring.2015.prediction.stack) <- c("DEPTH", "TRI", "SED.SIZE", "SED.TYPE",
                                         "DAILYMU.OISST", "d30MU.OISST", "d180MU.OISST",
                                         "d365MU.OISST", "d1825MU.OISST", "d3650MU.OISST", "SEASON", "RANDOM")

writeRaster(spring.2015.prediction.stack, "I:/jschuetz/Documents/SESYNC/GIS/spring.2015.prediction.stack.grd", overwrite=T)





##### make future prediction stacks by adding 2C to all T layers


fall.2c <- fall.2015.prediction.stack
fall.2c[[5]] <- fall.2c[[5]] + 2
fall.2c[[6]] <- fall.2c[[6]] + 2
fall.2c[[7]] <- fall.2c[[7]] + 2
fall.2c[[8]] <- fall.2c[[8]] + 2
fall.2c[[9]] <- fall.2c[[9]] + 2
fall.2c[[10]] <- fall.2c[[10]] + 2

writeRaster(fall.2c, "I:/jschuetz/Documents/SESYNC/GIS/fall.2C.prediction.stack.grd", overwrite=T)

spring.2c <- spring.2015.prediction.stack
spring.2c[[5]] <- spring.2c[[5]] + 2
spring.2c[[6]] <- spring.2c[[6]] + 2
spring.2c[[7]] <- spring.2c[[7]] + 2
spring.2c[[8]] <- spring.2c[[8]] + 2
spring.2c[[9]] <- spring.2c[[9]] + 2
spring.2c[[10]] <- spring.2c[[10]] + 2

writeRaster(spring.2c, "I:/jschuetz/Documents/SESYNC/GIS/spring.2C.prediction.stack.grd", overwrite=T)

