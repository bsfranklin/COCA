##### make ocean and ocean floor layers 


##### load libraries

library(raster)
library(spatstat) 
library(maptools)
library(rgdal)
library(rgeos)
library(sp)


##### load OISST template

sst <- raster("Z:/COCA-conf/GIS/sst.template.tif")
sst[] <- 1


##### load TNC layer describing benthic sediments

sed <- readOGR(dsn = "Z:/COCA-conf/GIS", 
                  layer = "TNC_benthicsediment",
                  verbose = FALSE)


##### rasterize TNC layer based on two sediment attributes

sed.size <- rasterize(sed, sst, field="GRIDCODE", background=NA)
sed.type <- rasterize(sed, sst, field="GRPSED", background=NA)


##### load ETOPO topobathymetry layer and reproject

bathy <- raster("Z:/COCA-conf/GIS/NEShelf_Etopo1_bathy.tiff")
depth <- projectRaster(bathy, sst)


##### calculate terrain roughness index (TRI) and reproject

tri <- terrain(bathy, opt='TRI', unit='degrees', neighbors=8)
tri <- projectRaster(tri, sst)






##### calculate day of year in trawl data for establishing spring and fall prediction days

library(timeDate) 


##### convert date fields to datetime fields

load("Z:/COCA-conf/NEFSC trawl/trawl_data_for_models_AA_02102016.RData")

trawl.data$DATETIME<- strptime(trawl.data$DATE, format = "%Y-%m-%d")
trawl.data$DAY_OF_YEAR <- (trawl.data$DATETIME)$yday + 1

spring.day <- round(mean(trawl.data$DAY_OF_YEAR[trawl.data$SEASON == "SPRING"]))
spring.date <- strptime(paste("2015", spring.day), format="%Y %j")
spring.query <- format(spring.date, "%Y.%m.%d")

fall.day <- round(mean(trawl.data$DAY_OF_YEAR[trawl.data$SEASON == "FALL"]))
fall.date <- strptime(paste("2015", fall.day), format="%Y %j")
fall.query <- format(fall.date,"%Y.%m.%d")


##### make fall prediction stack based on mean day of fall trawls

all.sst <- stack("Z:/COCA-conf/OISST/EC_sst_1981_2015_OISST-V2-AVHRR_agg_combined.nc")

fall.p <- grep(fall.query, names(all.sst))

DAILYMU.OISST <- mean(all.sst[[fall.p]])
d30MU.OISST <- mean(all.sst[[(fall.p-29):fall.p]])
d180MU.OISST <- mean(all.sst[[(fall.p-179):fall.p]])
d365MU.OISST <- mean(all.sst[[(fall.p-364):fall.p]])
d1825MU.OISST <- mean(all.sst[[(fall.p-1824):fall.p]])
d3650MU.OISST <- mean(all.sst[[(fall.p-3649):fall.p]])

Ts <- raster::rotate(stack(DAILYMU.OISST, 
                           d30MU.OISST, 
                           d180MU.OISST, 
                           d365MU.OISST, 
                           d1825MU.OISST, 
                           d3650MU.OISST))

random <- Ts[[1]] * 0
season <- random + 1

fall.2015.prediction.stack <- stack(depth, 
                                    tri, 
                                    sed.size, 
                                    sed.type, 
                                    Ts, 
                                    season, 
                                    random)

names(fall.2015.prediction.stack) <- c("DEPTH", 
                                       "TRI", 
                                       "SED.SIZE", 
                                       "SED.TYPE",
                                       "DAILYMU.OISST", 
                                       "d30MU.OISST", 
                                       "d180MU.OISST",
                                       "d365MU.OISST", 
                                       "d1825MU.OISST", 
                                       "d3650MU.OISST", 
                                       "SEASON", 
                                       "RANDOM")

writeRaster(fall.2015.prediction.stack, "Z:/COCA-conf/GIS/fall_2015_predictors.grd", overwrite=T)



##### make spring prediction stack based on mean spring trawl day

spring.p <- grep(spring.query, names(all.sst))

DAILYMU.OISST <- mean(all.sst[[spring.p]])
d30MU.OISST <- mean(all.sst[[(spring.p-29):spring.p]])
d180MU.OISST <- mean(all.sst[[(spring.p-179):spring.p]])
d365MU.OISST <- mean(all.sst[[(spring.p-364):spring.p]])
d1825MU.OISST <- mean(all.sst[[(spring.p-1824):spring.p]])
d3650MU.OISST <- mean(all.sst[[(spring.p-3649):spring.p]])

Ts <- raster::rotate(stack(DAILYMU.OISST, 
                           d30MU.OISST, 
                           d180MU.OISST, 
                           d365MU.OISST, 
                           d1825MU.OISST, 
                           d3650MU.OISST))

season <- season + 1

spring.2015.prediction.stack <- stack(depth, 
                                      tri, 
                                      sed.size, 
                                      sed.type, 
                                      Ts, 
                                      season, 
                                      random)

names(spring.2015.prediction.stack) <- c("DEPTH", 
                                         "TRI", 
                                         "SED.SIZE", 
                                         "SED.TYPE",
                                         "DAILYMU.OISST", 
                                         "d30MU.OISST", 
                                         "d180MU.OISST",
                                         "d365MU.OISST", 
                                         "d1825MU.OISST", 
                                         "d3650MU.OISST", 
                                         "SEASON", 
                                         "RANDOM")

writeRaster(spring.2015.prediction.stack, "Z:/COCA-conf/GIS/spring_2015_predictors.grd", overwrite=T)



