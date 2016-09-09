library(raster)
library(dismo)
library(gbm)

fall.2015.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/GIS/fall.2015.prediction.stack.grd")
spring.2015.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/GIS/spring.2015.prediction.stack.grd")

load("I:/jschuetz/Documents/SESYNC/ForJS/trawl.model.dat.adj.02102016.Rdata")

##### rename trawl data

trawl.data <- trawl.dat.full.reduced2
rm(trawl.dat.full.reduced2)

trawl.data$RANDOM <- runif(nrow(trawl.data),0,1)

trawl.data$BIOMASS[is.na(trawl.data$BIOMASS)] <- 0
trawl.data$LOG.BIOMASS.1 <- log(trawl.data$BIOMASS + 1)
  
spp.sesync <- c(301,72,73,74,75,103)

fall.out <- stack()
spring.out <- stack()

for (spp in 1:length(spp.sesync)){
  
  ##### build model using each season of data

  my.data <- trawl.data[trawl.data$SVSPP == spp.sesync[spp],]
  
  my.data$SEQUENCE <- sample(seq(1,nrow(my.data),1))
                             
  train.data <- my.data[my.data$SEQUENCE < 0.8*(nrow(my.data)),]
  test.data <- my.data[my.data$SEQUENCE >= 0.8*(nrow(my.data)),]
    
  modelfull <- gbm.step(data=train.data, 
                    gbm.x=c(8,57,48:55,62), 
                    gbm.y=63, 
                    family="gaussian", 
                    tree.complexity=4, 
                    learning.rate=0.01,
                    bag.fraction=0.5)

  select.y <- gbm.simplify(modelfull, n.folds=10, plot=TRUE)

  model <- gbm.step(data=train.data, 
                    gbm.x=select.y$pred.list[[length(select.y$pred.list)]], 
                    gbm.y=63, 
                    family="gaussian", 
                    tree.complexity=4, 
                    learning.rate=0.01,
                    bag.fraction=0.5)
  
  save(model, file=paste("C:/Users/jschuetz/Documents/SESYNC_out/SVSPP_", spp.sesync[spp], "_FALL_SPRING_LOG_BIOMASS_GBM", sep=""))

  fall.prediction <- raster::predict(fall.2015.prediction.stack,
                                   model,
                                   fun=predict,
                                   na.rm=T,
                                   type="response",
                                   n.trees=model$n.trees)

  fall.out <- stack(fall.out, fall.prediction)
  
  spring.prediction <- raster::predict(spring.2015.prediction.stack,
                                   model,
                                   fun=predict,
                                   na.rm=T,
                                   type="response",
                                   n.trees=model$n.trees)

  spring.out <- stack(spring.out, spring.prediction)
  
}

writeRaster(fall.out, "C:/Users/jschuetz/Documents/SESYNC_out/FALL_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
writeRaster(spring.out, "C:/Users/jschuetz/Documents/SESYNC_out/SPRING_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")



