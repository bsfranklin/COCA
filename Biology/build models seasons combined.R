library(raster)
library(dismo)
library(gbm)

fall.2015.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/GIS/fall.2015.prediction.stack.grd")
spring.2015.prediction.stack <- stack("I:/jschuetz/Documents/SESYNC/GIS/spring.2015.prediction.stack.grd")

trawl.data$RANDOM <- runif(nrow(trawl.data),0,1)

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
                    gbm.y=59, 
                    family="bernoulli", 
                    tree.complexity=4, 
                    learning.rate=0.01,
                    bag.fraction=0.5)

  select.y <- gbm.simplify(modelfull, n.folds=10, plot=TRUE)

  model <- gbm.step(data=train.data, 
                    gbm.x=select.y$pred.list[[length(select.y$pred.list)]], 
                    gbm.y=59, 
                    family="bernoulli", 
                    tree.complexity=4, 
                    learning.rate=0.01,
                    bag.fraction=0.5)
  
  save(model, file=paste("C:/Users/jschuetz/Documents/SESYNC_out/SVSPP_", spp.sesync[spp], "_FALL_SPRING_GBM", sep=""))

  eval <- evaluate(model, 
                       p=test.data[test.data$PRESENCE==1, select.y$pred.list[[length(select.y$pred.list)]]], 
                       a=test.data[test.data$PRESENCE==0, select.y$pred.list[[length(select.y$pred.list)]]], 
                       n.trees=model$n.trees)
                           
  save(eval, file=paste("C:/Users/jschuetz/Documents/SESYNC_out/SVSPP_", spp.sesync[spp], "_FALL_SPRING_GBM_EVAL", sep=""))
  
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

writeRaster(fall.out, "C:/Users/jschuetz/Documents/SESYNC_out/FALL_PREDICTIONS_FROM_ANNUAL_GBM.grd")
writeRaster(spring.out, "C:/Users/jschuetz/Documents/SESYNC_out/SPRING_PREDICTIONS_FROM_ANNUAL_GBM.grd")



