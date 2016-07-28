library(raster)
library(dismo)
library(gbm)

# load prediction stacks

load("I:/jschuetz/Documents/SESYNC/R/COCA_prediction_stacks.RData")

scenario <- c("spring.2015", "fall.2015", "spring.1C", "fall.1C", "spring.2C", "fall.2C")

spp.sesync <- c(301, 72, 73, 74, 75, 103)

for (i in 1:length(scenario)){
  
  stack.out <- stack()

  for (spp in 1:length(spp.sesync)){
  
    ##### build model using each season of data
  
    load(paste("I:/jschuetz/Documents/SESYNC/Output/models and evaluations/SVSPP_", spp.sesync[spp], "_FALL_SPRING_LOG_BIOMASS_GBM", sep=""))
  
    scenario.prediction <- raster::predict(get(paste(scenario[i], ".prediction.stack", sep="")),
                                     model,
                                     fun=predict,
                                     na.rm=T,
                                     type="response",
                                     n.trees=model$n.trees)
    
    stack.out <- stack(stack.out, scenario.prediction)
  
  }

  writeRaster(stack.out, paste("I:/jschuetz/Documents/SESYNC/Output/predictions/", scenario[i], "_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd", sep=""), overwrite=T)

}


##### for PA models


for (i in 1:length(scenario)){
  
  stack.out <- stack()
  
  for (spp in 1:length(spp.sesync)){
    
    ##### build model using each season of data
    
    load(paste("I:/jschuetz/Documents/SESYNC/Output/models and evaluations/SVSPP_", spp.sesync[spp], "_FALL_SPRING_GBM", sep=""))
    
    scenario.prediction <- raster::predict(get(paste(scenario[i], ".prediction.stack", sep="")),
                                           model,
                                           fun=predict,
                                           na.rm=T,
                                           type="response",
                                           n.trees=model$n.trees)
    
    stack.out <- stack(stack.out, scenario.prediction)
    
  }
  
  writeRaster(stack.out, paste("I:/jschuetz/Documents/SESYNC/Output/predictions/", scenario[i], "_PREDICTIONS_FROM_ANNUAL_GBM.grd", sep=""), overwrite=T)
  
}

