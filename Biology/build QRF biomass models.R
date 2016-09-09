library(raster)
library(quantregForest)
library(ModelMap)
library(dplyr)

load("Z:/COCA-conf/NEFSC trawl/trawl_data_for_models_AA_02102016.RData")

##### add random and log biomass +1 columns

trawl.data$RANDOM <- runif(nrow(trawl.data),0,1)
trawl.data$BIOMASS[is.na(trawl.data$BIOMASS)] <- 0
trawl.data$LOG.BIOMASS.1 <- log(trawl.data$BIOMASS + 1)
  

##### identify species for generating models

spp.sesync <- c(301,72,73,74,75,103)


##### create empty prediction stacks to fill

fall.out <- stack()
spring.out <- stack()


##### loop through species

for (spp in 1:length(spp.sesync)){
  
  setwd("~/Sandbox/SVSPP_301")
  
  my.data <- trawl.data[trawl.data$SVSPP == spp.sesync[spp], c(1,8,25,48:58,59,61)]
  my.data <- my.data[!is.na(my.data$d3650MU.OISST),]
  
  write.csv(my.data, "my.data.csv", row.names=F)
  
  qdatafn <- "my.data.csv"
  qdata.trainfn <- "my.data_TRAIN.csv"
  qdata.testfn <- "my.data_TEST.csv"
  
  folder <- getwd()
  
  get.test( proportion.test=0.2,
            qdatafn=qdatafn,
            seed=13,
            folder=folder,
            qdata.trainfn=qdata.trainfn,
            qdata.testfn=qdata.testfn)
  
  predList <- c( "DAILYMU.OISST",
                 "d30MU.OISST",
                 "d180MU.OISST",
                 "d365MU.OISST",
                 "d1825MU.OISST",
                 "d3650MU.OISST",
                 "DEPTH",
                 "TRI")
  
  predFactor <- FALSE
  
  unique.rowname <- "ID"
  
  rastLUTfn_spring2015 <- read.table("predictors.spring.2015_LUT.csv",
                                     header=FALSE,
                                     sep=",",
                                     stringsAsFactors=FALSE)
  
  model.explore(qdata.trainfn=qdata.trainfn,
                folder=folder,
                predList=predList,
                predFactor=predFactor,
                OUTPUTfn="LOBSTER biomass",
                response.name="LOG.BIOMASS.1",
                response.type="continuous",
                unique.rowname=unique.rowname,
                device.type=c("png"),
                cex=1.2,
                rastLUTfn=rastLUTfn_spring2015,
                na.value=-9999,
                col.ramp=terrain.colors(101),
                col.cat=c("wheat1","springgreen2","darkolivegreen4",
                         "darkolivegreen2","yellow","thistle2",
                         "brown2","brown4"))
                
  model.type <- "QRF"
  
  MODELfn <- "QRF"
  
  response.type <- "continuous"
  
  response.name <- "LOG.BIOMASS.1"
  
  model.qrf <- model.build(model.type=model.type,
                           qdata.trainfn=qdata.trainfn,
                           folder=folder,
                           unique.rowname=unique.rowname,
                           MODELfn=MODELfn,
                           predList=predList,
                           predFactor=predFactor,
                           response.name=response.name,
                           response.type=response.type,
                           seed=13,
                           na.action="na.omit")
  
  model.mapmake(model.obj=model.qrf,
                folder=folder,
                MODELfn=MODELfn,
                rastLUTfn=rastLUTfn_spring2015,
                na.action="na.omit",
                quantiles=c(0.25, 0.5, 0.75))
  
  maps <- stack("QRF_map_RF.img", "QRF_map_QRF.img")
  
  names(maps) <- c("QRF_mean", 
                   "QRF_quantile_0.25", 
                   "QRF_quantile_0.50", 
                   "QRF_quantile_0.75")

  maps <- exp(maps)-1
  
  writeRaster(maps, "SVSPP_301_BIOMASS_spring_2015.img")

  
  
  
  
  
  model.type <- "RF"
  
  MODELfn <- "RF"
  
  response.type <- "continuous"
  
  response.name <- "LOG.BIOMASS.1"
  
  model.rf <- model.build(model.type=model.type,
                           qdata.trainfn=qdata.trainfn,
                           folder=folder,
                           unique.rowname=unique.rowname,
                           MODELfn=MODELfn,
                           predList=predList,
                           predFactor=predFactor,
                           response.name=response.name,
                           response.type=response.type,
                           seed=13,
                           na.action="na.omit")
  
  pred.rf <- model.diagnostics( model.obj=model.rf,
                                 qdata.testfn=qdata.testfn,
                                 folder=folder,
                                 MODELfn=MODELfn,
                                 unique.rowname=unique.rowname,
                                 # Model Validation Arguments
                                 prediction.type="TEST",
                                 device.type=c("pdf"),
                                 cex=1.2)
  
  model.mapmake(model.obj=model.rf,
                folder=folder,
                MODELfn=MODELfn,
                rastLUTfn=rastLUTfn_spring2015,
                map.sd=TRUE)  
  
  rfmap <- raster("RF_map.img")
  rfmap <- exp(rfmap)-1
  plot(rfmap)
  
  diff<-rfmap-maps[[1]]
  
  
  plot(diff)
  
  folder <- getwd()
  
  get.test( proportion.test=0.2,
            qdatafn=data.fn,
            seed=13,
            folder=folder,
            qdata.trainfn=data.train.fn,
            qdata.testfn=data.test.fn)
  
  
  ##### build model using each season of data
  save(qrf, file=paste("SVSPP_", spp.sesync[spp], "_QRF", sep=""))

  
  ##### run predictions
  
  fall_2015_predictors <- stack("Z:/COCA-conf/GIS/predictor stacks/fall_2015_predictors.grd")
  rastLUTfn <- "Z:/COCA-conf/GIS/predictor stacks/fall_2015_predictors.grd"
  model.mapmake( model.obj=qrf,
                 folder=folder,
                 MODELfn=MODELfn,
                 rastLUTfn=fall_2015_predictors,
                 na.action="na.omit",
                 quantiles=c(0.5, 0.025, 0.975))
  
  qp <- stack("modelmap_qrf_map_QRF.img")
  
  qp.back <- exp(qp)-1
  
  writeRaster(qp.back, "modelmap_qrf_map_QRF_back_transformed.img")
  
  spring.prediction <- raster::predict(spring_2015_predictors,
                                   model,
                                   fun=predict,
                                   na.rm=T,
                                   type="response",
                                   n.trees=model$n.trees)

}

writeRaster(fall.out, "C:/Users/jschuetz/Documents/SESYNC_out/FALL_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")
writeRaster(spring.out, "C:/Users/jschuetz/Documents/SESYNC_out/SPRING_PREDICTIONS_FROM_ANNUAL_LOG_BIOMASS_GBM.grd")



