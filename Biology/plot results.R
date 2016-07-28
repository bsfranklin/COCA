library(raster)
library(rgeos)

##### 

##### load ocean for plotting

eq.area.exp.scenarios <- stack("I:/jschuetz/Documents/SESYNC/Output/SVSPP_301_spring_scenarios.grd")

ocean <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/GIS", 
                 layer = "ocean",
                 verbose = FALSE)

eq.area.ocean <- spTransform(ocean, CRS(proj4string(eq.area.exp.scenarios)))


plot(eq.area.exp.scenarios[[1]])
plot(eq.area.ocean, add=T)


box <- extent(-350000, 650000, 0, 1000000)

z<-crop(eq.area.exp.scenarios[[1]], box)

plot(z)
plot(eq.area.ocean, add=T)

zz<-z
zz[]<-1
plot(zz)