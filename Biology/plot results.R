##### 

##### load ocean for plotting

ocean <- readOGR(dsn = "I:/jschuetz/Documents/SESYNC/GIS", 
                 layer = "ocean",
                 verbose = FALSE)

eq.area.ocean <- spTransform(ocean, CRS(p))


plot(eq.area.exp.scenarios[[1]])
plot(eq.area.ocean, add=T)
