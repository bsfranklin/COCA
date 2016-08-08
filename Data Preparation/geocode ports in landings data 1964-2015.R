setwd("Z:/COCA-conf/Landings")

##### create a list of ports from landings data

ports <- NULL

landings.1 <- read.csv("Mills_1964-2015 MA, ME, RI Landings combined sheets.csv")
landings.2 <- read.csv("Mills_1982-2015 GAR Landings combined sheets.csv")

landings <- rbind(landings.1, landings.2)

ports <- aggregate(YEAR ~ PORT + STATE + PORT_CODE, landings, length)
ports <- ports[,1:3]
ports$QUERY <- paste(ports[,1], ports[,2], sep=", ")
ports <- ports[ports$PORT != "",]


##### geocode

library(ggmap)

lat.lon <- geocode(ports$QUERY, 
                   output = "more",
                   source = "google", 
                   messaging = FALSE,
                   override_limit = FALSE)

ports.lat.lon <- cbind(ports, lat.lon)

write.csv(ports.lat.lon, "Z:/COCA-conf/Output/geocoded port names from all landings data 1964-2015.csv", row.names=F)


##### THESE WILL NEED PROOFING
