setwd("Z:/COCA-conf/VTR and permit")

##### establish empty object to fill

ports <- NULL


##### generate list of files in folder and remove the folder with incomplete files

my.files <- list.files()
my.files <- my.files[1:19]


##### read in separate VTR data files and find unique combinations of port, state, port code 

for (d in 1:length(my.files)){
  
  vtr.data.1 <- read.csv(my.files[d])
  ports.temp <- aggregate(VTR_YEAR ~ PORT_NAME + STATE + PORT_CODE, vtr.data, length)
  ports <- rbind(ports, ports.temp)
  
}


##### aggregate across years

ports <- aggregate(VTR_YEAR ~ PORT_NAME + STATE + PORT_CODE, ports, length)
ports <- ports[,1:3]
ports$QUERY <- paste(ports[,1], ports[,2], sep=", ")


##### geocode based on port name

library(ggmap)

lat.lon <- geocode(ports$QUERY, 
                   output = "more",
                   source = "google", 
                   messaging = FALSE,
                   override_limit = FALSE)

ports.lat.lon <- cbind(ports, lat.lon)

write.csv(ports.lat.lon, "geocoded port names from VTR data 1996-2015.csv", row.names=F)


##### THESE WILL NEED PROOFING