##### identify which ports report landings data every year between 1996 and 2015

setwd("Z:/COCA-conf/Landings")

landings <- read.csv("Mills_1982-2015 GAR Landings combined sheets.csv")

landings.1996.2015 <- landings[landings$YEAR > 1995,]

##### identify ports sampled every year

all.port.years.1996.2015 <- aggregate(cbind(VALUE, LANDED_LBS) ~ PORT_CODE + PORT + STATE + YEAR, landings.1996.2015, length)
all.ports.1996.2015 <- aggregate(YEAR ~ PORT_CODE + PORT + STATE, all.port.years.1996.2015, length)
sampled.all.years <- all.ports.1996.2015[all.ports.1996.2015$YEAR == 20,]


##### summarize total lbs landed and value of landings across years

value.lbs.years <- aggregate(cbind(VALUE, LANDED_LBS) ~ PORT_CODE + PORT + STATE + YEAR, landings.1996.2015, sum)
value.lbs.years$LANDED_LBS <- as.numeric(value.lbs.years$LANDED_LBS)
value.lbs.years$VALUE <- as.numeric(value.lbs.years$VALUE)
value.lbs <- aggregate(cbind(VALUE, LANDED_LBS) ~ PORT_CODE + PORT + STATE, value.lbs.years, sum)
value.lbs.out <- merge(all.ports.1996.2015.out[,-4], value.lbs)

##### remove ports with OTHER in name

other <- grep("OTHER", value.lbs.out$PORT)

value.lbs.out <- value.lbs.out[-other,]

write.csv(value.lbs.out, "Z:/COCA-conf/Output/ports sampled every year 1996-2015.csv", row.names=F)

