##### identify which ports report landings data every year between 2011 and 2015

library(tidyverse)
library(stringr)


##### identify ports sampled every year 2011-2015

my.ports <- tbl_df(read.csv("Z:/COCA-conf/Landings/Mills_1982-2015 GAR Landings combined sheets.csv")) %>%
  filter(YEAR > 2010, YEAR < 2016, duplicated(str_c(YEAR, STATE, PORT, PORT_CODE)) == FALSE) %>%
  group_by(STATE, PORT, PORT_CODE) %>%
  summarise(N.YEARS = n()) %>%
  filter(N.YEARS == 5)


##### landed lbs and value for ports sampled every year 2011-2015

my.lbs.value <- tbl_df(read.csv("Z:/COCA-conf/Landings/Mills_1982-2015 GAR Landings combined sheets.csv")) %>%
  filter(YEAR > 2010, YEAR < 2016) %>%
  semi_join(my.ports) %>%
  group_by(STATE, PORT, PORT_CODE) %>%
  summarise(SUM_LANDED_LBS_2011_2015 = sum(LANDED_LBS, na.rm=T), 
            SUM_VALUE_2011_2015 = sum(VALUE, na.rm=T)) %>%
  mutate(MEAN_LANDED_LBS_2011_2015 = SUM_LANDED_LBS_2011_2015/5,
         MEAN_VALUE_2011_2015 = SUM_VALUE_2011_2015/5)
  
write.csv(my.lbs.value, "Z:/COCA-conf/Output/ports sampled every year 2011-2015 with landed lbs and value aggregated.csv", row.names=F)

final.ports <- filter(my.lbs.value, MEAN_VALUE_2011_2015 > 1000000,  "OTHER")

# footprints gear type x port and just port and shelfwide
# link commit id to output

##### read in colburn communities and join

colburn <- tbl_df(read.csv("Z:/COCA-conf/Landings/Ports Aggregated.csv"))
names(colburn)[1:2] <- c("PORT", "STATE")
my.ports <- left_join(my.ports, colburn)
misses <- anti_join(colburn, my.ports)



##### look at how many colburn ports are not listed in full landings data set

all.ports <- tbl_df(read.csv("Z:/COCA-conf/Landings/Mills_1982-2015 GAR Landings combined sheets.csv")) %>%
  filter(duplicated(str_c(YEAR, STATE, PORT, PORT_CODE)) == FALSE) %>%
  group_by(STATE, PORT, PORT_CODE) %>%
  summarise(N.YEARS = n())

all.misses <- anti_join(colburn, all.ports)
