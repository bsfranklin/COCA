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







##### aggregate values based on colburn communities when those are different from ports

# this relaxes the criterion that ports report landings every year from 2011-2015...because they
# combine with ports that are sampled every year to form colburns' fishing communities


my.ports <- tbl_df(read.csv("Z:/COCA-conf/Landings/Mills_1982-2015 GAR Landings combined sheets.csv")) %>%
  filter(YEAR > 2010, YEAR < 2016, duplicated(str_c(YEAR, STATE, PORT, PORT_CODE)) == FALSE) %>%
  group_by(STATE, PORT, PORT_CODE) %>%
  summarise(N.YEARS = n())


##### landed lbs and value for ports sampled every year 2011-2015

my.lbs.value <- tbl_df(read.csv("Z:/COCA-conf/Landings/Mills_1982-2015 GAR Landings combined sheets.csv")) %>%
  filter(YEAR > 2010, YEAR < 2016) %>%
  semi_join(my.ports) %>%
  group_by(STATE, PORT, PORT_CODE) %>%
  summarise(SUM_LANDED_LBS_2011_2015 = sum(LANDED_LBS, na.rm=T), 
            SUM_VALUE_2011_2015 = sum(VALUE, na.rm=T)) %>%
  mutate(MEAN_LANDED_LBS_2011_2015 = SUM_LANDED_LBS_2011_2015/5,
         MEAN_VALUE_2011_2015 = SUM_VALUE_2011_2015/5)


colburn <- read_csv("Z:/COCA-conf/Landings/Ports Aggregated.csv")
names(colburn)[1:2] <- c("PORT", "STATE")

# change port names in colburn data so they match landings data port names

colburn$PORT[colburn$PORT == "SPRUCE HEAD"] <- "SPRUCEHEAD"
colburn$PORT[colburn$PORT == "SAINT GEORGE"] <- "ST. GEORGE"
colburn$PORT[colburn$PORT == "GOULDSBORO"] <- "GOLDSBORO"


my.lbs.value.c <- left_join(my.lbs.value, colburn) %>%
  mutate(JGS.COMMUNITY = str_to_upper(GEO_NAME_ACS2014)) %>%
  mutate(JGS.COMMUNITY = gsub("/", "_", JGS.COMMUNITY, fixed = T)) %>%
  mutate(JGS.COMMUNITY = gsub(",", "", JGS.COMMUNITY, fixed = T)) %>%
  mutate(JGS.COMMUNITY = gsub(" ", "_", JGS.COMMUNITY, fixed = T)) %>%
  mutate(JGS.COMMUNITY = gsub("-", "_", JGS.COMMUNITY, fixed = T)) 
  
i <- is.na(my.lbs.value.c$JGS.COMMUNITY)

my.lbs.value.c[i, "JGS.COMMUNITY"] <- str_c(my.lbs.value.c$PORT[i], "_", my.lbs.value.c$STATE[i])

final.communities <- group_by(my.lbs.value.c, JGS.COMMUNITY) %>%
  mutate(MEAN_LANDED_LBS_2011_2015_COMMUNITY = sum(MEAN_LANDED_LBS_2011_2015), 
            MEAN_VALUE_2011_2015_COMMUNITY = sum(MEAN_VALUE_2011_2015)) %>%
  filter(MEAN_VALUE_2011_2015_COMMUNITY > 1000000, !str_detect(JGS.COMMUNITY, "OTHER"))

write.csv(final.communities, "Z:/COCA-conf/Output/communities and ports meeting selection criteria.csv", row.names=F)

