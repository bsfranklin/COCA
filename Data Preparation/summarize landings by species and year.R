library(tidyverse)

setwd("Z:/COCA-conf/Landings")

landings <- read.csv("Mills_1982-2015 GAR Landings combined sheets.csv") %>%
  group_by(SPECIES, YEAR) %>%
  summarize(LANDED_LBS = sum(LANDED_LBS, na.rm = TRUE), VALUE = sum(VALUE, na.rm = TRUE))

write.csv(landings, "Z:/COCA-conf/Output/landings by species and year.csv", row.names = FALSE)
