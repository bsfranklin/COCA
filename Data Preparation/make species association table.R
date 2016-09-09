##### read in descriptions of pairwise matches between nefsc, landings, and vtr data sets

landings.vtr <- read.csv("Z:/COCA-conf/Output/species tables/species_LANDINGS_VTR.csv")
nefsc.vtr <- read.csv("Z:/COCA-conf/Output/species tables/species_NEFSC_VTR.csv")
nefsc.landings <- read.csv("Z:/COCA-conf/Output/species tables/species_LANDINGS_NEFSC.csv")

##### select only those cases where there are matches

nefsc.landings.match <- nefsc.landings[nefsc.landings$COMNAME != "",]
nefsc.merged <- merge(nefsc.landings.match, nefsc.vtr[,1:2])

a <-landings.vtr[!is.na(landings.vtr$IN_VTR_DATA_SET) & !is.na(landings.vtr$IN_LANDINGS_DATA_SET), c(1,4)]
b <- nefsc.vtr[!is.na(nefsc.vtr$IN_NEFSC_DATA_SET) & !is.na(nefsc.vtr$IN_VTR_DATA_SET), c(1,5)]
c <- nefsc.merged[, c(3,6)]

##### merge all the matches into one data frame

ab <- merge(a, b, all=T)
abc <- merge(ab, c, all=T)
d <- abc[!duplicated(abc),]

##### make data frames with unique species identifiers for each data set

nefsc.all <- nefsc.vtr[!duplicated(nefsc.vtr$SVSPP) & !is.na(nefsc.vtr$SVSPP), c(1:3,8,10)]
vtr.all <- nefsc.vtr[!duplicated(nefsc.vtr$VTR_SPP_CODE) & !is.na(nefsc.vtr$VTR_SPP_CODE), c(4:6,10)]
landings.all <- nefsc.landings[!duplicated(nefsc.landings$LANDINGS_SPP_CODE) & !is.na(nefsc.landings$LANDINGS_SPP_CODE), c(1:3,5)]

##### merge unique/complete identifiers with the data frame describing matches to produce species association table
##### for all three data sets

d1 <- merge(d, nefsc.all, all=T)
d2 <- merge(d1, vtr.all, all=T)
d3 <- merge(d2, landings.all, all=T)

##### clean up a bit and reorder columns

d3[is.na(d3$NOTES_NEFSC_VTR_MATCHES), "NOTES_NEFSC_VTR_MATCHES"] <- ""
d3[is.na(d3$NOTES_NEFSC_LANDINGS_MATCHES), "NOTES_NEFSC_LANDINGS_MATCHES"] <- ""
i <- d3$HARE_ET_AL_2016_SPP == 1
d3$HARE_ET_AL_2016_SPP[!i] <- NA

out <- d3[,c(4:6,8,2,9,1,10,11,3,12,7)]

##### write

write.csv(out, "Z:/COCA-conf/myCOCA/species_association_table.csv", row.names=F)

