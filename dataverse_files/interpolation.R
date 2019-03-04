library(raster)
library(rgdal)
library(rgeos)
library(plyr)
library(reshape)
library(ineq)
library(maps)
library(maptools)
library(foreign)
library(RColorBrewer)
library(classInt)
data(state.fips)
state.fips <- unique(state.fips[,c("fips","abb")])
state.fips$abb <- as.character(state.fips$abb)
state.fips <- rbind(state.fips, c(2, "AK"))
state.fips <- rbind(state.fips, c(15, "HI"))
rownames(state.fips) <- state.fips$abb
fips.state <- state.fips
rownames(fips.state) <- fips.state$fips
data(county.fips)

#### http://publications.newberry.org/ahcbp/
histcounties <- readOGR("/Users/mblackwell/workland/data/US_AtlasHCB_Counties_Gen001/US_HistCounties_Gen001_Shapefile", layer = "US_HistCounties_Gen001")
histcounties$fips <- as.numeric(as.character(histcounties$FIPS))

histstates <- readOGR("/Users/mblackwell/workland/data/US_AtlasHCB_StateTerr_Gen001/US_HistStateTerr_Gen001_Shapefile", layer = "US_HistStateTerr_Gen001")

## These data are from Jeremy Atack
## https://my.vanderbilt.edu/jeremyatack/data-downloads/
## other possible sources for rails: http://railroads.unl.edu/resources/
rail <- readOGR("/Users/mblackwell/workland/data/RR1826-19113", "RR1826-1911")
rail <- rail[rail$InOpBy < 1860,]
rivers <- readOGR("/Users/mblackwell/workland/data/SteamboatNavigatedRivers2", layer = "SteamboatNavigatedRivers")
rivers <- rivers[rivers$START < 1860 & rivers$END > 1870,]
canals <- readOGR("/Users/mblackwell/workland/data/19thC_Canals2", layer = "19thC_Canals")
canals <- canals[which(canals$OPENED < 1860 & canals$CLOSED > 1860),]
## coastal/shoreline data comes from the NOAA
## https://coast.noaa.gov/htdata/SocioEconomic/NOAA_CoastalCountyDefinitions.pdf
##
coastal <- read.csv("/Users/mblackwell/workland/data/coastal.csv")


histcounties <- spTransform(histcounties, CRS(proj4string(rivers)))
histstates <- spTransform(histstates, CRS(proj4string(rivers)))
canals <- spTransform(canals, CRS(proj4string(rivers)))
rail <- spTransform(rail, CRS(proj4string(rivers)))

histcounties$fips[which(histcounties$NAME == "CARROLL (ext)")] <- 22035

cc1860 <- which(histcounties$START_N <= 18590101 & histcounties$END_N >= 18590101)
counties1860 <- histcounties[cc1860,]
kk <- as.character(counties1860$NAME[counties1860$STATE_TERR == "South Carolina"])
kk <- sub(" Dist.", "", kk)
kk <- paste("south carolina,",tolower(kk), sep = "")
kk.fips <- NA
kk.fips[kk %in% county.fips$polyname] <- unlist(lapply(kk, function(x) county.fips$fips[county.fips$polyname == x]))
counties1860$fips[counties1860$STATE_TERR == "South Carolina"] <- kk.fips
counties1860 <- counties1860[!is.na(counties1860$fips),]
newpolys <- unionSpatialPolygons(counties1860, counties1860$fips)
kk <- unique(as.data.frame(counties1860)[,c("fips", "STATE_TERR")])
rownames(kk) <- as.character(kk$fips)
counties1860 <- SpatialPolygonsDataFrame(newpolys, data = kk)
counties1860$coarea1860.km2 <- gArea(counties1860, byid = TRUE)/(1000^2)

cc2000 <- which(histcounties$START_N <= 20001231 & histcounties$END_N >= 20001231)
counties2000 <- histcounties[cc2000,]
counties2000$fips <- as.numeric(as.character(counties2000$FIPS))
counties2000 <- counties2000[!is.na(counties2000$fips),]
counties2000$coarea2000.km2 <- gArea(counties2000, byid = TRUE)/(1000^2)
counties2000 <- counties2000[!(counties2000$STATE_TERR %in% c("Alaska", "Hawaii")),]
counties2000$coastal <- 1 * (counties2000$fips %in% coastal$fips)
counties2000$river <- 1 * (colSums(gCrosses(counties2000, rivers, byid = TRUE)) > 0)
counties2000$canal <- 1 * (colSums(gCrosses(counties2000, canals, byid = TRUE)) > 0)
counties2000$water1860 <- 1 * (counties2000$coastal + counties2000$river + counties2000$canal > 0)
counties2000$rail1860 <- 1 * (colSums(gCrosses(counties2000, rail, byid = TRUE)) > 0)

## test plots
## plot(counties2000, border = "grey80", col = ifelse(counties2000$coastal, "grey60", "white"))
## plot(counties1860, border = "grey80", col = ifelse(counties1860$water, "grey60", "white"))
## plot(rivers[rivers$END > 1860,], col = "dodgerblue", lwd = 1, add = TRUE)
## plot(counties2000, border = "grey80", col = ifelse(counties2000$canal, "grey60", "white"))
## plot(canals, col = "dodgerblue", lwd = 1, add = TRUE)
## plot(counties2000, border = "grey80", col = ifelse(counties2000$rail1860, "grey60", "white"))
## plot(rail, col = "indianred", add = TRUE)


ss2000 <- which(histstates$START_N <= 20001231 & histstates$END_N >= 20001231)
states2000 <- histstates[ss2000,]
states2000 <- states2000[!(states2000$ABBR_NAME %in% c("AK", "HI")),]



## 1860 Census
## Data comes from ICPSR 35206
## http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35206
census1860 <- read.dta("../census1860.dta")
vartable <- cbind(names(census1860), attr(census1860, "var.labels"))
cvars <- c("totpop", "stot", "fips", "urb860", "farmval", "slhtot", "rail", "water", "nbmutot","fctot","wmtot",
           "farm39", "farm1019", "farm2049", "farm5099", "farm100", "farm500", "farm1000","equipval", "persest","realest","whtot","acimp","acunimp",
           grep("slh*", colnames(census1860), value = TRUE),
           grep("*acc$", colnames(census1860), value = TRUE))
census1860 <- census1860[census1860$level == 1, ]
census1860 <- census1860[, cvars]
census1860 <- census1860[!is.na(census1860$fips),]
census1860$fips <- floor(census1860$fips)
census1860 <- cast(melt(census1860, id = "fips"), fips ~ variable, sum)
census1860$sprop <- census1860$stot/census1860$totpop
census1860$slaveperholder <- census1860$stot/census1860$slhtot
census1860$totalfarms <- with(census1860, farm39 + farm1019 + farm2049 + farm5099 + farm100 + farm500 + farm1000)
census1860$totac <- with(census1860, acimp + acunimp)
census1860$smallfarms <- with(census1860, farm39 + farm1019 + farm2049)
census1860$largeslhprop <- with(census1860, ( slh5069 + slh7099 + slh100 + slh200 + slh300 + slh500 + slh1000)/slhtot)
census1860$bigfarmprop <- with(census1860, (farm500 + farm1000)/totalfarms)

## Data from ICPSR 08611
## http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/08611
countyvote <- foreign::read.dta("~/workland/data/08611-0001-Data.dta")
sharevars <- grep("PERCENT", attr(countyvote, "var.labels"))
tovars <- grep("TTL", attr(countyvote, "var.labels"))
for (j in sharevars) is.na(countyvote[,j]) <- which(countyvote[,j] > 100)
for (j in tovars) is.na(countyvote[,j]) <- which(countyvote[,j] == 9999999)
countyvote$V1 <- gsub("\\s\\(.*\\)", "", countyvote$V1)
countyvote$V3[countyvote$V3 == 9999] <- NA
names(countyvote)[1] <- "state"
names(countyvote)[2] <- "county"
names(countyvote)[3] <- "fips"
countyvote$fips <- countyvote$fips/10
countyvote$demvotes1856 <- countyvote$V87 * countyvote$V92
countyvote$totalvotes1856 <- countyvote$V92
state.to.abb <- read.csv("state-abbreviations.csv", stringsAsFactors = FALSE)
state.to.abb$State <- toupper(state.to.abb$State)
rownames(state.to.abb) <- state.to.abb$State
countyvote$state.abb <- state.to.abb[countyvote$state,2]
countyvote$sfips <- state.fips[countyvote$state.abb,1]
countyvote$cfips <- as.character(countyvote$fips)
countyvote$cfips[nchar(as.character(countyvote$fips)) == 1] <- paste("00", countyvote$cfips[nchar(as.character(countyvote$fips)) == 1], sep = "")
countyvote$cfips[nchar(as.character(countyvote$fips)) == 2] <- paste("0", countyvote$cfips[nchar(as.character(countyvote$fips)) == 2], sep = "")
countyvote$fips <- as.numeric(paste(countyvote$sfips, countyvote$cfips, sep = ""))
census1860 <- merge(census1860, countyvote[,c("fips", "demvotes1856", "totalvotes1856")], by = "fips", all.x = TRUE)
census1860 <- census1860[which(census1860$fips %in% counties1860$fips),]

## Cacluate the list of intersections and create overlap matrix
inters <- over(counties1860, SpatialPolygons(counties2000@polygons,proj4string=counties2000@proj4string), returnList = TRUE)
A.1860 <- matrix(0, nrow = nrow(counties1860), ncol = nrow(counties2000))
for (i in 1:nrow(counties1860)) {
    totarea <- gArea(counties1860[i,])
    for (j in inters[[i]]) {
        this.int <- gIntersection(counties1860[i,], counties2000[j,])
        if (!is.null(this.int)) A.1860[i,j] <- gArea(this.int)/totarea
    }
}
rownames(A.1860) <- as.character(counties1860$fips)
A.1860[A.1860 < 0.05]  <- 0
A.1860 <- A.1860/rowSums(A.1860)
A0.1860 <- A.1860[as.character(census1860$fips),]


cc1840 <- which(histcounties$START_N <= 18400101 & histcounties$END_N >= 18400101)
counties1840 <- histcounties[cc1840,]
kk <- as.character(counties1840$NAME[counties1840$STATE_TERR == "South Carolina"])
kk <- sub(" Dist.", "", kk)
kk <- paste("south carolina,",tolower(kk), sep = "")
kk.fips <- NA
kk.fips[kk %in% county.fips$polyname] <- unlist(lapply(kk, function(x) county.fips$fips[county.fips$polyname == x]))
counties1840$fips[counties1840$STATE_TERR == "South Carolina"] <- kk.fips
counties1840 <- counties1840[!is.na(counties1840$fips),]
newpolys <- unionSpatialPolygons(counties1840, counties1840$fips)
kk <- unique(as.data.frame(counties1840)[,c("fips", "STATE_TERR")])
rownames(kk) <- as.character(kk$fips)
counties1840 <- SpatialPolygonsDataFrame(newpolys, data = kk)
counties1840$coarea1840.km2 <- gArea(counties1840, byid = TRUE)/(1000^2)

## plot(counties1840, border = "black", col = "grey70", lwd = 0.1)


## 1840 Census
## Data comes from ICPSR 35206
## http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35206
census1840 <- read.dta("../census1840.dta")
vartable <- cbind(names(census1840), attr(census1840, "var.labels"))
cvars <- c("totpop", "stot", "fips", "transpor","fctot")
census1840 <- census1840[census1840$level == 1, ]
census1840 <- census1840[, cvars]
census1840 <- census1840[!is.na(census1840$fips),]
census1840$fips <- floor(census1840$fips)
census1840 <- cast(melt(census1840, id = "fips"), fips ~ variable, sum)
census1840$sprop <- census1840$stot/census1840$totpop
census1840$fbprop <- with(census1840, fctot/totpop)
census1840$water <- census1840$transpor
agcensus1840 <- read.dta("../agcensus1840.dta")
vartable <- cbind(names(agcensus1840), attr(agcensus1840, "var.labels"))
cvars <- c("fips", "totagout")
agcensus1840 <- agcensus1840[agcensus1840$level == 1, ]
agcensus1840 <- agcensus1840[, cvars]
census1840 <- merge(census1840, agcensus1840, by = "fips")
census1840$fvalpc <- with(census1840, totagout/totpop)
names(census1840)[-1] <- paste(names(census1840)[-1], "1840", sep = "")
census1840 <- census1840[which(census1840$fips %in% counties1840$fips),]

inters <- over(counties1840, SpatialPolygons(counties2000@polygons,proj4string=counties2000@proj4string), returnList = TRUE)
A.1840 <- matrix(0, nrow = nrow(counties1840), ncol = nrow(counties2000))
for (i in 1:nrow(counties1840)) {
    totarea <- gArea(counties1840[i,])
    for (j in inters[[i]]) {
        this.int <- gIntersection(counties1840[i,], counties2000[j,])
        if (!is.null(this.int)) A.1840[i,j] <- gArea(this.int)/totarea
    }
}
rownames(A.1840) <- as.character(counties1840$fips)
A.1840[A.1840 < 0.05]  <- 0
A.1840 <- A.1840/rowSums(A.1840)
A0.1840 <- A.1840[as.character(census1840$fips),]



cc1850 <- which(histcounties$START_N <= 18500101 & histcounties$END_N >= 18500101)
counties1850 <- histcounties[cc1850,]
counties1850 <- counties1850[counties1850$STATE_TERR != "Mexican Cession",]
kk <- as.character(counties1850$NAME[counties1850$STATE_TERR == "South Carolina"])
kk <- sub(" Dist.", "", kk)
kk <- paste("south carolina,",tolower(kk), sep = "")
kk.fips <- NA
kk.fips[kk %in% county.fips$polyname] <- unlist(lapply(kk, function(x) county.fips$fips[county.fips$polyname == x]))
counties1850$fips[counties1850$STATE_TERR == "South Carolina"] <- kk.fips
counties1850 <- counties1850[!is.na(counties1850$fips),]
newpolys <- unionSpatialPolygons(counties1850, counties1850$fips)
kk <- unique(as.data.frame(counties1850)[,c("fips", "STATE_TERR")])
rownames(kk) <- as.character(kk$fips)
counties1850 <- SpatialPolygonsDataFrame(newpolys, data = kk)
counties1850$coarea1850.km2 <- gArea(counties1850, byid = TRUE)/(1000^2)

## plot(counties1850, border = "black", col = "grey60", lwd = 0.1)

## 1850 Census
## Data comes from ICPSR 35206
## http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35206
census1850 <- read.dta("../census1850.dta")
vartable <- cbind(names(census1850), attr(census1850, "var.labels"))
cvars <- c("totpop", "stot", "fips", "urb850","methacc", "churacc", "farmval", "rail", "water","fctot")
census1850 <- census1850[census1850$level == 1, ]
census1850 <- census1850[, cvars]
census1850 <- census1850[!is.na(census1850$fips),]
census1850$fips <- floor(census1850$fips)
census1850 <- cast(melt(census1850, id = "fips"), fips ~ variable, sum)
census1850$sprop <- census1850$stot/census1850$totpop
census1850$methprop <- with(census1850, methacc/churacc)
census1850$fvalpc <- with(census1850, farmval/totpop)
census1850$fbprop <- with(census1850, fctot/totpop)
names(census1850)[-1] <- paste(names(census1850)[-1], "1850", sep = "")
census1850 <- census1850[which(census1850$fips %in% counties1850$fips),]

## Cacluate the list of intersections and create overlap matrix
inters <- over(counties1850, SpatialPolygons(counties2000@polygons,proj4string=counties2000@proj4string), returnList = TRUE)
A.1850 <- matrix(0, nrow = nrow(counties1850), ncol = nrow(counties2000))
for (i in 1:nrow(counties1850)) {
    totarea <- gArea(counties1850[i,])
    for (j in inters[[i]]) {
        this.int <- gIntersection(counties1850[i,], counties2000[j,])
        if (!is.null(this.int)) A.1850[i,j] <- gArea(this.int)/totarea
    }
}
rownames(A.1850) <- as.character(counties1850$fips)
A.1850[A.1850 < 0.05]  <- 0
A.1850 <- A.1850/rowSums(A.1850)
A0.1850 <- A.1850[as.character(census1850$fips),]



counties2000$totpop1860 <- NA
counties2000$stot1860 <- NA
counties2000$slhtot1860 <- NA
counties2000$whtot1860 <- NA
counties2000$acimp1860 <- NA
counties2000$acunimp1860 <- NA
counties2000$totac1860 <- NA
counties2000$totalfarms1860 <- NA
counties2000$farm39.1860 <- NA
counties2000$farm1019.1860 <- NA
counties2000$farm2049.1860 <- NA
counties2000$farm5099.1860 <- NA
counties2000$farm100.1860 <- NA
counties2000$farm500.1860 <- NA
counties2000$farm1000.1860 <- NA
counties2000$smallfarms1860 <- NA
counties2000$farmval1860 <- NA
counties2000$fbtot1860 <- NA
counties2000$methacc1860 <- NA
counties2000$churacc1860 <- NA
counties2000$demvotes1856 <- NA
counties2000$totalvotes1856 <- NA
counties2000$land.ineq1860 <- NA
counties2000$totpop1850 <- NA
counties2000$stot1850 <- NA
counties2000$totpop1840 <- NA
counties2000$stot1840 <- NA

fnames <- c("farm39.1860", "farm1019.1860", "farm2049.1860", "farm5099.1860", "farm100.1860", "farm500.1860",
            "farm1000.1860")
posvals <- which(colSums(A0.1860) > 0)
for (i in posvals) {
  weights1860 <- A0.1860[which(A0.1860[,i] > 0),i]
  these.counties <- census1860[census1860$fips %in% as.numeric(names(which(A0.1860[,i] > 0))),]
  counties2000$totpop1860[i] <- sum(weights1860 * these.counties$totpop, na.rm = TRUE)
  counties2000$stot1860[i] <- sum(weights1860 * these.counties$stot, na.rm = TRUE)
  counties2000$slhtot1860[i] <- sum(weights1860 * these.counties$slhtot, na.rm = TRUE)
  counties2000$whtot1860[i] <- sum(weights1860 * these.counties$whtot, na.rm = TRUE)
  counties2000$wmtot1860[i] <- sum(weights1860 * these.counties$wmtot, na.rm = TRUE)
  counties2000$acimp1860[i] <- sum(weights1860 * these.counties$acimp, na.rm = TRUE)
  counties2000$acunimp1860[i] <- sum(weights1860 * these.counties$acunimp, na.rm = TRUE)
  counties2000$totac1860[i] <- sum(weights1860 * these.counties$totac, na.rm = TRUE)
  counties2000$totalfarms1860[i] <- sum(weights1860 * these.counties$totalfarms, na.rm = TRUE)
  counties2000$farm39.1860[i] <- sum(weights1860 * these.counties$farm39, na.rm = TRUE)
  counties2000$farm1019.1860[i] <- sum(weights1860 * these.counties$farm1019, na.rm = TRUE)
  counties2000$farm2049.1860[i] <- sum(weights1860 * these.counties$farm2049, na.rm = TRUE)
  counties2000$farm5099.1860[i] <- sum(weights1860 * these.counties$farm5099, na.rm = TRUE)
  counties2000$farm100.1860[i] <- sum(weights1860 * these.counties$farm100, na.rm = TRUE)
  counties2000$farm500.1860[i] <- sum(weights1860 * these.counties$farm500, na.rm = TRUE)
  counties2000$farm1000.1860[i] <- sum(weights1860 * these.counties$farm1000, na.rm = TRUE)
  counties2000$smallfarms1860[i] <- sum(weights1860 * these.counties$smallfarms, na.rm = TRUE)
  counties2000$farmval1860[i] <- sum(weights1860 * these.counties$farmval, na.rm = TRUE)
  counties2000$fbtot1860[i] <- sum(weights1860 * these.counties$fctot, na.rm = TRUE)
  counties2000$methacc1860[i] <- sum(weights1860 * these.counties$methacc, na.rm = TRUE)
  counties2000$churacc1860[i] <- sum(weights1860 * these.counties$churacc, na.rm = TRUE)
  counties2000$demvotes1856[i] <- sum(weights1860 * these.counties$demvotes1856, na.rm = TRUE)
  counties2000$totalvotes1856[i] <- sum(weights1860 * these.counties$totalvotes1856, na.rm = TRUE)
  if (!is.na(counties2000$totalfarms1860[i])) {
    fdist <- rep(c(6, 14.5, 34.5, 74.5, 299.5, 750, 1000),
                 times = counties2000[i, fnames]@data)
    counties2000$land.ineq1860[i] <- Gini(fdist)
  }
  weights1850 <- A0.1850[which(A0.1850[,i] > 0),i]
  these.counties1850 <- census1850[census1850$fips %in% as.numeric(names(which(A0.1850[,i] > 0))),]
  counties2000$totpop1850[i] <- sum(weights1850 * these.counties1850$totpop1850, na.rm = TRUE)
  counties2000$stot1850[i] <- sum(weights1850 * these.counties1850$stot1850, na.rm = TRUE)
  weights1840 <- A0.1840[which(A0.1840[,i] > 0),i]
  these.counties1840 <- census1840[census1840$fips %in% as.numeric(names(which(A0.1840[,i] > 0))),]
  counties2000$totpop1840[i] <- sum(weights1840 * these.counties1840$totpop1840, na.rm = TRUE)
  counties2000$stot1840[i] <- sum(weights1840 * these.counties1840$stot1840, na.rm = TRUE)
}
counties2000$fvalpc1860 <- counties2000$farmval1860/counties2000$totpop1860
counties2000$fvalpac1860 <- counties2000$farmval1860/counties2000$acimp1860
counties2000$fbprop1860 <- counties2000$fbtot1860/counties2000$totpop1860
counties2000$methprop1860 <- counties2000$methacc1860/counties2000$churacc1860
counties2000$sfarmprop1860 <- counties2000$smallfarms1860/counties2000$totalfarms1860
counties2000$pslave1860 <- counties2000$stot1860/counties2000$totpop1860
counties2000$pslave1850 <- counties2000$stot1850/counties2000$totpop1850
counties2000$pslave1840 <- counties2000$stot1840/counties2000$totpop1840
counties2000$pdem1856.int <- counties2000$demvotes1856/counties2000$totalvotes1856
summary(counties2000[, c("fvalpc1860", "fvalpac1860", "pslave1860", "wmtot1860", "fbprop1860", "methprop1860", "sfarmprop1860", "land.ineq1860", "rail1860", "water1860", "pdem1856.int")])

counties2000.df <- as(counties2000, "data.frame")
#save(list = c("counties2000.df", "counties2000", "counties1860"), file = "census1860-interp.RData")

southern.states <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", "Louisiana",
                      "Mississippi", "Missouri", "North Carolina", "South Carolina", "Tennessee", "Texas",
                     "Virginia", "West Virginia")

pslave.bins <- cut(counties2000$pslave1860, breaks = seq(0,1, length = 6))
pslave1850.bins <- cut(counties2000$pslave1850, breaks = seq(0,1, length = 6))
pslave1840.bins <- cut(counties2000$pslave1840, breaks = seq(0,1, length = 6))
sprop.bins <- cut(counties1860$sprop, breaks = seq(0,1, length = 6))
sprop1850.bins <- cut(counties1850$sprop1850, breaks = seq(0,1, length = 6))
mcols <-  c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
leg.text <- c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%")
names(mcols) <- levels(pslave.bins)

#cairo_pdf(file = "figs/pslave2000-map.pdf", width = 7, height = 4, pointsize = 8, family = "Minion Pro")
plot(counties2000[counties2000$STATE_TERR %in% southern.states,], col = mcols[pslave.bins[counties2000$STATE_TERR %in% southern.states]], border = NA)
plot(states2000[states2000$FULL_NAME %in% southern.states,], border = "grey30", add = TRUE, lwd = 0.25)
legend("topleft", horiz = FALSE, col = mcols, legend =  leg.text, bty = "n", fill = mcols, border = "white")
#dev.off()


#cairo_pdf(file = "figs/pslave1860-map.pdf", width = 7, height = 4, pointsize = 8)
plot(counties1860[counties1860$STATE_TERR %in% southern.states,], col = mcols[sprop.bins[counties1860$STATE_TERR %in% southern.states]], border = NA)
plot(states2000[states2000$FULL_NAME %in% southern.states,], border = "grey30", lwd = 0.25, add = TRUE)
legend("topleft", horiz = FALSE, col = mcols, legend =  leg.text, bty = "n", fill = mcols, border = "white")
#dev.off()

#cairo_pdf(file = "figs/pslave1850-2000-map.pdf", width = 7, height = 4, pointsize = 8, family = "Minion Pro")
plot(counties2000[counties2000$STATE_TERR %in% southern.states,], col = mcols[pslave1850.bins[counties2000$STATE_TERR %in% southern.states]], border = NA)
plot(states2000[states2000$FULL_NAME %in% southern.states,], border = "grey30", add = TRUE, lwd = 0.25)
legend("topleft", horiz = FALSE, col = mcols, legend =  leg.text, bty = "n", fill = mcols, border = "white")
#dev.off()

#cairo_pdf(file = "figs/pslave1840-2000-map.pdf", width = 7, height = 4, pointsize = 8, family = "Minion Pro")
plot(counties2000[counties2000$STATE_TERR %in% southern.states,], col = mcols[pslave1840.bins[counties2000$STATE_TERR %in% southern.states]], border = NA)
plot(states2000[states2000$FULL_NAME %in% southern.states,], border = "grey30", add = TRUE, lwd = 0.25)
legend("topleft", horiz = FALSE, col = mcols, legend =  leg.text, bty = "n", fill = mcols, border = "white")
#dev.off()
