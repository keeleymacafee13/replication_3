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

## The following data, which is required to produce the graph, is here:
## http://publications.newberry.org/ahcbp/
histstates <- readOGR("/Users/mblackwell/workland/data/US_AtlasHCB_StateTerr_Gen001/US_HistStateTerr_Gen001_Shapefile", layer = "US_HistStateTerr_Gen001")

ss2000 <- which(histstates$START_N <= 20001231 & histstates$END_N >= 20001231)
states2000 <- histstates[ss2000,]
states2000 <- states2000[!(states2000$ABBR_NAME %in% c("AK", "HI")),]

load("census1860-interp.RData")
southern.states <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", "Louisiana",
                      "Mississippi", "Missouri", "North Carolina", "South Carolina", "Tennessee", "Texas",
                     "Virginia", "West Virginia")

counties1860 <- merge(counties1860, census1860, by = "fips")
counties1850 <- merge(counties1850, census1850, by = "fips")

pslave.bins <- cut(counties2000$pslave1860, breaks = seq(0,1, length = 6))
pslave1850.bins <- cut(counties2000$pslave1850, breaks = seq(0,1, length = 6))
pslave1840.bins <- cut(counties2000$pslave1840, breaks = seq(0,1, length = 6))
mcols <-  c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
leg.text <- c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%")
names(mcols) <- levels(pslave.bins)

#cairo_pdf(file = "figs/pslave2000-map.pdf", width = 7, height = 4, pointsize = 8, family = "Minion Pro")
plot(counties2000[counties2000$STATE_TERR %in% southern.states,], col = mcols[pslave.bins[counties2000$STATE_TERR %in% southern.states]], border = NA)
plot(states2000[states2000$FULL_NAME %in% southern.states,], border = "grey30", add = TRUE, lwd = 0.25)
legend("topleft", horiz = FALSE, col = mcols, legend =  leg.text, bty = "n", fill = mcols, border = "white")
#dev.off()



countydata <- read.csv("abs-jop-countydata.csv", stringsAsFactors = FALSE)

cotton.bins <- cut(countydata$cottonsuit, breaks = quantile(countydata$cottonsuit, c(0, 0.4, 0.5, 0.6, 0.7, 0.8, 1), na.rm=TRUE))
mcols <- brewer.pal(6, "Greens")
#leg.text <- c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%")
names(mcols) <- levels(cotton.bins)

tmp.data <- data.frame(fips = countydata$fips, cotton.cols = as.character(mcols[cotton.bins]))
mp <- map("county", plot=FALSE,namesonly=TRUE)
tmp.data <- merge(county.fips, tmp.data, by = "fips", all.x = TRUE)
rownames(tmp.data) <- tmp.data$polyname

#cairo_pdf(file = "figs/cotton-suit-map.pdf", width = 7, height = 4, pointsize = 8, family = "Minion Pro")
map("county", fill = TRUE, col = as.character(tmp.data[mp, "cotton.cols"]),resolution = 0,lty = 0,projection = "polyconic")
#points(x = -92, y = 34, pch = 19, cex = 2)
map("state",col = "black",fill=FALSE,add=TRUE,lty=1,lwd=1,projection="polyconic")
legend("bottomleft", legend = c("Least Suitable", NA, NA, NA, NA, "Most Suitable"), fill = mcols, bty = "o", border = NA, y.intersp = 0.5, bg = grey(0.8), box.lwd = 0)
#dev.off()
