library(hexbin)
outwidths <- c(5, -1, 5, -1, 6)
outflows2000 <- read.fwf("outtxt_flow.txt", widths = outwidths, col.names = c("fips.start", "fips.end", "outflow"))

countydata <- read.csv("abs-jop-countydata.csv", stringsAsFactors = FALSE)

outflows2000 <- merge(outflows2000, countydata[,c("fips", "pslave1860", "totpop00")], by.x = "fips.start", by.y = "fips")

outflows2000 <- merge(outflows2000, countydata[,c("fips", "pslave1860", "totpop00")], by.x = "fips.end", by.y = "fips", suffixes = c(".start", ".end"))

outflows2000$outflow.pc <- outflows2000$outflow/outflows2000$totpop00.start


outflows2000$pslave1860.start[is.na(outflows2000$pslave1860.start)] <- 0
outflows2000$pslave1860.end[is.na(outflows2000$pslave1860.end)] <- 0
outflows2000$pslave1860.absdiff <- abs(outflows2000$pslave1860.start - outflows2000$pslave1860.end)

flist <- unique(outflows2000$fips.end)
pslave.inflow <- rep(NA, length(flist))
for (i in 1:length(flist)) {
  this.co <- outflows2000[which(outflows2000$fips.end == flist[i]),]
  w <- this.co$outflow/sum(this.co$outflow)
  pslave.inflow[i] <- sum(w * this.co$pslave1860.start)
}

pslave.inflow <- data.frame(fips = flist, pslave.inflow = pslave.inflow)

## Figure A.3
#cairo_pdf(file= "figs/outflows.pdf", width = 6, height = 4, pointsize = 10, family = "Minion Pro")
hb1 <- with(subset(outflows2000, pslave1860.start > 0), hexbin(x = pslave1860.absdiff, outflow.pc, xbins = 50))
#rid::grid.newpage()
vp <- grid::viewport(width=0.7, height=0.7, xscale = c(-0.05,1), yscale=c(-0.01, 0.25))
grid::pushViewport(vp)
grid::grid.xaxis()
grid::grid.yaxis()
grid::grid.text("Absolute difference in proportion slave between counties", x = 0.5, y = -0.15, gp = grid::gpar(fontsize = 10))
grid::grid.text("Prop. Outflow, 1995-2000", x = -0.15, y = 0.5, gp = grid::gpar(fontsize = 10), rot = 90)
grid.hexagons(hb1)
grid::popViewport()
#dev.off()
