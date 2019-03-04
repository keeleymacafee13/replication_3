library(foreign)
library(data.table)
library(maps)
library(latticeExtra)
data(state.fips)
data(county.fips)
dodgerblue.30 <- rgb(30, 144, 255, 76.5, max =255)
indianred.30 <- rgb(205, 92, 92, 76.5, max =255)
indianred.75 <- rgb(205, 92, 92, 191, max =255)


## data from: Three Generations Combined, 1965-1997 (ICPSR 4532)
## http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/4532
yps <- read.dta("04532-0001-Data.dta", convert.factors = FALSE)
yps$psu65 <- as.numeric(strtrim(yps$v7, width = nchar(yps$v7) - 1))
yps$psu73 <- yps$v265
yps$psu82 <- yps$v3002
south <- data.table(yps[which(yps$psu65 >= 300 & yps$psu65 < 600),])

table(south[psu65 == 567, psu73])
table(south[psu65 == 567, psu82])
psu.fips <- c("375" = 13121, "537" = 5019, "597" = 37053,
              "395" = 12025, "547" = 22035, "527" = 48143,
              "475" = 21067, "365" = 48201, "577" = 47081,
              "385" = 21111, "517" = 5093, "445" = 1101,
              "567" = 21177, "465" = 12095, "497" = 37147,
              "425" = 5119, "507" = 51155, "557" = 28121,
              "455" = 45079, "415" = 51159, "487" = 12115,
              "435" = 48441, "587" = 37189, "313" = 11001)
psu82.fips <- c("140" = 48441, "150" = 22001, "170" = 13121,
                "210" = 5019, "220" = 45079, "240" = 37053,
                "265" = 48143, "328" = 47081, "330" = 48201,
                "359" = 21067, "370" = 5119, "390" = 21111,
                "410" = 12025, "430" = 5093, "440" = 1101,
                "450" = 21177, "470" = 12095, "490" = 37147,
                "507" = 51155, "520" = 51159, "560" = 12115,
                "590" = 28121, "700" = 37189)
south$fips <- psu.fips[as.character(south$psu65)]
south$fips73 <- psu.fips[as.character(south$psu73)]
south$fips82 <- psu82.fips[as.character(south$psu82)]
south <- south[!is.na(fips)]
south[, dem65 := 1 * (v84 < 14)]
south[, rep65 := 1 * (v84 %in% 15:17)]
south[, mo_dem := 1 * (v117 < 14)]
south[, mo_rep := 1 * (v117 %in% 15:17)]
south[, fa_dem := 1 * (v113 < 14)]
south[, fa_rep := 1 * (v113 %in% 15:17)]
south[, dem73 := 1 * (v482 < 3)]
south[, rep73 := 1 * (v482 %in% 4:6)]
south[, dem97 := 1 * (v5754 < 3)]
south[, rep97 := 1 * (v5754 %in% 4:6)]
south[, ideo73 := v381]
is.na(south$ideo73) <- south$ideo73 %in% c(0, 8, 9)
south[, ideo82 := v1304]
is.na(south$ideo82) <- south$ideo82 %in% c(0, 8, 9)
south[, ideo97 := v5300]
is.na(south$ideo97) <- south$ideo97 > 10
south[, btherm65 := v261]
is.na(south$btherm65) <- south$btherm65 == 99
south[, btherm73 := v775]
is.na(south$btherm73) <- south$btherm73 == 99
south[, btherm82 := v4019]
is.na(south$btherm82) <- south$btherm82 == 99
south[, btherm97 := v6726]
is.na(south$btherm97) <- south$btherm97 == 99
south[, wtherm65 := v259]
is.na(south$wtherm65) <- south$wtherm65 == 99
south[, wtherm73 := v773]
is.na(south$wtherm73) <- south$wtherm73 == 99
south[, wtherm82 := v4018]
is.na(south$wtherm82) <- south$wtherm82 == 99
south[, wtherm97 := v6725]
is.na(south$wtherm97) <- south$wtherm97 == 99
south[, sotherm65 := v128]
is.na(south$sotherm65) <- south$sotherm65 %in% c(1, 2, 3)
south[, sotherm65 := v128]
is.na(south$sotherm65) <- south$sotherm65 %in% c(1, 2, 3)
south[, sotherm73 := v427]
is.na(south$sotherm73) <- south$sotherm73 %in% c(98, 99)
south[, sotherm97 := v5613]
is.na(south$sotherm97) <- south$sotherm97 > 100
south[, btherm65.pa := g443]
south[, wtherm65.pa := g441]
south[, white := 1 * (v232 == 1)]
south[, black := 1 * (v232 == 2)]
south <- south[white == 1]

south[fips != fips73, .N]
south[fips != fips82, .N]

countydata <- read.csv("abs-jop-countydata.csv", stringsAsFactors = FALSE)
south.comb <- merge(as.data.frame(south), countydata, by = "fips", all.x = TRUE)
south.comb <- merge(south.comb, countydata, by.x = "fips73", by.y = "fips",
                    all.x = TRUE, suffixes = c(".65", ".73"))
south.comb <- merge(south.comb, countydata[, c("pslave1860", "fips")], by.x = "fips82",
                    by.y = "fips", all.x = TRUE)
names(south.comb)[names(south.comb) == "pslave1860"] <- "pslave.82"
south.comb <- data.table(south.comb)

south.comb$pslave.cat <- NA
south.comb$pslave.cat[is.na(south.comb$pslave1860.65)] <- 0
south.comb$pslave.cat[which(south.comb$pslave1860.65 < 0.30)] <- 1
south.comb$pslave.cat[which(south.comb$pslave1860.65 >= 0.30)] <- 2

ypc.out65 <- cor.test(south.comb$wtherm65.pa-south.comb$btherm65.pa, south.comb$wtherm65-south.comb$btherm65)
ypc.out73 <- cor.test(south.comb$wtherm65.pa-south.comb$btherm65.pa, south.comb$wtherm73-south.comb$btherm73)
ypc.out82 <- cor.test(south.comb$wtherm65.pa-south.comb$btherm65.pa, south.comb$wtherm82-south.comb$btherm82)
ypc.out97 <- cor.test(south.comb$wtherm65.pa-south.comb$btherm65.pa, south.comb$wtherm97-south.comb$btherm97)
ypc.mods <- list(ypc.out65, ypc.out73, ypc.out82, ypc.out97)
ypc.corrs <- sapply(ypc.mods, function(x) x$estimate)
ypc.cis <- sapply(ypc.mods, function(x) x$conf.int)
ypc.ps <- sapply(ypc.mods, function(x) x$p.value)

##cairo_pdf(filename = "figs/youth-parent-correlations.pdf", family = "Minion Pro", height = 4.5, width = 5.5, pointsize = 9)
yrs <- c(1965, 1973, 1982, 1997)
plot(x = NULL, y = NULL, xlim = c(1963, 1999), ylim = c(-0.5, 1), xlab = "Year", ylab = "Correlation with Parent's Black-White Therm. Scores", xaxt = "n", bty = "n")
axis(side = 1, at = yrs)
abline(h = 0, lty = 2)
points(x = yrs, y = ypc.corrs, col = "dodgerblue", pch = 19)
segments(x0 = yrs, y0 = ypc.cis[1,], y1 = ypc.cis[2,], col = "dodgerblue", lwd = 2)
##dev.off()
