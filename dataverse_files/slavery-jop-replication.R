## slavery-jop-replication.R - code to generate results for slavery paper
## 2016-01-11 - created file that replicates JOP R&R submission


library(foreign)
library(plyr)
library(reshape)
library(sandwich)
library(maps)
library(stargazer)
library(AER)
library(Formula)
library(lme4)
library(cem)
library(latticeExtra)
library(stringr)
source("panel-utils.R")
data(state.fips)
state.fips <- unique(state.fips[,c("fips","abb")])
state.fips$abb <- as.character(state.fips$abb)
state.fips <- rbind(state.fips, c(2, "AK"))
state.fips <- rbind(state.fips, c(15, "HI"))
rownames(state.fips) <- state.fips$abb
fips.state <- state.fips
rownames(fips.state) <- fips.state$fips
data(county.fips)
dodgerblue.30 <- rgb(30, 144, 255, 76.5, max =255)
indianred.30 <- rgb(205, 92, 92, 76.5, max =255)
indianred.75 <- rgb(205, 92, 92, 191, max =255)


## http://people.su.se/~ma/clustering.pdf
robust.se <- function(fm, clvar){
    # R-codes (www.r-project.org) for computing
    # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
    # The arguments of the function are:
    # fitted model, cluster1 and cluster2
    # You need to install libraries `sandwich' and `lmtest'
  library(sandwich);library(lmtest);
  x <- eval(fm$call$data, envir = parent.frame())
  if ("polr" %in% class(fm)) {
    require(MASS)
    cluster <- x[rownames(predict(fm, type = "probs")), clvar]
  } else {
    cluster <- x[names(predict(fm)), clvar]
  }
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- dim(vcov(fm))[1]
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

ch.row <- function(name, yesno) {
    c(name, ifelse(yesno, "$\\checkmark$", ""))
}


countydata <- read.csv("abs-jop-countydata.csv", stringsAsFactors = FALSE)
wh.counties <- read.csv("abs-jop-cces-white-countydata.csv", stringsAsFactors = FALSE)
cces.comb <- read.csv("abs-jop-cces-ind.csv", stringsAsFactors = FALSE)

st.list <- c("AL", "AR", "GA", "FL", "KY", "LA", "MS", "MO", "NC", "SC", "TN", "TX", "VA","WV")
cces.comb$abs.sample <- 1 * (cces.comb$state.abb %in% st.list)
wh.counties$abs.sample <- 1 * (wh.counties$state.abb %in% st.list)
countydata$abs.sample <- 1 * (countydata$state.abb %in% st.list)
wh.counties$tractor.growth <- (wh.counties$tractors40 - wh.counties$tractors30)

cces.comb$inc.cat <- factor(cces.comb$inc.cat, levels = c("<20k", "20-50k", "50-100k", "100-150k", "150k+"))
whites <- cces.comb[which(cces.comb$white == 1),]
blacks <- cces.comb[which(cces.comb$black == 1),]
latinos <- cces.comb[which(cces.comb$latino == 1),]
others <- cces.comb[which(cces.comb$white != 1 & cces.comb$black != 1 & cces.comb$latino != 1),]

dim(cces.comb)
dim(whites)
sum(whites$state.abb %in% st.list)

dim(blacks)
sum(blacks$state.abb %in% st.list)

dim(latinos)
sum(latinos$state.abb %in% st.list)


## Individual-level data
southerners <- subset(cces.comb, abs.sample == 1)
s.whites <- subset(whites, abs.sample == 1)
s.blacks <- subset(blacks, abs.sample == 1)
s.latinos <- subset(latinos, abs.sample == 1)
s.whites$state.abb <- factor(s.whites$state.abb)
s.blacks$state.abb <- factor(s.blacks$state.abb)
s.latinos$state.abb <- factor(s.latinos$state.abb)

## County-level data
south.counties <- subset(wh.counties, abs.sample == 1)
south.counties$state.abb <- factor(south.counties$state.abb)
south.counties <- south.counties[order(as.numeric(south.counties$fips)),]

nrow(s.whites)
sum(countydata$state.abb %in% st.list)
dim(south.counties)

## NES Results
nes.counties <- read.csv("abs-jop-nes-white-countydata.csv", stringsAsFactors = FALSE)
nes.counties$abs.sample <- 1 * (nes.counties$state.abb %in% st.list)
nes.comb <- read.csv("abs-jop-nes-ind.csv", stringsAsFactors = FALSE)
nes.comb$abs.sample <- 1 * (nes.comb$state.abb %in% st.list)
nes.whites <- nes.comb[which(nes.comb$white == 1),]
nes.blacks <- nes.comb[which(nes.comb$black == 1),]

## Individual-level analysis
ns.whites <- subset(nes.whites, abs.sample == 1)
ns.blacks <- subset(nes.blacks, abs.sample == 1)
ns.whites$state.abb <- factor(ns.whites$state.abb)
ns.blacks$state.abb <- factor(ns.blacks$state.abb)

dim(nes.whites)
dim(ns.whites)
dim(nes.blacks)
dim(ns.blacks)
sum(nes.counties$state.abb %in% st.list)


base1860.form <- formula(. ~ pslave1860 + log(coarea00) + latitude + I(latitude^2) + longitude + I(longitude^2)+ rugged  + land.ineq1860 + sfarmprop1860 + log(totpop1860) + log(fvalpac1860) + log(acimp1860) + fbprop1860  + rail1860 + water1860 + state.abb)

ind.form <- formula(. ~ pslave1860   + log(coarea00) + latitude + I(latitude^2) + longitude + I(longitude^2) + rugged + land.ineq1860 + sfarmprop1860 + log(totpop1860) + log(fvalpac1860) + log(acimp1860) + fbprop1860 + rail1860 + water1860 + as.factor(educ) +  inc.cat +religion + female + age + state.abb*as.factor(year))

ind.int.form <- formula(. ~ pslave1860   + log(coarea00) + latitude + I(latitude^2) + longitude + I(longitude^2) + rugged + land.ineq1860 + sfarmprop1860 + log(totpop1860) + log(fvalpac1860) + log(acimp1860) + fbprop1860 + rail1860 + water1860 + as.factor(educ)*pslave1860 + inc.cat*pslave1860 + religion*pslave1860 + female*pslave1860 + age*pslave1860  + state.abb*as.factor(year))

context.form <- formula(. ~ pslave1860   + log(coarea00) + latitude + I(latitude^2) + longitude + I(longitude^2) + rugged + land.ineq1860 + sfarmprop1860 + log(totpop1860) + log(fvalpac1860) + log(acimp1860) + fbprop1860 + rail1860 + water1860 + as.factor(educ) + inc.cat  +religion +female + age + blkprop.z00 + log(medinc.z10) + w.unemp.rate2014 + log(wbincratio2014) + state.abb*as.factor(year))
context.int.form <- formula(. ~ pslave1860   + log(coarea00) + latitude + I(latitude^2) + longitude + I(longitude^2) + rugged + land.ineq1860 + sfarmprop1860 + log(totpop1860) + log(fvalpac1860) + log(acimp1860) + fbprop1860 + rail1860 + water1860 + as.factor(educ) +  inc.cat  +religion +female + age + blkprop.z00*pslave1860 + log(medinc.z10)*pslave1860 + w.unemp.rate2014*pslave1860 + log(wbincratio2014)*pslave1860 + state.abb*as.factor(year))

## have to use Formula package for ivreg calls
base.iv.form <- Formula(. ~ pslave1860 + log(coarea00) + rugged + latitude + I(latitude^2) + longitude + I(longitude^2)  + water1860  + state.abb | cottonsuit + log(coarea00) + rugged  + latitude + I(latitude^2) + longitude + I(longitude^2) + water1860  + state.abb)

base.first.form <- formula(pslave1860 ~ cottonsuit + log(coarea00) + rugged + latitude + I(latitude^2) + longitude + I(longitude^2)  +water1860 + state.abb)

rform.form <- formula(. ~  cottonsuit + log(coarea00) + rugged + latitude + I(latitude^2)+ longitude + I(longitude^2)  + water1860+  state.abb)



#cairo_pdf(filename = "../figs/scatters.pdf", family = "Minion Pro", height = 5.5, width = 6.5, pointsize = 11)
par(mfrow = c(2,2), mar = 0.1 + c(5, 4, 0, 2))
plot(south.counties$pslave, south.counties$dem, pch = 19, col = "#33333333", xlab = "Proportion Slave, 1860", ylab = "Proportion Democrat", yaxt = "n", cex = south.counties$sample.size/100)
axis(side = 2, las = 2)
abline(lm(dem ~ pslave1860, data = south.counties, weights = sample.size), lwd = 2, col = "#AA0000")
plot(south.counties$pslave1860, south.counties$affirm, pch = 19, col = "#33333333", xlab = "Proportion Slave, 1860", ylab = "Support for Affirmative Action", yaxt = "n", cex = south.counties$sample.size/100)
axis(side = 2, las = 2)
abline(lm(affirm ~ pslave1860, data = south.counties, weights = sample.size), lwd = 2, col = "#AA0000")
plot(south.counties$pslave1860, south.counties$resent, pch = 19, col = "#33333333", xlab = "Proportion Slave, 1860", ylab = "Racial Resentment", yaxt = "n", cex = south.counties$sample.size.res/75)
axis(side = 2, las = 2)
abline(lm(resent ~ pslave1860, data = south.counties, weights = sample.size.res), lwd = 2, col = "#AA0000")
with(subset(nes.counties, state.abb %in% st.list), plot(pslave1860, wtherm-btherm, cex = sample.size.bt/40, pch = 19, col = "#33333333", xlab = "Proportion Slave, 1860", ylab = "White - Black Thermometer Score", las = 1, xlim = c(0, 0.9)))
abline(lm(I(wtherm-btherm) ~ pslave1860, weights = sample.size.bt, data = subset(nes.counties, state.abb %in% st.list)), lwd = 2, col = "#AA0000")
#dev.off()

#cairo_pdf(filename = "../figs/scatters-alt.pdf", family = "Minion Pro", height = 2.5, width = 7, pointsize = 11)
par(mfrow = c(1,4), mar = 0.1 + c(4, 3, 2, 0.5), cex.main = 1)
plot(south.counties$pslave1860, south.counties$dem, pch = 19, col = "#33333333", xlab = "Proportion Slave, 1860", ylab = "", main = "Proportion Democrat", yaxt = "n", cex = south.counties$sample.size/100)
axis(side = 2, las = 2)
abline(lm(dem ~ pslave1860, data = south.counties, weights = sample.size), lwd = 2, col = "#AA0000")
plot(south.counties$pslave1860, south.counties$affirm, pch = 19, col = "#33333333", xlab = "Proportion Slave, 1860", ylab = "", main = "Affirmative Action", yaxt = "n", cex = south.counties$sample.size/100)
axis(side = 2, las = 2)
abline(lm(affirm ~ pslave1860, data = south.counties, weights = sample.size), lwd = 2, col = "#AA0000")
plot(south.counties$pslave1860, south.counties$resent, pch = 19, col = "#33333333", xlab = "Proportion Slave, 1860", ylab="", main = "Racial Resentment", yaxt = "n", cex = south.counties$sample.size.res/75)
axis(side = 2, las = 2)
abline(lm(resent ~ pslave1860, data = south.counties, weights = sample.size.res), lwd = 2, col = "#AA0000")
with(subset(nes.counties, state.abb %in% st.list), plot(pslave1860, wtherm-btherm, cex = sample.size.bt/40, pch = 19, col = "#33333333", xlab = "Proportion Slave, 1860", ylab = "", main = "White - Black Therm. Score", las = 1, xlim = c(0, 0.9)))
abline(lm(I(wtherm-btherm) ~ pslave1860, weights = sample.size.bt, data = subset(nes.counties, state.abb %in% st.list)), lwd = 2, col = "#AA0000")
#dev.off()


## Tables

cnty.res <- lm(dem ~ pslave1860, data = south.counties, weights = sample.size)
summary(cnty.res)

cnty.res.fe <- lm(dem ~ pslave1860 + state.abb, data = south.counties, weights = sample.size)
summary(cnty.res.fe)

cnty.res.full <- lm(update(base1860.form, dem ~ .), data = south.counties, weights = sample.size)
summary(cnty.res.full)

cnty.aff <- lm(affirm ~ pslave1860, data = south.counties, weights = sample.size)
summary(cnty.aff)

cnty.aff.fe <- lm(affirm ~ pslave1860 + state.abb, data = south.counties, weights = sample.size)
summary(cnty.aff.fe)

cnty.aff.full <- lm(update(base1860.form, affirm ~ .), data = south.counties, weights = sample.size)
summary(cnty.aff.full)


cnty.resent <- lm(resent ~ pslave1860, data = south.counties, weights = sample.size.res)
summary(cnty.resent)

cnty.resent.fe <- lm(resent ~ pslave1860 +  state.abb, data = south.counties, weights = sample.size.res)
summary(cnty.resent.fe)

cnty.resent.full <- lm(update(base1860.form, resent ~ .), data = south.counties, weights = sample.size.res)
summary(cnty.resent.full)

## NES Individual Results
therm.mod <- lm(therm.diff ~ pslave1860, data = ns.whites, weights = weight)
therm.mod.rse <- robust.se(therm.mod, clvar = "fips")
therm.mod.rse

therm.mod.fe <- lm(therm.diff ~ pslave1860 + state.abb*as.factor(year), data = ns.whites, weights = weight)
therm.mod.fe.rse <- robust.se(therm.mod.fe, clvar = "fips")
therm.mod.fe.rse

therm.1860 <- lm(update(base1860.form, therm.diff ~ . + state.abb*as.factor(year)), data = ns.whites, weights = weight)
therm.1860.rse <- robust.se(therm.1860, clvar = "fips")
therm.1860.rse


## for comparison with coefficient
0.2*abs(coef(cnty.res.full)["pslave1860"])/sd(south.counties$dem, na.rm = TRUE)
0.2*abs(coef(cnty.aff.full)["pslave1860"])/sd(south.counties$affirm, na.rm = TRUE)
0.2*coef(cnty.resent.full)["pslave1860"]/sd(south.counties$resent, na.rm = TRUE)
0.2*coef(therm.1860)["pslave1860"]/sd(ns.whites$therm.diff, na.rm = TRUE)

## Table 1
tab1 <- stargazer(cnty.res, cnty.res.full, cnty.aff.full, cnty.resent.full,
                  keep = "pslave1860", style = "apsr", omit.stat = c("adj.rsq","ll", "F", "ser"),
                  covariate.labels = c("Prop. Slave, 1860"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 4), ch.row("State Fixed Effects", c(FALSE, rep(TRUE,3))), ch.row("1860 Covariates", c(FALSE, rep(TRUE,3))), rep("", 4)), multicolumn = TRUE)
tab1[4] <- "\\\\[-1.8ex] & \\multicolumn{2}{c}{Prop. Democrat} & Affirm. Action & Racial Resentment \\\\ "
tab1 <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", tab1)
tab1 <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", tab1)
tab1 <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", tab1)
tab1 <- tab1[-(length(tab1)-1)]
cat(paste(tab1, collapse = "\n"), "\n")

tab1.alt <- stargazer(cnty.res, cnty.res.full, cnty.aff.full, cnty.resent.full, therm.1860,
                      se = list(NULL, NULL, NULL, NULL, therm.1860.rse[,2]),
                  keep = "pslave1860", style = "apsr", omit.stat = c("adj.rsq","ll", "F", "ser"),
                  covariate.labels = c("Prop. Slave, 1860"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 4), c("Level", "County", "County", "County", "County", "Individual"), ch.row("1860 Covariates", c(FALSE, rep(TRUE,3), FALSE)), ch.row("State Fixed Effects", c(FALSE, rep(TRUE,4))), ch.row("State-Year Fixed Effects", c(rep(FALSE,4), TRUE)), ch.row("Clustered SEs", c(rep(FALSE,4), TRUE)), rep("", 4)), multicolumn = TRUE)
tab1.alt[4] <- "\\\\[-1.8ex] & \\multicolumn{2}{c}{Proportion} & Support for & Racial  & White-Black  \\\\ "
tab1.alt <- append(tab1.alt, " & \\multicolumn{2}{c}{Democrat} & Affirm. Action &  Resentment & Therm. Diff  \\\\ ", after = 4)
tab1.alt <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", tab1.alt)
tab1.alt <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", tab1.alt)
tab1.alt <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", tab1.alt)
tab1.alt <- tab1.alt[-(length(tab1.alt)-1)]
cat(paste(tab1.alt, collapse = "\n"), "\n")
##cat(paste(tab1.alt, collapse = "\n"), "\n", file = "../tables/main-results.tex")




## Instrumental Variables
cnty.iv <- ivreg(update(base.iv.form, dem ~ .), data = south.counties, weights = sample.size)
summary(cnty.iv)

cnty.aff.iv <- ivreg(update(base.iv.form, affirm  ~ .), data = south.counties, weights = sample.size)
summary(cnty.aff.iv)

cnty.resent.iv <- ivreg(update(base.iv.form, resent  ~ .), data = south.counties, weights = sample.size.res)
summary(cnty.resent.iv)

therm.cty.iv <- ivreg(update(base.iv.form, therm.diff ~ .), data = nes.counties, weights = sample.size.td, subset = abs.sample == 1)
summary(therm.cty.iv)

cnty.res.first <- lm(base.first.form, data = south.counties)
summary(cnty.res.first)


iv.tab <- stargazer(cnty.res.first, cnty.iv, cnty.aff.iv, cnty.resent.iv,
          keep = c("cotton", "pslave1860"), style = "apsr", omit.stat = c("ll", "rsq", "adj.rsq", "ser"),
                    covariate.labels = c("Cotton Suitability", "Prop. Slave, 1860"), dep.var.labels = c("Prop Slave", "Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 5), ch.row("State Fixed Effects", rep(TRUE,4)), ch.row("Geographic Controls", rep(TRUE,4)), c("Model", rep("2SLS", 4)), c("", "First Stage", rep("Second Stage", 3)), rep("", 5)), multicolumn = TRUE, model.names = FALSE)
iv.tab[4] <- "\\\\[-1.8ex] & Prop. Slave & Prop. Democrat & Affirm. Action & Racial Resentment \\\\ "
iv.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", iv.tab)
iv.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", iv.tab)
iv.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", iv.tab)
iv.tab <- iv.tab[-(length(iv.tab)-1)]
cat(paste(iv.tab, collapse = "\n"), "\n")

iv.tab.alt <- stargazer(cnty.res.first, cnty.iv, cnty.aff.iv, cnty.resent.iv, therm.cty.iv,
          keep = c("cotton", "pslave1860"), style = "apsr", omit.stat = c("ll", "rsq", "adj.rsq", "ser"),
                    covariate.labels = c("Cotton Suitability", "Prop. Slave, 1860"), dep.var.labels = c("Prop Slave", "Prop Democrat", "Affirm. Action", "Racial Resentment", "BW"), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 5), ch.row("State Fixed Effects", rep(TRUE,5)), ch.row("Geographic Controls", rep(TRUE,5)), c("Model", rep("2SLS", 5)), c("", "First Stage", rep("Second Stage", 4)), rep("", 5)), multicolumn = FALSE, model.names = FALSE)
iv.tab.alt[4] <- "\\\\[-1.8ex] & Proportion & Proportion & Support for & Racial  & White-Black  \\\\ "
iv.tab.alt <- append(iv.tab.alt, " & Slave, 1860 & Democrat & Affirm. Action &  Resentment & Therm. Diff  \\\\ ", after = 4)
iv.tab.alt <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", iv.tab.alt)
iv.tab.alt <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", iv.tab.alt)
iv.tab.alt <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", iv.tab.alt)
iv.tab.alt <- iv.tab.alt[-(length(iv.tab.alt)-1)]
cat(paste(iv.tab.alt, collapse = "\n"), "\n")
##cat(paste(iv.tab.alt, collapse = "\n"), "\n", file = "../tables/iv-table.tex")

### Reduced form check in the north


cnty.dem.rform <- lm(update(rform.form, dem ~ .), data = south.counties, weights = sample.size)
summary(cnty.dem.rform)

cnty.dem.ns.rform <- lm(update(rform.form, dem ~ .), data = wh.counties, weights = sample.size, subset = !(state.abb %in% st.list) & !(state.abb %in% c("MD", "DE","KY", "MO", "WV")))
summary(cnty.dem.ns.rform)

cnty.aff.rform <- lm(update(rform.form, affirm ~ .), data = south.counties, weights = sample.size)
summary(cnty.aff.rform)

cnty.aff.ns.rform <- lm(update(rform.form, affirm ~ .), data = wh.counties, weights = sample.size, subset = !(state.abb %in% st.list) & !(state.abb %in% c("MD", "DE","KY", "MO", "WV")))
summary(cnty.aff.ns.rform)

cnty.resent.rform <- lm(update(rform.form, resent ~ .), data = south.counties, weights = sample.size.res)
summary(cnty.resent.rform)

cnty.resent.ns.rform <- lm(update(rform.form, resent ~ .), data = wh.counties, weights = sample.size.res, subset = !(state.abb %in% st.list) & !(state.abb %in% c("MD", "DE","KY","MO", "WV")))
summary(cnty.resent.ns.rform)

cnty.therm.rform <- lm(update(rform.form, therm.diff ~ .), data = nes.counties, weights = sample.size.td, subset = abs.sample == 1)
summary(cnty.therm.rform)

cnty.therm.ns.rform <- lm(update(rform.form, therm.diff ~ .), data = nes.counties, weights = sample.size.td, subset = abs.sample == 0 &  !(state.abb %in% c("MD", "DE","KY","MO", "WV")))
summary(cnty.therm.ns.rform)

rform.tab <- stargazer(cnty.dem.rform, cnty.dem.ns.rform, cnty.aff.rform, cnty.aff.ns.rform, cnty.resent.rform, cnty.resent.ns.rform,
          keep = "cotton", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("FAO Cotton Suitability"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"),
          title = "Reduced form of the instrumental variables estimates for the South (columns 1, 3, and 5) and the North (columns 2, 4, and 6).", label = "t:falsification", column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 7), ch.row("State Fixed Effects", rep(TRUE,6)), ch.row("Geographic Controls", rep(TRUE,6)), rep("", 7)), multicolumn = TRUE)
rform.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{2}{c}{Prop. Democrat} & \\multicolumn{2}{c}{Affirm. Action} & \\multicolumn{2}{c}{Racial Resentment} \\\\ "
rform.tab <- append(rform.tab, "\\\\[-1.8ex] & South & Non-South & South & Non-South & South & Non-South \\\\ ", after = 5)
rform.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", rform.tab)
rform.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", rform.tab)
rform.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", rform.tab)
rform.tab <- rform.tab[-(length(rform.tab)-1)]
cat(paste(rform.tab, collapse = "\n"), "\n")
##cat(paste(rform.tab, collapse = "\n"), "\n", file = "../tables/reduced-form.tex")


## NES Results County
therm.cty.mod <- lm(therm.diff ~ pslave1860, data = nes.counties, weights = sample.size.td, subset = abs.sample == 1)
summary(therm.cty.mod)

therm.cty.mod.fe <- lm(therm.diff ~ pslave1860 + state.abb, data = nes.counties, weights = sample.size.td, subset = abs.sample == 1)
summary(therm.cty.mod.fe)

therm.cty.1860 <- lm(update(base1860.form, therm.diff ~ .), data = nes.counties, weights = sample.size.td, subset = abs.sample == 1)
summary(therm.cty.1860)

therm.cty.iv <- ivreg(update(base.iv.form, therm.diff ~ .), data = nes.counties, weights = sample.size.td, subset = abs.sample == 1)
summary(therm.cty.iv)


therm.ind <- lm(update(base1860.form, I(wtherm-btherm) ~ .  + as.factor(year)*state.abb + female + age + I(age^2) + inc.mid + as.factor(educ) + religion), data = ns.whites, weights = weight)
therm.ind.rse <- robust.se(therm.ind, clvar = "fips")
therm.ind.rse

## LA, NC, AR need to be dropped due to lack of state-year variation
## results the same with separate state and year fixed effects
therm.iv <- ivreg(update(base.iv.form, I(wtherm-btherm) ~ . + as.factor(year)*state.abb | . + as.factor(year)*state.abb), data = ns.whites, weights = weight, subset = !(state.abb %in% c("LA", "NC","AR", "WV")))
therm.iv.rse <- robust.se(therm.iv, clvar = "fips")
therm.iv.rse

therm.iv.first <- lm(update(base.first.form, . ~ . + as.factor(year)*state.abb), data = ns.whites, weights = weight, subset = !(state.abb %in% c("LA", "NC","AR", "WV")))
summary(therm.iv.first)

btherm.1860 <- lm(update(base1860.form, btherm ~ . + as.factor(year)*state.abb), data = ns.whites, weights = weight)
btherm.1860.rse <- robust.se(btherm.1860, clvar = "fips")
btherm.1860.rse

## LA, NC, AR need to be dropped due to lack of state-year variation
## results the same with separate state and year fixed effects
btherm.iv <- ivreg(update(base.iv.form, btherm ~ . + as.factor(year)*state.abb | . + as.factor(year)*state.abb), data = ns.whites, weights = weight, subset = !(state.abb %in% c("LA", "NC","AR", "WV")))
btherm.iv.rse <- robust.se(btherm.iv, clvar = "fips")
btherm.iv.rse

wtherm.1860 <- lm(update(base1860.form, wtherm ~ . + as.factor(year)*state.abb), data = ns.whites, weights = weight)
wtherm.1860.rse <- robust.se(wtherm.1860, clvar = "fips")
wtherm.1860.rse

## LA, NC, AR need to be dropped due to lack of state-year variation
## results the same with separate state and year fixed effects
wtherm.iv <- ivreg(update(base.iv.form, wtherm ~ . + as.factor(year)*state.abb | . + as.factor(year)*state.abb), data = ns.whites, weights = weight, subset = !(state.abb %in% c("LA", "NC","AR")))
wtherm.iv.rse <- robust.se(wtherm.iv, clvar = "fips")
wtherm.iv.rse


therm.tab <- stargazer(therm.mod, therm.mod.fe, therm.1860, therm.iv,
          keep = "pslave1860", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860"), dep.var.labels = c("White Thermometer - Black Thermometer (-100 to 100)"), se = list(therm.mod.rse[,2], therm.mod.fe.rse[,2], therm.1860.rse[,2], therm.iv.rse[,2]), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 5), ch.row("Clustered SEs", rep(TRUE,4)), ch.row("State-Year Fixed Effects", c(FALSE, rep(TRUE,3))), ch.row("Geographic Controls", c(rep(FALSE, 2), rep(TRUE,2))), ch.row("1860 Covariates", c(FALSE, FALSE, TRUE, FALSE)), rep("", 7)), multicolumn = TRUE)
therm.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{4}{c}{White Thermometer - Black Thermometer (-100 to 100)} \\\\ "
therm.tab[5] <- "\\\\[-1.8ex] &  \\multicolumn{3}{c}{OLS} & IV \\\\ "
therm.tab <- therm.tab[-6]
therm.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", therm.tab)
therm.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", therm.tab)
therm.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", therm.tab)
therm.tab <- therm.tab[-(length(therm.tab)-1)]
therm.tab <- append(therm.tab, paste("First-stage t-statistic & & & & ", round(summary(therm.iv.first)$coef[2,3], 2), "$^{**}$ \\\\", sep = ""), length(therm.tab)-2)
cat(paste(therm.tab, collapse = "\n"), "\n")
##cat(paste(therm.tab, collapse = "\n"), "\n", file = "../tables/therm-tab.tex")

septherm.tab <- stargazer(btherm.1860, btherm.iv, wtherm.1860, wtherm.iv,
          keep = "pslave1860", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860"), dep.var.labels = c("Black Therm. Scores", "White Therm. Scores"), se = list(btherm.1860.rse[,2], btherm.iv.rse[,2], wtherm.1860.rse[,2], wtherm.iv.rse[,2]), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 5), ch.row("Clustered SEs", rep(TRUE,4)), ch.row("State-Year Fixed Effects", rep(TRUE,4)), ch.row("Geographic Controls", rep(TRUE,4)), ch.row("1860 Covariates", c(TRUE, FALSE, TRUE, FALSE)), rep("", 7)), multicolumn = TRUE)
septherm.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{2}{c}{Black Therm. Scores} &  \\multicolumn{2}{c}{White Therm. Scores} \\\\ "
septherm.tab[5] <- "\\\\[-1.8ex] &  OLS & IV &  OLS & IV \\\\ "
septherm.tab <- septherm.tab[-6]
septherm.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", septherm.tab)
septherm.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", septherm.tab)
septherm.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", septherm.tab)
septherm.tab <- septherm.tab[-(length(septherm.tab)-1)]
cat(paste(septherm.tab, collapse = "\n"), "\n")
##cat(paste(septherm.tab, collapse = "\n"), "\n", file = "../tables/therm-separate.tex")


ctytherm.tab <- stargazer(therm.cty.mod, therm.cty.mod.fe, therm.cty.1860, therm.cty.iv,
          keep = "pslave1860", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860"), dep.var.labels = c("White Thermometer - Black Thermometer (-100 to 100)"), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 5), ch.row("Clustered SEs", rep(TRUE,4)), ch.row("State-Year Fixed Effects", rep(TRUE,4)), ch.row("Geographic Controls", rep(TRUE,4)), ch.row("1860 Covariates", c(TRUE, FALSE, TRUE, FALSE)), rep("", 7)), multicolumn = TRUE)
ctytherm.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{4}{c}{White Thermometer - Black Thermometer (-100 to 100)} \\\\ "
ctytherm.tab[5] <- "\\\\[-1.8ex] &  \\multicolumn{3}{c}{OLS} & IV \\\\ "
ctytherm.tab <- ctytherm.tab[-6]
ctytherm.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", ctytherm.tab)
ctytherm.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", ctytherm.tab)
ctytherm.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", ctytherm.tab)
ctytherm.tab <- ctytherm.tab[-(length(ctytherm.tab)-1)]
cat(paste(ctytherm.tab, collapse = "\n"), "\n")
##cat(paste(ctytherm.tab, collapse = "\n"), "\n", file = "../tables/therm-county.tex")

## Neighbor matching
cnty.res.neighbor <- lm(update(base1860.form, dem ~ .), data = south.counties, weights = sample.size, subset = nmatch.diff.20 == 1)
summary(cnty.res.neighbor)

cnty.aff.neighbor <- lm(update(base1860.form, affirm ~ .), data = south.counties, weights = sample.size, subset = nmatch.diff.20 == 1)
summary(cnty.aff.neighbor)

cnty.resent.neighbor <- lm(update(base1860.form, resent ~ .), data = south.counties, weights = sample.size.res, subset = nmatch.diff.20 == 1)
summary(cnty.resent.neighbor)

## Matching (Non-slave south versus north)
## Note that there will be some slight deviation from the results in
## the paper due to the random nature of k2k matching in this context
mdata <- wh.counties[which(!(wh.counties$state.abb %in% st.list) | wh.counties$pslave < 0.05),]
mdata <- mdata[which(!(mdata$state.abb %in% c("MD", "DE"))), ]
mvars <- c("dem", "affirm", "resent","south", "fvalpc1860", "fbprop1860", "totpop1860", "longitude", "latitude", "coarea00")
mdata$south <- 1 * (mdata$state.abb %in% st.list)
mout <- cem("south", data = mdata[,mvars], drop = c("dem", "affirm", "resent"), k2k = TRUE)

unique(mdata$state.abb[mout$w > 0 & mdata$south == TRUE])
unique(mdata$state.abb[mout$w > 0 & mdata$south == FALSE])

match.dem <- lm(update(base1860.form, dem ~ south + . - pslave1860 - state.abb), data = mdata, weights = sample.size, subset = mout$w > 0)
summary(match.dem)

match.aff <- lm(update(base1860.form, affirm ~ south + . - pslave1860 - state.abb), data = mdata, weights = sample.size, subset = mout$w > 0)
summary(match.aff)

match.resent <- lm(update(base1860.form, resent ~ south + . - pslave1860 - state.abb), data = mdata, weights = sample.size.res, subset = mout$w > 0)
summary(match.resent)

## Table 2
match.tab <- stargazer(cnty.res.neighbor, match.dem, cnty.aff.neighbor, match.aff, cnty.resent.neighbor, match.resent,
          dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), keep = c("pslave1860","south"), covariate.labels = c("Prop. Slave, 1860","Slave State"),
           style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 7), ch.row("State Fixed Effects", rep(c(TRUE,FALSE),3)), ch.row("1860 Covariates", rep(TRUE, 6)), ch.row("50\\% Threshold Match", rep(c(TRUE,FALSE), 3)), ch.row("North-South Match", rep(c(FALSE,TRUE), 3)), rep("", 7)), multicolumn = TRUE)
match.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{2}{c}{Prop. Democrat} & \\multicolumn{2}{c}{Affirm. Action} & \\multicolumn{2}{c}{Racial Resentment} \\\\ "
match.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", match.tab)
match.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", match.tab)
match.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", match.tab)
match.tab <- match.tab[-(length(match.tab)-1)]
cat(paste(match.tab, collapse = "\n"), "\n")
##cat(paste(match.tab, collapse = "\n"), "\n", file = "../tables/matching.tex")


cor(south.counties$pslave1860, south.counties$blkprop00, use = "pairwise")

cnty.res.blk70.ptbias <- lm(update(base1860.form, dem ~ . + blkprop70), data =south.counties, weights = sample.size)
summary(cnty.res.blk70.ptbias)

cnty.res.blk00.ptbias <- lm(update(base1860.form, dem ~ . + blkprop00), data =south.counties, weights = sample.size)
summary(cnty.res.blk00.ptbias)

cnty.aff.blk70.ptbias <- lm(update(base1860.form, affirm ~ . + blkprop70), data = south.counties, weights = sample.size)
summary(cnty.aff.blk70.ptbias)

cnty.aff.blk00.ptbias <- lm(update(base1860.form, affirm ~ . + blkprop00), data = south.counties, weights = sample.size)
summary(cnty.aff.blk00.ptbias)


cnty.resent.blk70.ptbias <- lm(update(base1860.form, resent ~ . + blkprop70), data = south.counties, weights = sample.size.res)
summary(cnty.resent.blk70.ptbias)

cnty.resent.blk00.ptbias <- lm(update(base1860.form, resent ~ . + blkprop00), data = south.counties, weights = sample.size.res)
summary(cnty.resent.blk00.ptbias)


cnty.dem.blk00.first <- lm(update(base1860.form, dem ~ . + blkprop00 +log(totpop00) +highsch90 + unemp + log(medinc00) + wbincratio00), data =south.counties, weights = sample.size)
summary(cnty.dem.blk00.first)

cnty.dem.blk00.iv <- ivreg(update(base.iv.form, I(dem - coef(cnty.dem.blk00.first)["blkprop00"]*blkprop00) ~ .), data = south.counties, weights = sample.size)
summary(cnty.dem.blk00.iv)

cnty.dem.blk00 <- lm(update(base1860.form, I(dem - coef(cnty.dem.blk00.first)["blkprop00"]*(blkprop00)) ~ .), data =south.counties, weights = sample.size)
summary(cnty.dem.blk00)

cnty.aff.blk00.first <- lm(update(base1860.form, affirm ~ . + blkprop00 +log(totpop00) +highsch90 + unemp + log(medinc00) + wbincratio00), data =south.counties, weights = sample.size)
summary(cnty.aff.blk00.first)

cnty.aff.blk00.iv <- ivreg(update(base.iv.form, I(affirm - coef(cnty.aff.blk00.first)["blkprop00"]*blkprop00) ~ .), data = south.counties, weights = sample.size)
summary(cnty.aff.blk00.iv)

cnty.aff.blk00 <- lm(update(base1860.form, I(affirm - coef(cnty.aff.blk00.first)["blkprop00"]*(blkprop00)) ~ .), data =south.counties, weights = sample.size)
summary(cnty.aff.blk00)


cnty.resent.blk00.first <- lm(update(base1860.form, resent ~ . + blkprop00 +log(totpop00) +highsch90 + unemp + log(medinc00) + wbincratio00), data =south.counties, weights = sample.size.res)
summary(cnty.resent.blk00.first)

cnty.resent.blk00.iv <- ivreg(update(base.iv.form, I(resent - coef(cnty.resent.blk00.first)["blkprop00"]*blkprop00) ~ .), data = south.counties, weights = sample.size.res)
summary(cnty.resent.blk00.iv)

cnty.resent.blk00 <- lm(update(base1860.form, I(resent - coef(cnty.resent.blk00.first)["blkprop00"]*(blkprop00)) ~ .), data =south.counties, weights = sample.size.res)
summary(cnty.resent.blk00)



## bootstrap the SEs
set.seed(6251983)
boots <- 1000
cnty.dem.blk00.boots <- rep(NA, times = boots)
cnty.aff.blk00.boots <- rep(NA, times = boots)
cnty.resent.blk00.boots <- rep(NA, times = boots)
for (b in 1:boots) {
  sc.star <- south.counties[sample(1:nrow(south.counties), replace = TRUE),]
  boot.dem.first <- lm(update(base1860.form, dem ~ . + blkprop00 +log(totpop00) +highsch90 + unemp + log(medinc00) + wbincratio00), data =sc.star, weights = sample.size)
  boot.dem  <- lm(update(base1860.form, I(dem - coef(boot.dem.first)["blkprop00"]*(blkprop00)) ~ .), data =sc.star, weights = sample.size)
  cnty.dem.blk00.boots[b] <- coef(boot.dem)["pslave1860"]

  boot.aff.first <- lm(update(base1860.form, affirm ~ . + blkprop00 +log(totpop00) +highsch90 + unemp + log(medinc00) + wbincratio00), data =sc.star, weights = sample.size)
  boot.aff  <- lm(update(base1860.form, I(affirm - coef(boot.aff.first)["blkprop00"]*(blkprop00)) ~ .), data =sc.star, weights = sample.size)
  cnty.aff.blk00.boots[b] <- coef(boot.aff)["pslave1860"]

  boot.resent.first <- lm(update(base1860.form, resent ~ . + blkprop00 +log(totpop00) +highsch90 + unemp + log(medinc00) + wbincratio00), data =sc.star, weights = sample.size.res)
  boot.resent <- lm(update(base1860.form, I(resent - coef(boot.resent.first)["blkprop00"]*(blkprop00)) ~ .), data =sc.star, weights = sample.size.res)
  cnty.resent.blk00.boots[b] <- coef(boot.resent)["pslave1860"]

}

# Put bootstrapped SEs into a holder (we don't calculate the BSE for
# the other coefficients since we don't care about them and don't
# report them in the paper)
dem.bse <- summary(cnty.dem.blk00)$coef[,2]
dem.bse["pslave1860"] <- sd(cnty.dem.blk00.boots)
aff.bse <- summary(cnty.aff.blk00)$coef[,2]
aff.bse["pslave1860"] <- sd(cnty.aff.blk00.boots)
resent.bse <- summary(cnty.resent.blk00)$coef[,2]
resent.bse["pslave1860"] <- sd(cnty.resent.blk00.boots)
#save(list=c("cnty.dem.blk00.boots", "cnty.aff.blk00.boots", "cnty.resent.blk00.boots"), file = "blkprop00-seqg-boots.RData")

blkprop.tab <- stargazer(cnty.res.blk00.ptbias, cnty.dem.blk00, cnty.aff.blk00.ptbias, cnty.aff.blk00, cnty.resent.blk00.ptbias, cnty.resent.blk00,
          keep = c("pslave1860", "blkprop00"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"), se = list(NULL, dem.bse, NULL, aff.bse, NULL, resent.bse),
          covariate.labels = c("Prop. Slave, Direct Effect", "Prop. Black, 2000"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 7), ch.row("State Fixed Effects", rep(TRUE, 6)), ch.row("1860 Covariates", rep(TRUE, 6)), ch.row("Bootstrapped SEs", rep(c(FALSE,TRUE), 3)), c("Model", rep(c("WLS", "Seq. g-est."), 3)), rep("", 7)), multicolumn = TRUE)
blkprop.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{2}{c}{Prop. Democrat} & \\multicolumn{2}{c}{Affirm. Action} & \\multicolumn{2}{c}{Racial Resentment} \\\\ "
blkprop.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", blkprop.tab)
blkprop.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", blkprop.tab)
blkprop.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", blkprop.tab)
blkprop.tab <- blkprop.tab[-(length(blkprop.tab)-1)]
cat(paste(blkprop.tab, collapse = "\n"), "\n")
##cat(paste(blkprop.tab, collapse = "\n"), "\n", file = "../tables/racial-threat.tex")

cor(south.counties$pslave1860, south.counties$blkprop70, use = "complete.obs")
cor(south.counties$pslave1860, south.counties$blkprop00, use = "complete.obs")

## Appendix Table
seqg.tab <- stargazer(cnty.dem.blk00.first, cnty.aff.blk00.first, cnty.resent.blk00.first,
          keep = c("pslave1860", "blkprop00", "totpop00", "highsch90", "unemp","medinc","wbincratio"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860","Prop. Black 2000", "Log Population, 2000", "Percent High School Graduates, 1990", "Unemployment, 1999", "Median Income, 2000", "White-Black Income Ratio, 2000"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 4), ch.row("State Fixed Effects", rep(TRUE, 3)), ch.row("1860 Covariates", rep(TRUE, 3)), rep("", 4)), multicolumn = FALSE)
seqg.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", seqg.tab)
seqg.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", seqg.tab)
seqg.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", seqg.tab)
seqg.tab <- seqg.tab[-(length(seqg.tab)-1)]
cat(paste(seqg.tab, collapse = "\n"), "\n")
##cat(paste(seqg.tab, collapse = "\n"), "\n", file = "../tables/sequential-g-first.tex")

## antebellum results

cnty.dem.houses <- lm(update(base1860.form, dem ~ . + slavesperhouse), data = south.counties, weights = sample.size)
summary(cnty.dem.houses)

cnty.aff.houses <- lm(update(base1860.form, affirm ~ . + slavesperhouse), data = south.counties, weights = sample.size)
summary(cnty.aff.houses)

cnty.resent.houses <- lm(update(base1860.form, resent ~ . + slavesperhouse), data = south.counties, weights = sample.size.res)
summary(cnty.resent.houses)

cnty.dem.mortrate <- lm(update(base1860.form, dem ~ . + I(log(s.mortrate/w.mortrate))), data = south.counties, weights = sample.size, subset = s.mortrate > 0)
summary(cnty.dem.mortrate)

cnty.aff.mortrate <- lm(update(base1860.form, affirm ~ . + I(log(s.mortrate/w.mortrate))), data = south.counties, weights = sample.size, subset = s.mortrate > 0)
summary(cnty.aff.mortrate)

cnty.resent.mortrate <- lm(update(base1860.form, resent ~ . + I(log(s.mortrate/w.mortrate))), data = south.counties, weights = sample.size.res, subset = s.mortrate > 0)
summary(cnty.resent.mortrate)


ante.tab <- stargazer(cnty.dem.mortrate, cnty.dem.houses, cnty.aff.mortrate, cnty.aff.houses, cnty.resent.mortrate, cnty.resent.houses,
          keep = c("pslave1860", "mortrate", "slavesperhouse"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860", "Log Relative Slave Mortality, 1860", "Avg. Residents per Slave Dwelling"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 7), ch.row("State Fixed Effects", rep(TRUE, 6)), ch.row("1860 Covariates", rep(TRUE, 6)), rep("", 7)), multicolumn = FALSE)
ante.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{2}{c}{Prop. Democrat} & \\multicolumn{2}{c}{Affirm. Action} & \\multicolumn{2}{c}{Racial Resentment} \\\\ "
ante.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", ante.tab)
ante.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", ante.tab)
ante.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", ante.tab)
ante.tab <- ante.tab[-(length(ante.tab)-1)]
cat(paste(ante.tab, collapse = "\n"), "\n")
##cat(paste(ante.tab, collapse = "\n"), "\n", file = "../tables/antebellum.tex")



coef(cnty.dem.houses)["pslave1860"]*sd(south.counties$pslave1860, na.rm = TRUE)
coef(cnty.dem.houses)["slavesperhouse"]*sd(south.counties$slavesperhouse, na.rm = TRUE)


## 1851 Gubernatorial elections in MS and GA
## see txt files accompanying these csv files for codebooks
## Data comes from ICPSR 00001

ga.votes <- read.csv("ga-votes-1824-1860.csv", stringsAsFactors = FALSE)
ga.votes$cfips <- strtrim(ga.votes$V3, nchar(ga.votes$V3)-1)
ga.votes$fips <- as.numeric(paste(13, str_pad(ga.votes$cfips, 3, side = "left", pad = "0"), sep = ""))
is.na(ga.votes) <- ga.votes == 9999999
ga.votes$howell1851 <- ga.votes$V345/ga.votes$V348
head(ga.votes$howell1851)


ga.votes <- merge(ga.votes, countydata, by = "fips", all.x = TRUE, all.y = FALSE)


lattice::xyplot(howell1851 ~ sprop1850, data = ga.votes, subset = state.abb %in% st.list , pch = 16, lwd = 4, col.line = "indianred", col = dodgerblue.30, xlab = "Proportion Slave, 1850", ylab = "Howell Cobb Vote-share, 1851", panel =
                    function(x,y,...) {  panel.xyplot(x, y, ...)
                                         panel.smoother(x,y,method = "loess", col = "indianred", lwd = 4)})

summary(lm(howell1851 ~ sprop1850, data = ga.votes))

ms.votes <- read.csv("ms-votes-1824-1860.csv", stringsAsFactors = FALSE)
ms.votes$cfips <- strtrim(ms.votes$V3, nchar(ms.votes$V3)-1)
ms.votes$fips <- as.numeric(paste(28, str_pad(ms.votes$cfips, 3, side = "left", pad = "0"), sep = ""))
is.na(ms.votes) <- ms.votes == 9999999
ms.votes$foote1851 <- ms.votes$V187/ms.votes$V190
head(ms.votes$foote1851)

ms.votes <- merge(ms.votes, countydata, by = "fips", all.x = TRUE, all.y = FALSE)


lattice::xyplot(foote1851 ~ sprop1850, data = ms.votes, subset = state.abb %in% st.list , pch = 16, lwd = 4, col.line = "indianred", col = dodgerblue.30, xlab = "Proportion Slave, 1850", ylab = "Henry Foote Vote-share, 1851", panel =
                    function(x,y,...) {  panel.xyplot(x, y, ...)
                                         panel.smoother(x,y,method = "loess", col = "indianred", lwd = 4)})

summary(lm(foote1851 ~ sprop1850, data = ms.votes))

#cairo_pdf(filename = "figs/ms-ga-1851-vote.pdf", family = "Minion Pro", height = 4.5, width = 9, pointsize = 11)
p1 <- lattice::xyplot(howell1851 ~ sprop1850, data = ga.votes, subset = state.abb %in% st.list , pch = 16, lwd = 4, ylim = c(0,1), col.line = "indianred", main = "Georgia Gubernatorial Election, 1851", col = dodgerblue.30, xlab = "Proportion Slave, 1850", ylab = "Howell Cobb Vote-share, 1851", panel =
                    function(x,y,...) {  panel.xyplot(x, y, ...)
                                         panel.smoother(x,y,method = "loess", col = "indianred", lwd = 4)})
p2 <- lattice::xyplot(foote1851 ~ sprop1850, data = ms.votes, subset = state.abb %in% st.list , pch = 16, lwd = 4, ylim = c(0,1), col.line = "indianred", main = "Mississippi Gubernatorial Election, 1851", col = dodgerblue.30, xlab = "Proportion Slave, 1850", ylab = "Henry Foote Vote-share, 1851", panel =
                    function(x,y,...) {  panel.xyplot(x, y, ...)
                                         panel.smoother(x,y,method = "loess", col = "indianred", lwd = 4)})
plot(p1, position = c(0.0, 0, 0.49, 1), more = TRUE)
plot(p2, position = c(0.5, 0, 1, 1))
#dev.off()

den.cut <- quantile(south.counties$totpop1860/south.counties$coarea, na.rm = TRUE, probs = .9)

cnty.res.rural <- lm(update(base1860.form, dem ~ . ), data=south.counties, weights = sample.size, subset = totpop1860/coarea < den.cut)
summary(cnty.res.rural)

cnty.aff.rural <- lm(update(base1860.form, affirm ~ .), data =south.counties, weights = sample.size, subset = totpop1860/coarea < den.cut)
summary(cnty.aff.rural)

cnty.resent.rural <- lm(update(base1860.form, resent ~ .), data =south.counties, weights = sample.size.res, subset = totpop1860/coarea < den.cut)
summary(cnty.resent.rural)

civil.fe <- lm(I(farmval - farmval1870)/farmval ~ pslave1860  + state.abb, data = countydata, subset = abs.sample == 1)
summary(civil.fe)

south.counties$civil.des <- with(south.counties, (farmval - farmval1870)/farmval)

cnty.dem.civil <- lm(update(base1860.form, dem ~ . + civil.des), data = south.counties, weights = sample.size)
summary(cnty.dem.civil)

cnty.aff.civil <- lm(update(base1860.form, affirm ~ . + civil.des), data = south.counties, weights = sample.size)
summary(cnty.aff.civil)

cnty.resent.civil <- lm(update(base1860.form, resent ~ . + civil.des), data = south.counties, weights = sample.size.res)
summary(cnty.resent.civil)

ruralciv.tab <- stargazer(cnty.res.rural, cnty.dem.civil, cnty.aff.rural, cnty.aff.civil, cnty.resent.rural, cnty.resent.civil,
          keep = c("pslave1860", "civil.des"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860", "Civil War Destruction"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 7), ch.row("State Fixed Effects", rep(c(TRUE),6)), ch.row("1860 Covariates", rep(TRUE, 6)), ch.row("Dense 1860 Counties Dropped", rep(c(TRUE,FALSE), 3)), rep("", 7)), multicolumn = FALSE)
ruralciv.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{2}{c}{Prop. Democrat} & \\multicolumn{2}{c}{Affirm. Action} & \\multicolumn{2}{c}{Racial Resentment} \\\\ "
ruralciv.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", ruralciv.tab)
ruralciv.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", ruralciv.tab)
ruralciv.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", ruralciv.tab)
ruralciv.tab <- ruralciv.tab[-(length(ruralciv.tab)-1)]
cat(paste(ruralciv.tab, collapse = "\n"), "\n")
##cat(paste(ruralciv.tab, collapse = "\n"), "\n", file = "../tables/rural-civil-war.tex")



young.dem.1860 <- glm(update(base1860.form, dem ~ . + as.factor(year)*state.abb), data = s.whites, family=binomial(), weights = weights, subset = year - age > 1965)
young.dem.1860.rse <- robust.se(young.dem.1860, clvar = "fips")
young.dem.1860.rse


young.aff.1860 <- glm(update(base1860.form, affirm ~ . + as.factor(year)*state.abb), data = s.whites, family=binomial(), weights = weights, subset = year - age > 1965)
young.aff.1860.rse <- robust.se(young.aff.1860, clvar = "fips")
young.aff.1860.rse

young.resent.1860 <- lm(update(base1860.form, resent ~ . + as.factor(year)*state.abb), data = s.whites, weights = weights, subset = year - age > 1965)
young.resent.1860.rse <- robust.se(young.resent.1860, clvar = "fips")
young.resent.1860.rse

young.tab <- stargazer(young.dem.1860, young.aff.1860, young.resent.1860,
          keep = "pslave1860", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser", "bic", "aic"),
          se = list(young.dem.1860.rse[,2],young.aff.1860.rse[,2], young.resent.1860.rse[,2]),
          covariate.labels = c("Prop. Slave, 1860", "1st/2nd Generation Immigrant", "Prop. Slave $\\times$ 1st/2nd Gen. Imm."), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 4), ch.row("State-Year Fixed Effects", rep(c(TRUE),3)), ch.row("1860 Covariates", rep(TRUE, 3)), rep("", 4)), multicolumn = FALSE)
young.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", young.tab)
young.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", young.tab)
young.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", young.tab)
young.tab <- young.tab[-(length(young.tab)-1)]
cat(paste(young.tab, collapse = "\n"), "\n")
##cat(paste(young.tab, collapse = "\n"), "\n", file = "../tables/after-vra.tex")






## Lynchings - no data for TX, VA, WV
#countydata$popavg <- with(countydata, (totpop1880+totpop10+totpop20+totpop30+totpop40+totpop50)/6)
lynch.1860 <- lm(update(base1860.form, I(100000*lynchings/totpop20) ~ .), data = countydata, weights = coarea, subset = state.abb %in% st.list & !(state.abb %in% c("TX","VA","WV")))
summary(lynch.1860)

lynch.iv <- ivreg(update(base.iv.form, I(100000*lynchings/totpop20) ~ .), data = countydata, weights = coarea, subset = state.abb %in% st.list & !(state.abb %in% c("DE","MD","TX", "VA", "WV")))
summary(lynch.iv)


cnty.dem.tract <- lm(update(base1860.form, dem ~ pslave1860*tractor.growth + .), data = south.counties, weights = sample.size)
summary(cnty.dem.tract)

cnty.aff.tract <- lm(update(base1860.form, affirm ~ pslave1860*tractor.growth + .), data = south.counties, weights = sample.size)
summary(cnty.aff.tract)

cnty.resent.tract <- lm(update(base1860.form, resent ~ pslave1860*tractor.growth + .), data = south.counties, weights = sample.size.res)
summary(cnty.resent.tract)

quantile(south.counties$tractor.growth, probs = c(0.1, 0.9),na.rm = TRUE)
## CIs for 1st, 3rd quartiles using the sequential g-estimation approach
cnty.dem.tract.lo <- lm(update(base1860.form, I(dem - coef(cnty.dem.tract)['tractor.growth']*(tractor.growth - 0) - coef(cnty.dem.tract)['pslave1860:tractor.growth']*(tractor.growth-0)*pslave1860) ~ .), data = south.counties, weights = sample.size)
coef(cnty.dem.tract.lo)["pslave1860"]
confint(cnty.dem.tract.lo)["pslave1860",]

cnty.dem.tract.hi <- lm(update(base1860.form, I(dem - coef(cnty.dem.tract)['tractor.growth']*(tractor.growth - 0.07) - coef(cnty.dem.tract)['pslave1860:tractor.growth']*(tractor.growth-0.07)*pslave1860) ~ .), data = south.counties, weights = sample.size)
coef(cnty.dem.tract.hi)["pslave1860"]
confint(cnty.dem.tract.hi)["pslave1860",]

tractlynch.tab <- stargazer(lynch.1860, cnty.dem.tract, cnty.aff.tract, cnty.resent.tract,
          keep = c("pslave1860", "tractor.growth"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860", "Tractors Change, 1930-1940", "Prop Slave $\\times$ Tractors Change"), dep.var.labels = c("Lynchings", "Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 5), ch.row("State-Year Fixed Effects", rep(c(TRUE),4)), ch.row("1860 Covariates", rep(TRUE, 4)), rep("", 5)), multicolumn = FALSE)
tractlynch.tab[4] <- "\\\\[-1.8ex] &  Lynchings & Prop. Democrat & Affirm. Action & Racial Resentment \\\\ "
tractlynch.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", tractlynch.tab)
tractlynch.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", tractlynch.tab)
tractlynch.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", tractlynch.tab)
tractlynch.tab <- append(tractlynch.tab, "\\\\ Specification & WLS-county area & WLS-sample size & WLS-sample size & WLS-sample size \\\\", after = (length(tractlynch.tab)-3))
tractlynch.tab <- tractlynch.tab[-(length(tractlynch.tab)-1)]
cat(paste(tractlynch.tab, collapse = "\n"), "\n")
##cat(paste(tractlynch.tab, collapse = "\n"), "\n", file = "../tables/tractors-lynchings.tex")



tractors.lynch <- lm(update(base1860.form, I(lynchrate*100000) ~ tractor.growth + .), data = south.counties, subset = abs.sample == 1, weight = coarea)
summary(tractors.lynch)

tractors.ineq <- lm(update(base1860.form, I(blklwage1940-whtlwage1940) ~ tractor.growth + .), data = south.counties, subset = abs.sample == 1)
summary(tractors.ineq)

tractors.iv <- ivreg(update(base.iv.form, tractor.growth ~  .), data = south.counties, weights = coacres25)
summary(tractors.iv)

tractcheck.tab <- stargazer(tractors.lynch, tractors.ineq,
          keep = c("tractor.growth", "pslave"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
                            dep.var.labels = c("Lynchings per 100,000 residents", "log(Black-White Wage Ratio)"), covariate.labels = c("Tractor Growth, 1930-1940", "Prop. Slave, 1860"), column.sep.width = "0pt", float = FALSE, header = FALSE, add.lines = list(rep("", 4), ch.row("State-Year Fixed Effects", rep(c(TRUE),3)), ch.row("1860 Covariates", rep(TRUE, 3)), rep("", 4)), multicolumn = FALSE)
tractcheck.tab[4] <- "\\\\[-1.8ex] &  Lynchings per & Log Black-White \\\\ "
tractcheck.tab <- append(tractcheck.tab, "\\\\[-1.8ex] &  100,000 residents & Wage Ratio, 1940 \\\\", after = 4)
tractcheck.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", tractcheck.tab)
tractcheck.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", tractcheck.tab)
tractcheck.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", tractcheck.tab)
tractcheck.tab <- tractcheck.tab[-(length(tractcheck.tab)-1)]
cat(paste(tractcheck.tab, collapse = "\n"), "\n")
##cat(paste(tractcheck.tab, collapse = "\n"), "\n", file = "../tables/tractor-check.tex")


btenancy.1860 <- lm(update(base1860.form, I(btenantprop25/100-wtenantprop25/100) ~ .), data = countydata, subset = state.abb %in% st.list, weights = coacres25)
summary(btenancy.1860)

btenancy.iv <- ivreg(update(base.iv.form, I(btenantprop25/100-wtenantprop25/100) ~  .), data = countydata, subset = state.abb %in% st.list & cottonsuit > 0, weights = coacres25)
summary(btenancy.iv)

bowner.1860 <- lm(update(base1860.form, I(bfarmerownerprop25-wfarmerownerprop25) ~ .), data = countydata, subset = state.abb %in% st.list, weights = coacres25)
summary(bowner.1860)

bowner.iv <- ivreg(update(base.iv.form, I(bfarmerownerprop25-wfarmerownerprop25) ~  .), data = countydata, subset = state.abb %in% st.list, weights = coacres25)
summary(bowner.iv)


intermed.tab <- stargazer(btenancy.1860, btenancy.iv, bowner.1860, bowner.iv,
          keep = "pslave1860", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860"), dep.var.labels = c("Black Tenant Share", "Share of Black Farmer Pop. Owning"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 7), ch.row("State Fixed Effects", rep(TRUE, 4)), ch.row("Geographic Controls", rep(TRUE, 6)), ch.row("1860 Covariates", rep(c(TRUE,FALSE), 2)),rep("", 5)), multicolumn = TRUE, model.names = FALSE)
intermed.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{2}{c}{Black-White Tenancy} & \\multicolumn{2}{c}{Black-White Ownwer} \\\\ "
intermed.tab <- append(intermed.tab, "\\\\[-1.8ex] &  \\multicolumn{2}{c}{Share Gap, 1925} & \\multicolumn{2}{c}{Share Gap, 1925} \\\\", after = 4)
intermed.tab <- append(intermed.tab, "\\\\[-1.8ex] & OLS & IV & OLS & IV \\\\ ", after = 5)
intermed.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", intermed.tab)
intermed.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", intermed.tab)
intermed.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", intermed.tab)
intermed.tab <- intermed.tab[-(length(intermed.tab)-1)]
cat(paste(intermed.tab, collapse = "\n"), "\n")
##cat(paste(intermed.tab, collapse = "\n"), "\n", file = "../tables/tenancy.tex")



## Most county boundaries are set by 1925 or so.
## variables without the 1860 suffix are based on the 1860 county
## boundaries
pres.form <- formula(. ~ sprop + log(coarea00) + latitude + I(latitude^2) + longitude + I(longitude^2)+ rugged  + land.ineq + sfarmprop + log(totpop) + log(fvalpc) + log(acimp) + fbprop  + rail + water + state.abb)
pres.iv.form <- Formula(. ~ sprop + log(coarea00) + rugged + latitude + I(latitude^2) + longitude + I(longitude^2)  + water  + state.abb | cottonsuit + log(coarea00) + rugged  + latitude + I(latitude^2) + longitude + I(longitude^2) + water  + state.abb)
year.list <- seq(1840, 1964, by = 4)
outvars <- paste("pdem", year.list, sep = "")
pdemcoefs <- matrix(NA, nrow = length(outvars), ncol = 3)
pdemcoefs.nox <- matrix(NA, nrow = length(outvars), ncol = 3)
pdemcoefs.iv <- matrix(NA, nrow = length(outvars), ncol = 3)
pdemcoefs.rfns <- matrix(NA, nrow = length(outvars), ncol = 3)
pdemcoefs.rf <- matrix(NA, nrow = length(outvars), ncol = 3)
for (y in 1:length(outvars)) {
  if (!(outvars[y] %in% c("pdem1864", "pdem1868"))) {
    ## OLS
    ff <- as.formula(paste(outvars[y], " ~ ."))
    if (year.list[y] < 1924) {
      thismod <- lm(update(pres.form, ff), data = countydata, subset = state.abb %in% st.list)
    } else {
      thismod <- lm(update(base1860.form, ff), data = countydata, subset = state.abb %in% st.list)
    }
    pdemcoefs[y,1] <- 0.25*coef(thismod)[2]
    pdemcoefs[y,2:3] <- 0.25*confint(thismod)[2,]
    ## Only state FEs
    if (year.list[y] < 1924) {
      ff <- as.formula(paste(outvars[y], " ~ sprop + state.abb"))
    } else {
      ff <- as.formula(paste(outvars[y], " ~ pslave1860 + state.abb"))
    }
    thismod <- lm(ff, data = countydata, subset = state.abb %in% st.list)
    pdemcoefs.nox[y,1] <- 0.25*coef(thismod)[2]
    pdemcoefs.nox[y,2:3] <- 0.25*confint(thismod)[2,]
    ## IV
    if (year.list[y] < 1924) {
      ff <- update(pres.iv.form, as.formula(paste(outvars[y], "~ . | .")))
    } else {
      ff <- update(base.iv.form, as.formula(paste(outvars[y], "~ . | .")))
    }
    thismod <- ivreg(ff, data = countydata, subset = state.abb %in% st.list)
    pdemcoefs.iv[y,1] <- coef(thismod)[2]
    pdemcoefs.iv[y,2:3] <- confint(thismod)[2,]
    ## Reduced form in the non-south
    ff <- as.formula(paste(outvars[y], "~ cottonsuit + log(coarea00) + rugged+ latitude + I(latitude^2) + longitude + I(longitude^2)   + state.abb"))
    thismod <- lm(ff, data = countydata, subset = !(state.abb %in% st.list) & !(state.abb %in% c("MD", "DE", "MO")))
    pdemcoefs.rfns[y,1] <- coef(thismod)[2]
    pdemcoefs.rfns[y,2:3] <- confint(thismod)[2,]
    ## Reduced form in the south
    thismod <- lm(ff, data = countydata, subset = state.abb %in% st.list)
    pdemcoefs.rf[y,1] <- coef(thismod)[2]
    pdemcoefs.rf[y,2:3] <- confint(thismod)[2,]
  }
  ## if (outvars[y] %in% c("pdem1860","pdem1864")) {
  ##   pdemcoefs[y,1] <- NA
  ##   pdemcoefs[y,2:3] <- NA
  ##   pdemcoefs.iv[y,1] <- NA
  ##   pdemcoefs.iv[y,2:3] <- NA
  ## }
  ## if (outvars[y] == "pdem1856") {
  ##   pdemcoefs[y,1] <- NA
  ##   pdemcoefs[y,2:3] <- NA
  ## }
}

douglas.ols <- lm(update(pres.form, pdem1860 ~ .), data = countydata, subset = state.abb %in% st.list)
douglas.iv <- ivreg(update(pres.iv.form, pdem1860 ~ .), data = countydata, subset = state.abb %in% st.list)

wallace.ols <- lm(update(base1860.form, wallace68.alt ~ .), data = countydata, subset = state.abb %in% st.list)
wallace.iv <- ivreg(update(base.iv.form, wallace68.alt ~ .), data = countydata, subset = state.abb %in% st.list)
thurmond.ols <- lm(update(base1860.form, thurmond48 ~ .), data = countydata, subset = state.abb %in% st.list)
thurmond.iv <- ivreg(update(base.iv.form, thurmond48 ~ .), data = countydata, subset = state.abb %in% st.list)
obama.ols <- lm(update(base1860.form,  wht.obama.vote ~ .), data = countydata, subset = abs.sample == 1)
obama.iv <- ivreg(update(base.iv.form, wht.obama.vote  ~ .), data = countydata, subset = abs.sample == 1)

##cairo_pdf(file= "../figs/prescountyvote-iv.pdf", family = "Minion Pro", height = 4.5, width = 6.5, pointsize = 9)
plot(x = year.list, y = 0.25*pdemcoefs.iv[,1], ylim = range(c(.25*pdemcoefs.iv,25*confint(obama.iv)["pslave1860",]), na.rm = TRUE), xlim=c(min(year.list),2016), xlab = "Year",
     ylab = "Effect of Slavery on % Democrat", pch = 19, main = "Presidential Elections", bty = "n", yaxt = "n")
abline(v = 1904, lty = 2, col = "grey70")
text(x = 1904, y = 25*confint(obama.iv)["pslave1860",1]+0.5, "All states but KY have\nenacted poll taxes", pos = 4)
abline(v = 1965, lty = 2, col = "grey70")
text(x = 1965, y =  25*confint(obama.iv)["pslave1860",1]+0.5, "Voting Rights Act", pos = 4)
axis(side = 2, las = 2, cex = 0.8)
abline(h=0, col = "grey")
segments(x0 = year.list, y0 = .25*pdemcoefs.iv[,2], y1 = .25*pdemcoefs.iv[,3])
rect(xleft = 1860, xright = 1877, ybottom = -100, ytop=100, col = rgb(.5,.5,.5, alpha = 0.5), border = NA)
text(x = 1860, y = max(.25*pdemcoefs.iv, na.rm=TRUE)-1, "Civil War\nBegins", pos = 2)
text(x = 1877, y = max(.25*pdemcoefs.iv, na.rm=TRUE)-1, "Reconstruction\nEnds", pos = 4)
points(x = 1968, y = .25*coef(wallace.iv)["pslave1860"], pch = 17, col = "indianred")
segments(x0 = 1968, y0 = 0.25*confint(wallace.iv)["pslave1860",1], y1 = 0.25*confint(wallace.iv)["pslave1860",2], col = "indianred")
text(x = 1968, y = 0.25*coef(wallace.iv)["pslave1860"], "Wallace\n1968", pos = 4, col = "indianred")
points(x = 1949, y = .25*coef(thurmond.iv)["pslave1860"], pch = 17, col = "indianred")
segments(x0 = 1949, y0 = 0.25*confint(thurmond.iv)["pslave1860",1], y1 = 0.25*confint(thurmond.iv)["pslave1860",2], col = "indianred")
text(x = 1949, y = 0.35*confint(thurmond.iv)["pslave1860",2], "Thurmond\n1948", pos = 3, col = "indianred")
segments(x0=1949, y0=0.35*confint(thurmond.iv)["pslave1860",2],y1=0.26*confint(thurmond.iv)["pslave1860",2], lty = 3, col = "grey70")
points(x = 2008, y = 25*coef(obama.iv)["pslave1860"], pch = 19)
segments(x0 = 2008, y0 = 25*confint(obama.iv)["pslave1860",1], y1 = 25*confint(obama.iv)["pslave1860",2])
text(x = 2008, y = 25*coef(obama.iv)["pslave1860"], "Obama\n2008", pos = 4)
##dev.off()


##cairo_pdf(file= "../figs/prescountyvote-rform.pdf", family = "Minion Pro", height = 4, width = 6, pointsize = 9)
plot(x = year.list[year.list > 1868], y = pdemcoefs.rfns[year.list > 1868,1], ylim = range(c(pdemcoefs.rfns[year.list > 1868,], pdemcoefs.rf[year.list > 1868,]), na.rm = TRUE), xlab = "Year",
     ylab = "Effect of Cotton Suitability on % Democrat", pch = 19, main = "Presidential Elections", bty = "n", yaxt = "n")
points(x = year.list[year.list > 1868] + 1, y = pdemcoefs.rf[year.list > 1868,1], pch = 19, col = "indianred")
axis(side = 2, las = 2, cex = 0.8)
abline(h=0, col = "grey")
segments(x0 = year.list[year.list > 1868], y0 = pdemcoefs.rfns[year.list > 1868,2], y1 = pdemcoefs.rfns[year.list > 1868,3])
segments(x0 = year.list[year.list > 1868] + 1, y0 = pdemcoefs.rf[year.list > 1868,2], y1 = pdemcoefs.rf[year.list > 1868,3], col = "indianred")
##dev.off()


### Appendix tables

south.counties$lynchrate.scaled <- 100000*south.counties$lynchrate

sum.tab <- stargazer(south.counties[,c("dem", "affirm", "resent","lynchrate.scaled","coarea00", "rugged", "latitude", "longitude", "pslave1860", "land.ineq1860", "sfarmprop1860", "totpop1860", "fvalpac1860", "fbprop1860", "rail1860", "water1860", "cottonsuit", "blkprop00","tractor.growth")], covariate.labels = c("Prop. Democrat", "Support for Affirmative Action", "Racial Resentment", "Lynchings per 100,000 1920 Residents, 1882-1930", "County Area, 2000", "Ruggedness", "Latitude, 2000", "Longitude, 2000", "Prop. Slave, 1860", "Gini Coefficient for Land Holdings, 1860", "Prop. Small Farms ($<$ 50 Acres), 1860", "Total Population, 1860", "Farm Value per Capita, 1860", "Prop. Free Black, 1860", "Rail Access, 1860", "Water Acces, 1860", "Cotton Suitability", "Prop. Black, 2000", "Tractor Growth, 1930-1940"), float = FALSE, header = FALSE)
sum.tab <- append(sum.tab, "\\emph{Outcomes} \\\\", after = 6)
sum.tab <- append(sum.tab, "\\\\ \\emph{Geographic Variables} \\\\", after = 11)
sum.tab <- append(sum.tab, "\\\\ \\emph{1860 Variables} \\\\", after = 16)
sum.tab <- append(sum.tab, "\\\\ \\emph{Other Variables} \\\\", after = 25)
cat(paste(sum.tab, collapse = "\n"), "\n")
##cat(paste(sum.tab, collapse = "\n"), "\n", file = "../tables/summary-stats-county.tex")

cces.sum.tab <- stargazer(s.whites[,c("dem", "affirm", "resent", "age", "religion", "female", "inc.mid")], covariate.labels = c("Democratic Identification", "Support for Affirmative Action", "Racial Resentment", "Age", "Religious Importance", "Gender (Female = 1, Male = 0)", "Household Income (Bracket Midpoint)"), header = FALSE, float = FALSE)
ns.whites$wb.thermdiff <- ns.whites$wtherm-ns.whites$btherm
anes.sum.tab <- stargazer(ns.whites[,c("btherm", "wtherm", "wb.thermdiff")], covariate.labels = c("Black Therm. Score", "White Therm. Score", "White-Black Therm. Difference"), header = FALSE, float = FALSE)
cces.sum.tab <- append(cces.sum.tab, "\\emph{CCES, 2006, 2008, 2009, 2010, 2011} \\\\", after = 6)
cces.sum.tab <- append(cces.sum.tab, "\\\\ \\emph{ANES, 1984-1998} \\\\", after = 14)
cces.sum.tab <- append(cces.sum.tab, anes.sum.tab[7:9], after = 15)
cat(paste(cces.sum.tab, collapse = "\n"), "\n")
##cat(paste(cces.sum.tab, collapse = "\n"), "\n", file = "../tables/summary-stats-ind.tex")



south.counties$slavesperwhite <- with(south.counties, stot1860/whtot1860)
## with alternative definition of slavery -- slaves per white
sph.dem <- lm(dem ~ log(1+slavesperwhite), data = south.counties, weights = sample.size)
summary(sph.dem)

sph.dem.fe <- lm(dem ~ log(1+slavesperwhite) + state.abb, data = south.counties, weights = sample.size)
summary(sph.dem.fe)

sph.dem.full <- lm(update(base1860.form, dem ~ log(1+slavesperwhite) + . - pslave1860), data = south.counties, weights = sample.size)
summary(sph.dem.full)

sph.iv <- ivreg(update(base.iv.form, dem ~ log(1+slavesperwhite) + . - pslave1860), data = south.counties, weights = sample.size)
summary(sph.iv)


sph.aff <- lm(affirm ~ log(1+slavesperwhite), data = south.counties, weights = sample.size)
summary(sph.aff)

sph.aff.fe <- lm(affirm ~ log(1+slavesperwhite) + state.abb, data = south.counties, weights = sample.size)
summary(sph.aff.fe)

sph.aff.full <- lm(update(base1860.form, affirm ~ log(1+slavesperwhite) + . - pslave1860), data = south.counties, weights = sample.size)
summary(sph.aff.full)

sph.aff.iv <- ivreg(update(base.iv.form, affirm ~ log(1+slavesperwhite) + . - pslave1860), data = south.counties, weights = sample.size, subset = cottonsuit > 0)
summary(sph.aff.iv)


sph.resent <- lm(resent ~ log(1+slavesperwhite), data = south.counties, weights = sample.size.res)
summary(sph.resent)

sph.resent.fe <- lm(resent ~ log(1+slavesperwhite) +  state.abb, data = south.counties, weights = sample.size.res)
summary(sph.resent.fe)

sph.resent.full <- lm(update(base1860.form, resent ~ log(1+slavesperwhite) + . - pslave1860), data = south.counties, weights = sample.size.res)
summary(sph.resent.full)

sph.resent.iv <- ivreg(update(base.iv.form, resent ~ log(1+slavesperwhite) + . - pslave1860| .), data = south.counties, weights = sample.size.res, subset = cottonsuit > 0)
summary(sph.resent.iv)

sph.res.first <- lm(update(base.first.form, log(1+slavesperwhite) ~ .), data = south.counties, weights = sample.size, na.action = na.exclude)
summary(sph.res.first)


stargazer(sph.dem.full, sph.iv, sph.aff.full, sph.aff.iv, sph.resent.full,sph.resent.iv,
          keep = "slavesperwhite", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Log(1 + Slaves per White, 1860)"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"),
          title = "Effect of slavery on white partisan identification, views on affirmative action, and racial resentment with an alternative definition of slavery: slaves per slaveholder.", label = "t:slavesperholder")




## Individual results (whites)

varlabs <- c("Prop. Slave, 1860", "Log(County Area)", "Ruggedness", "Land Inequality, 1860", "Prop. Small Farm, 1860", "Log(Total Pop., 1860)", "Log(Farm Value per  acre, 1860)", "Prop. Free Black, 1860", "Log(Improved Acres, 1860)", "Rail Access, 1860", "Water Access, 1860", "Educ: HS Grad", "Educ: Some College", "Educ: 2-year degree", "Educ: 4-year degree", "Educ: Postgrad", "Family Inc: \\$20,000-50,000", "Family Inc: \\$50,000-100,000", "Family Inc: \\$100,000-150,000", "Family Inc: \\$150,000+", "Religion Import.", "Female", "Age", "Prop. Black in Zip, 2000", "Log(Median Zip Code Inc, 2010)", "White Unemp. Rate, 2010-2014", "Log(White-Black Median Inc Ratio)")

ind.dem.fe <- glm(dem ~ pslave1860 + state.abb*as.factor(year), data = s.whites, family=binomial(), weights = weights)
robust.se(ind.dem.fe, clvar = "fips")

ind.dem.1860 <- glm(update(base1860.form, dem ~ . + state.abb*as.factor(year)), data = s.whites, family=binomial(), weights = weights)
ind.dem.1860.rse <- robust.se(ind.dem.1860, clvar = "fips")
ind.dem.1860.rse

ind.dem.full <- glm(update(ind.form, dem ~ .), data = s.whites, family=binomial(), weights = weights)
ind.dem.full.rse <-robust.se(ind.dem.full, clvar = "fips")
ind.dem.full.rse

ind.dem.context <- glm(update(context.form, dem ~ .), data = s.whites, family = binomial(), weights = weights)
ind.dem.context.rse <- robust.se(ind.dem.context, clvar = "fips")
ind.dem.context.rse

ind.dem.int <- glm(update(ind.int.form, dem ~ .), data = s.whites, family=binomial(), weights = weights)
ind.dem.int.rse <-robust.se(ind.dem.int, clvar = "fips")
ind.dem.int.rse

ind.dem.context.int <- glm(update(context.int.form, dem ~ .), data = s.whites, family = binomial(), weights = weights)
ind.dem.context.int.rse <- robust.se(ind.dem.context.int, clvar = "fips")
ind.dem.context.int.rse



ind.aff.fe <- glm(affirm ~ pslave1860 + state.abb*as.factor(year), data = s.whites, family=binomial(), weights = weights)
robust.se(ind.aff.fe, clvar = "fips")

ind.aff.1860 <- glm(update(base1860.form, affirm ~ . + state.abb*as.factor(year)), data = s.whites, family=binomial(), weights = weights)
ind.aff.1860.rse <- robust.se(ind.aff.1860, clvar = "fips")
ind.aff.1860.rse

ind.aff.full <- glm(update(ind.form, affirm ~ .), data = s.whites, family=binomial(), weights = weights)
ind.aff.full.rse <- robust.se(ind.aff.full, clvar = "fips")
ind.aff.full.rse

ind.aff.context <- glm(update(context.form, affirm ~ .), data = s.whites, family = binomial(), weights = weights)
ind.aff.context.rse <- robust.se(ind.aff.context, clvar = "fips")
ind.aff.context.rse

ind.aff.int <- glm(update(ind.int.form, affirm ~ .), data = s.whites, family=binomial(), weights = weights)
ind.aff.int.rse <-robust.se(ind.aff.int, clvar = "fips")
ind.aff.int.rse

ind.aff.context.int <- glm(update(context.int.form, affirm ~ .), data = s.whites, family = binomial(), weights = weights)
ind.aff.context.int.rse <- robust.se(ind.aff.context.int, clvar = "fips")
ind.aff.context.int.rse


ind.resent.fe <- lm(resent ~ pslave1860 + state.abb*as.factor(year), data = s.whites, weights = weights)
robust.se(ind.resent.fe, clvar = "fips")

ind.resent.1860 <- lm(update(base1860.form, resent ~ . + state.abb*as.factor(year)), data = s.whites, weights = weights)
ind.resent.1860.rse <- robust.se(ind.resent.1860, clvar = "fips")
ind.resent.1860.rse

ind.resent.full <- lm(update(ind.form, resent ~ .), data = s.whites, weights = weights)
ind.resent.full.rse <- robust.se(ind.resent.full, clvar = "fips")
ind.resent.full.rse

ind.resent.context <- lm(update(context.form, resent ~ .), data = s.whites, weights = weights)
ind.resent.context.rse <- robust.se(ind.resent.context, clvar = "fips")
ind.resent.context.rse

ind.resent.int <- lm(update(ind.int.form, resent ~ .), data = s.whites, weights = weights)
ind.resent.int.rse <- robust.se(ind.resent.int, clvar = "fips")
ind.resent.int.rse

ind.resent.context.int <- lm(update(context.int.form, resent ~ .), data = s.whites, weights = weights)
ind.resent.context.int.rse <- robust.se(ind.resent.context.int, clvar = "fips")
ind.resent.context.int.rse


ind.full.table <- stargazer(ind.dem.1860, ind.dem.full, ind.dem.context, ind.aff.1860, ind.aff.full, ind.aff.context, ind.resent.1860, ind.resent.full,ind.resent.context,
          omit = c("state","year", "latitude", "longitude"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser", "bic", "rsq", "aic"),
          se = list(ind.dem.1860.rse[,2], ind.dem.full.rse[,2], ind.dem.context.rse[,2], ind.aff.1860.rse[,2], ind.aff.full.rse[,2], ind.aff.context.rse[,2], ind.resent.1860.rse[,2], ind.resent.full.rse[,2], ind.resent.context.rse[,2]), column.sep.width = "0pt",
          covariate.labels = varlabs, float = FALSE, header = FALSE, add.lines = list(rep("", 9), ch.row("State-Year Fixed Effects", rep(TRUE,9)), ch.row("Latitude/Longitude", rep(TRUE, 9)), rep("", 9)), multicolumn = TRUE, dep.var.labels = rep(c("Prop Democrat", "Affirm. Action", "Racial Resentment"), each = 3))
ind.full.table[4] <- "\\\\[-1.8ex] & \\multicolumn{3}{c}{Prop. Democrat} & \\multicolumn{3}{c}{Affirm. Action} & \\multicolumn{3}{c}{Racial Resentment} \\\\ "
ind.full.table[5] <- "\\\\[-1.8ex] & \\multicolumn{3}{c}{Logit} & \\multicolumn{3}{c}{Logit} & \\multicolumn{3}{c}{OLS} \\\\ "
ind.full.table <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", ind.full.table)
ind.full.table <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", ind.full.table)
ind.full.table <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", ind.full.table)
ind.full.table[length(ind.full.table)-1] <- ""
cat(paste(ind.full.table, collapse = "\n"), "\n")
##cat(paste(ind.full.table, collapse = "\n"), "\n", file = "../tables/individual-full.tex")


varlabs.int <- c("Prop. Slave $\\times$ Educ: HS Grad", "Prop. Slave $\\times$ Educ: Some College", "Prop. Slave $\\times$ Educ: 2-year degree", "Prop. Slave $\\times$ Educ: 4-year degree", "Prop. Slave $\\times$ Educ: Postgrad", "Prop. Slave $\\times$ Family Inc: \\$20,000-50,000", "Prop. Slave $\\times$ Family Inc: \\$50,000-100,000", "Prop. Slave $\\times$ Family Inc: \\$100,000-150,000", "Prop. Slave $\\times$ Family Inc: \\$150,000+", "Prop. Slave $\\times$ Religion Import.", "Prop. Slave $\\times$ Female", "Prop. Slave $\\times$ Age", "Prop. Slave $\\times$ Prop. Black in Zip, 2000", "Prop. Slave $\\times$ Log(Median Zip Code Inc, 2010)", "Prop. Slave $\\times$ White Unemp. Rate, 2010-2014", "Prop. Slave $\\times$ Log(White-Black Median Inc Ratio)")

int.full.table <- stargazer(ind.dem.int, ind.dem.context.int, ind.aff.int, ind.aff.context.int, ind.resent.int, ind.resent.context.int,
          keep = c("pslave1860:"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser", "bic", "rsq", "aic"),
          se = list(ind.dem.int.rse[,2], ind.dem.context.int.rse[,2], ind.aff.int.rse[,2], ind.aff.context.int.rse[,2], ind.resent.int.rse[,2], ind.resent.context.int.rse[,2]), column.sep.width = "0pt",
          covariate.labels = varlabs.int, float = FALSE, header = FALSE, add.lines = list(rep("", 6), ch.row("State-Year Fixed Effects", rep(TRUE,6)), ch.row("Lower-Order Terms", rep(TRUE,6)), ch.row("Individual-Level Covariates", rep(TRUE, 6)), ch.row("Contextual Covariates", rep(c(FALSE,TRUE), 3)), rep("", 6)), multicolumn = TRUE, dep.var.labels = rep(c("Prop Democrat", "Affirm. Action", "Racial Resentment"), each = 2))
int.full.table[4] <- "\\\\[-1.8ex] & \\multicolumn{2}{c}{Prop. Democrat} & \\multicolumn{2}{c}{Affirm. Action} & \\multicolumn{2}{c}{Racial Resentment} \\\\ "
int.full.table[5] <- "\\\\[-1.8ex] & \\multicolumn{2}{c}{Logit} & \\multicolumn{2}{c}{Logit} & \\multicolumn{2}{c}{OLS} \\\\ "
int.full.table <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", int.full.table)
int.full.table <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", int.full.table)
int.full.table <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", int.full.table)
int.full.table[length(int.full.table)-1] <- ""
cat(paste(int.full.table, collapse = "\n"), "\n")
##cat(paste(int.full.table, collapse = "\n"), "\n", file = "../tables/individual-interaction.tex")


## Livestock instead of land for civil war destruction

south.counties$civil.des.lstock <- with(south.counties, (livstock - livstock1870)/livstock)
is.na(south.counties$civil.des.lstock) <- which(south.counties$livstock == 0)
cnty.dem.civil.lstock <- lm(update(base1860.form, dem ~ . + civil.des.lstock), data = south.counties, weights = sample.size)
summary(cnty.dem.civil.lstock)

cnty.aff.civil.lstock <- lm(update(base1860.form, affirm ~ . + civil.des.lstock), data = south.counties, weights = sample.size)
summary(cnty.aff.civil.lstock)

cnty.resent.civil.lstock <- lm(update(base1860.form, resent ~ . + civil.des.lstock), data = south.counties, weights = sample.size.res)
summary(cnty.resent.civil.lstock)

livestock.tab <- stargazer(cnty.dem.civil.lstock, cnty.aff.civil.lstock, cnty.resent.civil.lstock,
          keep = c("pslave1860","civil.des.lstock"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
                           covariate.labels = c("Prop. Slave, 1860", "Livestock Value Loss, 1860-1870"), dep.var.labels = c("Prop. Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 3), ch.row("State Fixed Effects", rep(TRUE, 3)), ch.row("1860 Covariates", rep(TRUE, 3)), rep("", 3)), multicolumn = FALSE)
livestock.tab[4]
livestock.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", livestock.tab)
livestock.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", livestock.tab)
livestock.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", livestock.tab)
livestock.tab <- livestock.tab[-(length(livestock.tab)-1)]
cat(paste(livestock.tab, collapse = "\n"), "\n")
##cat(paste(livestock.tab, collapse = "\n"), "\n", file = "../tables/livestock.tex")



## Inequality outcomes
cnty.ineq40.full <- lm(update(base1860.form, I(whtlwage1940-blklwage1940) ~ .), data = south.counties)
summary(cnty.ineq40.full)

cnty.ineq40.iv <- ivreg(update(base.iv.form, I(whtlwage1940-blklwage1940) ~ .), data = south.counties, subset = cottonsuit > 0)
summary(cnty.ineq40.iv)

cnty.ineq40.rform <- lm(update(rform.form, I(blklwage1940) ~ .), data = wh.counties, !(state.abb %in% st.list) & !(state.abb %in% c("MD", "DE", "MO")) & cottonsuit > 0)
summary(cnty.ineq40.rform)

cnty.ineq.full <- lm(update(base1860.form, log(wbincratio2014) ~ .), data = south.counties)
summary(cnty.ineq.full)

cnty.ineq.iv <- ivreg(update(base.iv.form, log(wbincratio2014) ~ .), data = countydata, subset = state.abb %in% st.list & cottonsuit > 0)
summary(cnty.ineq.iv)


cnty.inc.full <- lm(update(base1860.form, log(w.med.income2014) ~ .), data = south.counties)
summary(cnty.inc.full)

cnty.inc.iv <- ivreg(update(base.iv.form, log(w.med.income2014) ~ .), data = south.counties, subset = cottonsuit > 0)
summary(cnty.inc.iv)

cnty.inc.rform <- lm(update(rform.form, log(w.med.income2014) ~ .), data = wh.counties, !(state.abb %in% st.list) & !(state.abb %in% c("MD", "DE", "MO")) & cottonsuit > 0)
summary(cnty.inc.rform)


ineq.tab <- stargazer(cnty.ineq40.full, cnty.ineq40.iv, cnty.ineq.full, cnty.ineq.iv, cnty.inc.full, cnty.inc.iv,
          keep = "pslave1860", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860"), dep.var.labels = c("log White Wage - log Black Wages, 1940", "log White Income - log Black Income, 1990", "log Median Income, 2000"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 7), ch.row("State Fixed Effects", rep(TRUE, 6)), ch.row("Geographic Controls", rep(TRUE, 6)), ch.row("1860 Covariates", rep(c(TRUE, FALSE), 3)), rep("", 7)), multicolumn = FALSE)
ineq.tab[4] <- "\\\\[-1.8ex] &  \\multicolumn{2}{c}{log White-Black} & \\multicolumn{2}{c}{log White-Black} & \\multicolumn{2}{c}{log White Median} \\\\ "
ineq.tab[5] <- " &  \\multicolumn{2}{c}{Wage Gap, 1940} & \\multicolumn{2}{c}{Med. Income Gap, 2014} & \\multicolumn{2}{c}{Income, 2014} \\\\ "
ineq.tab[6] <- "\\\\[-1.8ex] & OLS & IV & OLS & IV & OLS & IV \\\\ "
ineq.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", ineq.tab)
ineq.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", ineq.tab)
ineq.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", ineq.tab)
ineq.tab <- ineq.tab[-(length(ineq.tab)-1)]
cat(paste(ineq.tab, collapse = "\n"), "\n")
##cat(paste(ineq.tab, collapse = "\n"), "\n", file = "../tables/ineq-effect.tex")


ineq.dem <- lm(dem ~ log(wbincratio2014) + state.abb, data = south.counties, weights = sample.size)
summary(ineq.dem)

ineq.dem.full <- lm(update(base1860.form, dem ~ log(wbincratio2014) + .), data = south.counties, weights = sample.size)
summary(ineq.dem.full)


ineq.aff <- lm(affirm ~ log(wbincratio2014) + state.abb, data = south.counties, weights = sample.size)
summary(ineq.aff)

ineq.aff.full <- lm(update(base1860.form, affirm ~ log(wbincratio2014) + .), data = south.counties, weights = sample.size)
summary(ineq.aff.full)

ineq.resent <- lm(resent ~ log(wbincratio2014) + state.abb, data = south.counties, weights = sample.size.res)
summary(ineq.resent)

ineq.resent.full <- lm(update(base1860.form, resent ~ log(wbincratio2014) + .), data = south.counties, weights = sample.size.res)
summary(ineq.resent.full)


ineq2.tab <- stargazer(ineq.dem, ineq.dem.full, ineq.aff, ineq.aff.full, ineq.resent, ineq.resent.full,
          keep = c("wbincratio2014", "pslave1860"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
                       covariate.labels = c("log White-Black Income Ratio, 2014","Prop. Slave, 1860"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 7), ch.row("State Fixed Effects", rep(TRUE, 6)), ch.row("1860 Covariates", rep(TRUE, 6)), rep("", 7)), multicolumn = FALSE)
ineq2.tab[4] <- "\\\\[-1.8ex] & \\multicolumn{2}{c}{Proportion} & \\multicolumn{2}{c}{Support for} & \\multicolumn{2}{c}{Racial}  \\\\ "
ineq2.tab <- append(ineq2.tab, " & \\multicolumn{2}{c}{Democrat} & \\multicolumn{2}{c}{Affirm. Action} &  \\multicolumn{2}{c}{Resentment }  \\\\ ", after = 4)
ineq2.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", ineq2.tab)
ineq2.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", ineq2.tab)
ineq2.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", ineq2.tab)
ineq2.tab <- ineq2.tab[-(length(ineq2.tab)-1)]
cat(paste(ineq2.tab, collapse = "\n"), "\n")
##cat(paste(ineq2.tab, collapse = "\n"), "\n", file = "../tables/ineq-compare.tex")

## Interaction of slavery with income

ind.dem.inc <- glm(update(base1860.form, dem ~ . + pslave1860*log(inc.mid)), data = s.whites, family=binomial())
ind.dem.inc.rse <-robust.se(ind.dem.inc, clvar = "fips")
ind.dem.inc.rse

ind.aff.inc <- glm(update(base1860.form, affirm ~ . + pslave1860*log(inc.mid) ), data = s.whites, family=binomial())
ind.aff.inc.rse <- robust.se(ind.aff.inc, clvar = "fips")
ind.aff.inc.rse

ind.resent.inc<- lm(update(base1860.form, resent ~ . + pslave1860*log(inc.mid)), data = s.whites)
ind.resent.inc.rse <- robust.se(ind.resent.inc, clvar = "fips")
ind.resent.inc.rse

inc.int.tab <-  stargazer(ind.dem.inc, ind.aff.inc, ind.resent.inc,
          keep = c("pslave1860", "inc.mid"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser", "bic"),
          se = list(ind.dem.inc.rse[,2], ind.aff.inc.rse[,2], ind.resent.inc.rse[,2]),
          covariate.labels = c("Prop. Slave, 1860", "Income", "Prop. Slave $\\times$ Income"), dep.var.labels = c("Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 3), ch.row("State Fixed Effects", rep(TRUE, 3)), ch.row("1860 Covariates", rep(TRUE, 3)), ch.row("Clustered SEs", rep(TRUE, 3)), rep("", 3)), multicolumn = FALSE)
inc.int.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", inc.int.tab)
inc.int.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", inc.int.tab)
inc.int.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", inc.int.tab)
inc.int.tab <- inc.int.tab[-(length(inc.int.tab)-1)]
cat(paste(inc.int.tab, collapse = "\n"), "\n")
##cat(paste(inc.int.tab, collapse = "\n"), "\n", file = "../tables/income-interaction.tex")


## How much the black population declined?
south.counties$blk.shrink.4020 <- with(south.counties, blkprop20- blkprop40)
cnty.dem.blkshrink <- lm(update(base1860.form, dem ~ pslave1860*blk.shrink.4020 + .), data = south.counties, weights = sample.size)
summary(cnty.dem.blkshrink)

cnty.aff.blkshrink <- lm(update(base1860.form, affirm ~ pslave1860*blk.shrink.4020 + .), data = south.counties, weights = sample.size)
summary(cnty.aff.blkshrink)

cnty.resent.blkshrink <- lm(update(base1860.form, resent ~ pslave1860*blk.shrink.4020 + .), data = south.counties, weights = sample.size.res)
summary(cnty.resent.blkshrink)

south.counties$blk.shrink.7020 <- with(south.counties, blkprop20- blkprop70)
cnty.dem.blkshrink.70 <- lm(update(base1860.form, dem ~ pslave1860*blk.shrink.7020 + .), data = south.counties, weights = sample.size)
summary(cnty.dem.blkshrink.70)

cnty.aff.blkshrink.70 <- lm(update(base1860.form, affirm ~ pslave1860*blk.shrink.7020  + .), data = south.counties, weights = sample.size)
summary(cnty.aff.blkshrink.70)

cnty.resent.blkshrink.70 <- lm(update(base1860.form, resent ~ pslave1860*blk.shrink.7020 + .), data = south.counties, weights = sample.size.res)
summary(cnty.resent.blkshrink.70)

blkshrink.tab <- stargazer(cnty.dem.blkshrink, cnty.dem.blkshrink.70, cnty.aff.blkshrink, cnty.aff.blkshrink.70, cnty.resent.blkshrink, cnty.resent.blkshrink.70,
          keep = c("pslave1860", "blk.shrink.4020", "blk.shrink.7020"), style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860", "Prop Black Decline, 1940-1920", "Prop Black Decline, 1970-1920", "Prop Slave $\\times$ Black Decline, 1940-1920", "Prop Slave $\\times$ Black Decline, 1970-1920"),
                           dep.var.labels = c( "Prop Democrat", "Affirm. Action", "Racial Resentment"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 3), ch.row("State Fixed Effects", rep(TRUE, 6)), ch.row("1860 Covariates", rep(TRUE, 6)), rep("", 3)), multicolumn = FALSE)
blkshrink.tab[4] <- "\\\\[-1.8ex] & \\multicolumn{2}{c}{Prop. Democrat} & \\multicolumn{2}{c}{Affirm. Action} & \\multicolumn{2}{c}{Racial Resentment} \\\\ "
blkshrink.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", blkshrink.tab)
blkshrink.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", blkshrink.tab)
blkshrink.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", blkshrink.tab)
blkshrink.tab <- blkshrink.tab[-(length(blkshrink.tab)-1)]
cat(paste(blkshrink.tab, collapse = "\n"), "\n")
##cat(paste(blkshrink.tab, collapse = "\n"), "\n", file = "../tables/black-shrink.tex")




## Inflows and outflows
outflow.fe <- lm(I(outflow/totpop00) ~ pslave1860 + state.abb, data = countydata, subset = state.abb %in% st.list)
summary(outflow.fe)

outflow.1860 <- lm(update(base1860.form, I(outflow/totpop00) ~ .), data = countydata, subset = state.abb %in% st.list)
summary(outflow.1860)

inflow.fe <- lm(I(inflow/totpop00) ~ pslave1860 + state.abb, data = countydata, subset = state.abb %in% st.list)
summary(inflow.fe)

inflow.1860 <- lm(update(base1860.form, I(inflow/totpop00) ~ .), data = countydata, subset = state.abb %in% st.list)
summary(inflow.1860)


outmig.tab <- stargazer(inflow.1860, outflow.1860, #cnty.dem.flow, cnty.aff.flow, cnty.resent.flow,
          keep = "pslave1860", style = "apsr", omit.stat = c("ll", "adj.rsq", "F", "ser"),
          covariate.labels = c("Prop. Slave, 1860"), dep.var.labels = c("In-migration", "Out-migration"), column.sep.width = "5pt", float = FALSE, header = FALSE, add.lines = list(rep("", 3), ch.row("State Fixed Effects", rep(TRUE, 2)), ch.row("1860 Covariates", rep(TRUE, 2)), rep("", 3)), multicolumn = FALSE)
outmig.tab <- gsub("\\{\\*\\}", "\\{\\\\dagger\\}", outmig.tab)
outmig.tab <- gsub("\\{\\*\\*\\}", "\\{\\*\\}", outmig.tab)
outmig.tab <- gsub("\\{\\*\\*\\*\\}", "\\{\\*\\*\\}", outmig.tab)
outmig.tab <- outmig.tab[-(length(outmig.tab)-1)]
cat(paste(outmig.tab, collapse = "\n"), "\n")
##cat(paste(outmig.tab, collapse = "\n"), "\n", file = "../tables/outmig.tex")


## Thanks for reading our source code.
##
