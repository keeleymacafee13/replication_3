## Note: this code takes a while to run

library(reshape)
library(maps)

## 1940 PUMS
pums <- read.csv(gzfile("usa_00015_1940_census.csv.gz"), stringsAsFactors = FALSE)
names(pums) <- tolower(names(pums))
pums$county <- substr(pums$county, 0, nchar(pums$county)-1)
pums$county[nchar(pums$county) == 1] <- paste("00", pums$county[nchar(pums$county) == 1], sep = "")
pums$county[nchar(pums$county) == 2] <- paste("0", pums$county[nchar(pums$county) == 2], sep = "")
pums$fips <- as.numeric(paste(pums$statefip, pums$county, sep = ""))
is.na(pums$incwage) <- which(pums$incwage == 999999)
is.na(pums$fwage1) <- which(pums$fwage1 == 99999)
is.na(pums$valueh) <- which(pums$valueh == 999999)
pums.blkmen <- pums[which(pums$race == 2 & pums$sex == 1 & pums$age >18 & pums$incwage > 0),]
pums.whtmen <- pums[which(pums$race == 1 & pums$sex == 1 & pums$age >18 & pums$incwage > 0),]
pums.blkmen$logwage <- log(pums.blkmen$incwage)
pums.whtmen$logwage <- log(pums.whtmen$incwage)
pums.blkmen <- cast(melt(pums.blkmen[c("fips", "logwage")], id = c("fips")), fips ~ variable, mean, na.rm = TRUE)
pums.whtmen <- cast(melt(pums.whtmen[c("fips", "logwage")], id = c("fips")), fips ~ variable, mean, na.rm = TRUE)
pums.raceineq <- merge(x=pums.blkmen, y = pums.whtmen, by = "fips", all = TRUE)
names(pums.raceineq) <- c("fips", "blklwage1940", "whtlwage1940")

data(state.fips)
state.fips <- unique(state.fips[,c("fips","abb")])
state.fips$abb <- as.character(state.fips$abb)
state.fips <- rbind(state.fips, c(2, "AK"))
state.fips <- rbind(state.fips, c(15, "HI"))
rownames(state.fips) <- state.fips$abb
fips.state <- state.fips
rownames(fips.state) <- fips.state$fips
data(county.fips)
st.abbs <- read.csv("state-abbreviations.csv", stringsAsFactors = FALSE)
st.abbs$State <- tolower(st.abbs$State)
rownames(st.abbs) <- st.abbs$State
county.fips$polyname <- gsub(":.*$", "", county.fips$polyname)
county.fips$county.name <- unlist(lapply(strsplit(as.character(county.fips$polyname), split = ","), function (x) x[2]))
county.fips$state.name <- unlist(lapply(strsplit(as.character(county.fips$polyname), split = ","), function (x) x[1]))
county.fips$state.abb <- st.abbs[county.fips$state.name, "Abbreviation"]
county.fips$sfips <- state.fips[county.fips$state.abb, "fips"]
county.fips.u <- unique(county.fips)

## this code finds the fips code for the 1935 county based on string
## matching
pums$mcny5str <- tolower(pums$mcny5str)
pums$mcny5str[which(pums$mcny5str == "-")] <- NA
pums$mcny5str[which(pums$mcny5str == "\\")] <- NA
pums$mcny5str[which(pums$mcny5str == "")] <- NA
pums$mcny5str[which(pums$migrate5 %in% c(0,1))] <- NA
cnames <- unique(pums$mcny5str)

move.rows <- which(!is.na(pums$mcny5str))
pums$cname.match <- NA
pums$fips1935 <- NA

library(stringdist)
for (i in move.rows) {
  cat("this row: ", i, "\n")
  this.state <- county.fips.u[which(county.fips.u$sfips == pums$migplac5[i]),]
  match.rows <- amatch(pums$mcny5str[i], this.state$county.name, method = "jw", p = 0.1,
                       maxDist = 0.2)

  if (!is.na(match.rows)) {
    if (length(match.rows) == 1) {
      pums$fips1935[i] <- this.state$fips[match.rows]
      pums$cname.match[i] <- 0
    } else if (length(match.rows) > 1) {
      cat("multiple fuzzy matches: ", i, "\n")
    }
  }
}

pums$fips1935[is.na(pums$fips1935)] <- pums$fips[is.na(pums$fips1935)]
pums$statefip1935 <- as.numeric(substr(as.character(pums$fips1935), start = 1, stop = ifelse(nchar(as.character(pums$fips1935)) == 4, 1, 2)))
pums18w <- pums[which(pums$age >= 18 & pums$race == 1),]
pums18b <- pums[which(pums$age >= 18 & pums$race == 2),]


st.list <- c(1, 5, 12, 13, 21, 22, 28, 37, 45, 47, 48, 51, 54)
pums18w.south <- pums18w[which(pums18w$statefip %in% st.list | pums18w$statefip1935 %in% st.list),]
pums18b.south <- pums18b[which(pums18b$statefip %in% st.list | pums18b$statefip1935 %in% st.list),]

save(pums18w.south, file = "pums40-migration-white.RData")
save(pums18b.south, file = "pums40-migration-black.RData")


countydata <- read.csv("abs-jop-countydata.csv", stringsAsFactors = FALSE)
load("pums40-migration-white.RData")
load("pums40-migration-black.RData")

pums18w.south <- merge(pums18w.south, countydata, by = "fips")
pums18w.south <- merge(pums18w.south, countydata, by.x = "fips1935", by.y = "fips", suffixes = c("","1935"))

pums18b.south <- merge(pums18b.south, countydata, by = "fips")
pums18b.south <- merge(pums18b.south, countydata, by.x = "fips1935", by.y = "fips", suffixes = c("","1935"))

st.abb.list <- c("AL", "AR", "GA", "FL", "KY", "LA","MS", "MO", "NC", "SC", "TN", "TX", "VA", "WV")
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
  return(vcovCL)
}

marginal.se <- function(fm, xz, z0, vcv) {
  x <- strsplit(xz, ":")[[1]][1]
  z <- strsplit(xz, ":")[[1]][2]
  coef1 <- coef(fm)[x]
  coef2 <- coef(fm)[z]
  coef3 <- coef(fm)[xz]
  if (missing(vcv)) {
    vcv <- vcov(fm)
  }
  marg.eff <- coef1 + z0*coef3
  marg.se <- sqrt(vcv[x,x] + z0^2 * vcv[xz,xz] + 2 * z0 * vcv[xz,x])
  return(list(marg.eff = marg.eff, marg.se = marg.se))
}

pums18w.south$migrant <- 1 * (pums18w.south$fips != pums18w.south$fips1935)
pums18b.south$migrant <- 1 * (pums18b.south$fips != pums18b.south$fips1935)

out.base.covs <- formula( ~ migrant*pslave18601935 + log(coarea001935) + rugged1935 + latitude1935 + I(latitude1935^2)  + longitude1935 + I(longitude1935^2) + land.ineq18601935 + sfarmprop18601935 + log(totpop18601935)+ log(fvalpac18601935) + log(acimp18601935) + fbprop18601935 + rail18601935 + water18601935 + state.abb1935)

out.educ <- lm(update(out.base.covs, I(educ/sd(educ)) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.educ.cse <- robust.se(out.educ, "fips1935")
out.educ.hi <- marginal.se(out.educ, "migrant:pslave18601935", 0.4, out.educ.cse)
out.educ.lo <- marginal.se(out.educ, "migrant:pslave18601935", 0.1, out.educ.cse)
coeftest(out.educ, out.educ.cse)

out.higrade <- lm(update(out.base.covs, higrade ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.higrade.cse <- robust.se(out.higrade, "fips1935")
out.higrade.hi <- marginal.se(out.higrade, "migrant:pslave18601935", 0.4, out.higrade.cse)
out.higrade.lo <- marginal.se(out.higrade, "migrant:pslave18601935", 0.1, out.higrade.cse)
coeftest(out.higrade, out.higrade.cse)

out.age <- lm(update(out.base.covs, I(age/sd(age)) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.age.cse <- robust.se(out.age, "fips1935")
out.age.hi <- marginal.se(out.age, "migrant:pslave18601935", 0.4, out.age.cse)
out.age.lo <- marginal.se(out.age, "migrant:pslave18601935", 0.1, out.age.cse)
coeftest(out.age, out.age.cse)


out.sex <- lm(update(out.base.covs, I(sex==2) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.sex.cse <- robust.se(out.sex, "fips1935")
out.sex.hi <- marginal.se(out.sex, "migrant:pslave18601935", 0.4, out.sex.cse)
out.sex.lo <- marginal.se(out.sex, "migrant:pslave18601935", 0.1, out.sex.cse)
coeftest(out.sex, out.sex.cse)

out.nativity <- lm(update(out.base.covs, I(nativity==1) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.nativity.cse <- robust.se(out.nativity, "fips1935")
out.nativity.hi <- marginal.se(out.nativity, "migrant:pslave18601935", 0.4, out.nativity.cse)
out.nativity.lo <- marginal.se(out.nativity, "migrant:pslave18601935", 0.1, out.nativity.cse)
coeftest(out.nativity, out.nativity.cse)

out.bpl <- lm(update(out.base.covs, I(statefip1935 == bpl) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.bpl.cse <- robust.se(out.bpl, "fips1935")
out.bpl.hi <- marginal.se(out.bpl, "migrant:pslave18601935", 0.4, out.bpl.cse)
out.bpl.lo <- marginal.se(out.bpl, "migrant:pslave18601935", 0.1, out.bpl.cse)
coeftest(out.bpl, out.bpl.cse)

out.fbpl <- lm(update(out.base.covs, I(statefip1935 == fbpl) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.fbpl.cse <- robust.se(out.fbpl, "fips1935")
out.fbpl.hi <- marginal.se(out.fbpl, "migrant:pslave18601935", 0.4, out.fbpl.cse)
out.fbpl.lo <- marginal.se(out.fbpl, "migrant:pslave18601935", 0.1, out.fbpl.cse)
coeftest(out.fbpl, out.fbpl.cse)

out.mbpl <- lm(update(out.base.covs, I(statefip1935 == mbpl) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.mbpl.cse <- robust.se(out.mbpl, "fips1935")
out.mbpl.hi <- marginal.se(out.mbpl, "migrant:pslave18601935", 0.4, out.mbpl.cse)
out.mbpl.lo <- marginal.se(out.mbpl, "migrant:pslave18601935", 0.1, out.mbpl.cse)
coeftest(out.mbpl, out.mbpl.cse)

out.wkswork <- lm(update(out.base.covs, I(wkswork1/sd(wkswork1)) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.wkswork.cse <- robust.se(out.wkswork, "fips1935")
out.wkswork.hi <- marginal.se(out.wkswork, "migrant:pslave18601935", 0.4, out.wkswork.cse)
out.wkswork.lo <- marginal.se(out.wkswork, "migrant:pslave18601935", 0.1, out.wkswork.cse)
coeftest(out.wkswork, out.wkswork.cse)

out.incwage <- lm(update(out.base.covs, I(fwage1/sd(fwage1, na.rm = TRUE)) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list)
out.incwage.cse <- robust.se(out.incwage, "fips1935")
out.incwage.hi <- marginal.se(out.incwage, "migrant:pslave18601935", 0.4, out.incwage.cse)
out.incwage.lo <- marginal.se(out.incwage, "migrant:pslave18601935", 0.1, out.incwage.cse)
coeftest(out.incwage, out.incwage.cse)

out.rent <- lm(update(out.base.covs, I(rent/sd(rent, na.rm = TRUE)) ~ .), data = pums18w.south, subset = state.abb1935 %in% st.abb.list & rent < 9999)
out.rent.cse <- robust.se(out.rent, "fips")
out.rent.hi <- marginal.se(out.rent, "migrant:pslave18601935", 0.4, out.rent.cse)
out.rent.lo <- marginal.se(out.rent, "migrant:pslave18601935", 0.1, out.rent.cse)
coeftest(out.rent, out.rent.cse)

hi.eff <- c(out.educ.hi$marg.eff, out.age.hi$marg.eff, out.sex.hi$marg.eff,
            out.nativity.hi$marg.eff, out.bpl.hi$marg.eff, out.wkswork.hi$marg.eff, out.incwage.hi$marg.eff, out.rent.hi$marg.eff)

hi.ses <- c(out.educ.hi$marg.se, out.age.hi$marg.se, out.sex.hi$marg.se,
            out.nativity.hi$marg.se, out.bpl.hi$marg.se, out.wkswork.hi$marg.se, out.incwage.hi$marg.se, out.rent.hi$marg.se)

lo.eff <- c(out.educ.lo$marg.eff, out.age.lo$marg.eff, out.sex.lo$marg.eff,
            out.nativity.lo$marg.eff, out.bpl.lo$marg.eff, out.wkswork.lo$marg.eff, out.incwage.lo$marg.eff, out.rent.lo$marg.eff)

lo.ses <- c(out.educ.lo$marg.se, out.age.lo$marg.se, out.sex.lo$marg.se,
            out.nativity.lo$marg.se, out.bpl.lo$marg.se, out.wkswork.lo$marg.se, out.incwage.lo$marg.se, out.rent.lo$marg.se)


in.base.covs <- formula( ~ migrant*pslave1860 + log(coarea00) + latitude + I(latitude^2) + longitude + I(longitude^2)+ rugged  + land.ineq1860 + sfarmprop1860 + log(totpop1860) + log(fvalpac1860) + log(acimp1860) + fbprop1860  + rail1860 + water1860 + state.abb)

in.educ <- lm(update(in.base.covs, I(educ/sd(educ)) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.educ.cse <- robust.se(in.educ, "fips")
in.educ.hi <- marginal.se(in.educ, "migrant:pslave1860", 0.4, in.educ.cse)
in.educ.lo <- marginal.se(in.educ, "migrant:pslave1860", 0.1, in.educ.cse)
coeftest(in.educ, in.educ.cse)

in.higrade <- lm(update(in.base.covs, higrade ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.higrade.cse <- robust.se(in.higrade, "fips")
in.higrade.hi <- marginal.se(in.higrade, "migrant:pslave1860", 0.4, in.higrade.cse)
in.higrade.lo <- marginal.se(in.higrade, "migrant:pslave1860", 0.1, in.higrade.cse)
coeftest(in.higrade, in.higrade.cse)

in.age <- lm(update(in.base.covs, I(age/sd(age)) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.age.cse <- robust.se(in.age, "fips")
in.age.hi <- marginal.se(in.age, "migrant:pslave1860", 0.4, in.age.cse)
in.age.lo <- marginal.se(in.age, "migrant:pslave1860", 0.1, in.age.cse)
coeftest(in.age, in.age.cse)


in.sex <- lm(update(in.base.covs, I(sex==2) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.sex.cse <- robust.se(in.sex, "fips")
in.sex.hi <- marginal.se(in.sex, "migrant:pslave1860", 0.4, in.sex.cse)
in.sex.lo <- marginal.se(in.sex, "migrant:pslave1860", 0.1, in.sex.cse)
coeftest(in.sex, in.sex.cse)

in.nativity <- lm(update(in.base.covs, I(nativity==1) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.nativity.cse <- robust.se(in.nativity, "fips")
in.nativity.hi <- marginal.se(in.nativity, "migrant:pslave1860", 0.4, in.nativity.cse)
in.nativity.lo <- marginal.se(in.nativity, "migrant:pslave1860", 0.1, in.nativity.cse)
coeftest(in.nativity, in.nativity.cse)

in.bpl <- lm(update(in.base.covs, I(statefip == bpl) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.bpl.cse <- robust.se(in.bpl, "fips")
in.bpl.hi <- marginal.se(in.bpl, "migrant:pslave1860", 0.4, in.bpl.cse)
in.bpl.lo <- marginal.se(in.bpl, "migrant:pslave1860", 0.1, in.bpl.cse)
coeftest(in.bpl, in.bpl.cse)

in.fbpl <- lm(update(in.base.covs, I(statefip == fbpl) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.fbpl.cse <- robust.se(in.fbpl, "fips")
in.fbpl.hi <- marginal.se(in.fbpl, "migrant:pslave1860", 0.4, in.fbpl.cse)
in.fbpl.lo <- marginal.se(in.fbpl, "migrant:pslave1860", 0.1, in.fbpl.cse)
coeftest(in.fbpl, in.fbpl.cse)

in.mbpl <- lm(update(in.base.covs, I(statefip == mbpl) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.mbpl.cse <- robust.se(in.mbpl, "fips")
in.mbpl.hi <- marginal.se(in.mbpl, "migrant:pslave1860", 0.4, in.mbpl.cse)
in.mbpl.lo <- marginal.se(in.mbpl, "migrant:pslave1860", 0.1, in.mbpl.cse)
coeftest(in.mbpl, in.mbpl.cse)

in.wkswork <- lm(update(in.base.covs, I(wkswork1/sd(wkswork1)) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.wkswork.cse <- robust.se(in.wkswork, "fips")
in.wkswork.hi <- marginal.se(in.wkswork, "migrant:pslave1860", 0.4, in.wkswork.cse)
in.wkswork.lo <- marginal.se(in.wkswork, "migrant:pslave1860", 0.1, in.wkswork.cse)
coeftest(in.wkswork, in.wkswork.cse)

in.incwage <- lm(update(in.base.covs, I(fwage1/sd(fwage1, na.rm = TRUE)) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list)
in.incwage.cse <- robust.se(in.incwage, "fips")
in.incwage.hi <- marginal.se(in.incwage, "migrant:pslave1860", 0.4, in.incwage.cse)
in.incwage.lo <- marginal.se(in.incwage, "migrant:pslave1860", 0.1, in.incwage.cse)
coeftest(in.incwage, in.incwage.cse)

in.rent <- lm(update(in.base.covs, I(rent/sd(rent, na.rm = TRUE)) ~ .), data = pums18w.south, subset = state.abb %in% st.abb.list & rent < 9999)
in.rent.cse <- robust.se(in.rent, "fips")
in.rent.hi <- marginal.se(in.rent, "migrant:pslave1860", 0.4, in.rent.cse)
in.rent.lo <- marginal.se(in.rent, "migrant:pslave1860", 0.1, in.rent.cse)
coeftest(in.rent, in.rent.cse)

in.hi.eff <- c(in.educ.hi$marg.eff, in.age.hi$marg.eff, in.sex.hi$marg.eff,
               in.nativity.hi$marg.eff, in.bpl.hi$marg.eff, in.wkswork.hi$marg.eff,
               in.incwage.hi$marg.eff, in.rent.hi$marg.eff)

in.hi.ses <- c(in.educ.hi$marg.se, in.age.hi$marg.se, in.sex.hi$marg.se,
               in.nativity.hi$marg.se, in.bpl.hi$marg.se, in.wkswork.hi$marg.se,
               in.incwage.hi$marg.se, in.rent.hi$marg.se)

in.lo.eff <- c(in.educ.lo$marg.eff, in.age.lo$marg.eff, in.sex.lo$marg.eff,
               in.nativity.lo$marg.eff, in.bpl.lo$marg.eff, in.wkswork.lo$marg.eff,
               in.incwage.lo$marg.eff, in.rent.lo$marg.eff)

in.lo.ses <- c(in.educ.lo$marg.se, in.age.lo$marg.se, in.sex.lo$marg.se,
               in.nativity.lo$marg.se, in.bpl.lo$marg.se, in.wkswork.lo$marg.se,
               in.incwage.lo$marg.se, in.rent.lo$marg.se)

##cairo_pdf("figs/migration1940.pdf", family = "Minion Pro", height = 4, width = 7, pointsize = 9)
cov.labs <- c("Education", "Age", "Female", "Nativity", "Living in birthstate", "Weeks Worked", "Wages", "Monthly Rent")
layout(mat = matrix(c(1,1,2,3), nrow = 2, byrow=TRUE), heights = c(0.1,0.9))
par(mar = c(0,0,0,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x="top", bty = "n", col = c("black", "red"), pch = c(19,17), legend = c("High Prop Slave", "Low Prop Slave"), lty = 1, horiz = TRUE)
xwindow <- range(c(in.hi.eff + 2*in.hi.ses, in.hi.eff - 2*in.hi.ses, in.lo.eff + 2*in.lo.ses, in.lo.eff - 2*in.lo.ses))
par(mar = c(4.5,8.5,1,1))
plot(x = hi.eff, y = 1:length(hi.eff), xlim = xwindow, ylim = c(0,length(hi.eff) + 1), pch = 19, yaxt = "n", bty = "n", ylab = "", main = "Out-migration", xlab = "Migrants vs. Non-migrants")
axis(side = 2, at = 1:length(hi.eff) - .125, labels = cov.labs, las = 1)
points(x = lo.eff, y = (1:length(lo.eff)) - 0.25, pch = 17, col = "red")
segments(x0 = hi.eff - 1.96 * hi.ses, x1 = hi.eff + 1.96 * hi.ses, y0 = 1:length(hi.eff))
segments(x0 = lo.eff - 1.96 * lo.ses, x1 = lo.eff + 1.96 * lo.ses, y0 = 1:length(lo.eff) - 0.25, col = "red")
abline(v=0, col = "grey70", lty = 2)
plot(x = in.hi.eff, y = 1:length(in.hi.eff), xlim = xwindow, ylim = c(0,length(in.hi.eff)+1), pch = 19, yaxt = "n", bty = "n", ylab = "", main = "In-migration", xlab = "Migrants vs. Non-migrants")
axis(side = 2, at = 1:length(in.hi.eff) - .125, labels = cov.labs, las = 1)
points(x = in.lo.eff, y = (1:length(in.lo.eff)) - 0.25, pch = 17, col = "red")
segments(x0 = in.hi.eff - 1.96 * in.hi.ses, x1 = in.hi.eff + 1.96 * in.hi.ses, y0 = 1:length(in.hi.eff))
segments(x0 = in.lo.eff - 1.96 * in.lo.ses, x1 = in.lo.eff + 1.96 * in.lo.ses, y0 = 1:length(in.lo.eff) - 0.25, col = "red")
abline(v=0, col = "grey70", lty = 2)
##dev.off()
