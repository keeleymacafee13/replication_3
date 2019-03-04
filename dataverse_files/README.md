## Introduction

Glad to have you here! This is the replication archive for

Acharya, A., Blackwell, M., and Sen, M. 2016. The Political Legacy of American Slavery. In press, Journal of Politics. 

See below for details on the data files, R scripts, and specific variables in this archive. This file is written in markdown and you will find many of the relevant links to external data at the bottom of this file. If you have any questions or concerns, please reach out to Matthew Blackwell (mblackwell@gov.harvard.edu, http://www.mattblackwell.org).

## Data files

* abs-jop-cces-ind.csv: individual-level CCES data with county variables merged
* abs-jop-cces-white-countydata.csv: county-level data with aggregated white CCES responses merged
* abs-jop-countydata.csv: county-level data with no CCES or NES data merged
* abs-jop-nes-ind.csv: individual-level NES data with county variables merged
* abs-jop-nes-white-countydata.csv: county-level data with aggregated white NES responses merged
* `usa_00015_1940_census.csv.gz`: IPUMS data from the 1940 U.S. Census with migration for Figure 3.
* outtxt_flow.txt: data from the 2000 Census on outflows from county to county [Source][ctytocty]

## Scripts

* slavery-jop-replication.R: code to produce all tables and figures in Acharya, Blackwell & Sen (JOP 2016), except for Figures 1, 3, 6, A.1, A.3
* slavery-maps-replication.R: code to produce maps in the paper (Figure 1, A.1). Some shapefiles from the [Atlas of Historical County Boundaries][atlas] are needed to produce these figures
* slavery-migration-40s.R: code to produce Figure 3. Data comes from IPUMS and is in a gzip file, which is loaded directly from R. No need to extract the raw data. 
* slavery-migration-today.R: code to produce Figure A.3
* slavery-youth-parent.R: code to produce Figure 6. Requires data from [ICPSR 4532][].
* interpolation.R: code to reproduce the aerial interpolation done for the paper. Requires files from the [Atlas of Historical County Boundaries][atlas], [Jeremy Atack][atack], and the [Haines ICPSR][haines] repository.

## County variable sources & descriptions

* state.abb: state abbreviations
* fips: 2-digit state FIPS code + 3-digit county FIPS code
* county_name: name of county (2000 Census, [Haines ICPSR][haines], DS81)
* pslave1860:  proportion slave, 1860 (interpolated to 2000) 
* longitude: longitude of county ([2000 Census Shapefile][2000shp])
* latitude: latitude of county ([2000 Census Shapefile][2000shp])
* coarea00: area of county ([2000 Census Shapefile][2000shp])
* coarea: area (square miles) of county (2000 Census, [Haines ICPSR][haines], DS81)
* rugged: Hornbeck & Naidu [Replication Archive][hornbeck-naidu]
* land.ineq1860: Gini coefficients on land holdings, 1860 interpolated to 2000
* sfarmprop1860: proportion farms under 50 acres, 1860 interpolated to 2000
* totpop1860: total county population, 1860 interpolated to 2000
* fvalpc1860: farm value per capita, 1860 interpolated to 2000
* fvalpac1860: farm value per improve acre, 1860 interpolated to 2000
* acimp1860: total improved acres in county, 1860 interpolated to 2000
* fbprop1860: proportion free black, 1860 interpolated to 2000
* rail1860: rail access in 1860 based on 2000 county boundaries ([Atack shapefiles][atack])
* water1860: navigable waterway access in 1860, based on 2000 county boundaries ([Atack shapefiles][atack])
* sprop1850: proportion slave, 1850 (contemporaneous boundaries)
* sprop: proportion slave, 1860 (contemporaneous boundaries)
* cottonsuit: cotton suitability measure ([FAO][])
* nmatch.diff.20: dummy for if a county has a neighbor that differs by > 20 pp enslaved in 1860
* blkprop1870: proportion black, 1870
* blkprop20: proportion black, 1920
* blkprop40: proportion black, 1940
* blkprop70: proportion black, 1970
* blkprop00: proportion black, 2000
* blkprop2010: proportion black, 2010 (2010 Census, American FactFinder)
* totpop00: total county population, 2000
* highsch90: proportion of population with HS degree or higher, 1990 ([Haines ICPSR][haines], DS81)
* unemp: unemployment rate in 1999 ([BLS][])
* medinc00: median county income, 1997 ([Haines ICPSR][haines], DS81)
* wbincratio00: ratio of white median income to black median income, 2000 (2000 Census)
* slavesperhouse: average ratio of total slave holdings to number of slave quarters across farms within county, 1860 ([IPUMS Slave Samples][ipums-slave])
* s.mortrate: slave deaths per 100000 slaves in the county, 1860 (1860 Census Mortality Schedules)
* w.mortrate: white deaths per 100000 whites in the county, 1860 (1860 Census Mortality Schedules)
* livstock: total value of farm livestock, 1860
* livstock1870: total value of farm livestock, 1870
* farmval: total cash value of farms, 1860
* farmval1870: total cash value of farms, 1870
* totpop1880: total county population, 1880
* totpop1890: total county population, 1890
* totpop10: total county population, 1910
* totpop20: total county population, 1920
* totpop30: total county population, 1930
* totpop40: total county population, 1940
* totpop50: total county population, 1950
* lynchings: total number of lynchings, 1882--1930 ([Project HAL][])
* lynchrate: lynchings divided by 1920 county population
* tractors40: tractors per county acre, 1940 ([1940 Agricultural Census][haines-agri])
* tractors30: tractors per county acre, 1930 ([1930 Agricultural Census][haines-agri])
* blklwage1940: median log wages of black men, 1940 (1940 Census, IPUMS)
* whtlwage1940: median log wages of white men, 1940 (1940 Census, IPUMS)
* btenantprop25: percent of black-operated farms in tenancy, 1925 Agricultural Census
* wtenantprop25: percent of white-operated farms in tenancy, 1925 Agricultural Census
* coacres25: 
* bfarmerownerprop25: percent of black-operated farms owned by operator, 1925 Agricultural Census
* wfarmerownerprop25: percent of black-operated farms owned by operator, 1925 Agricultural Census
* land.ineq: Gini coefficient on farm holdings, 1860
* sfarmprop: proportion of farms smaller than 50 acres, 1860
* totpop: total population, 1860
* fvalpc: total farm value per capita, 1860
* acimp: total improved acreage, 1860
* fbprop: proportion free black, 1860
* rail: rail access, 1860
* water: waterway access, 1860
* pdem1840-pdem1964: percent of total vote for the Democratic presidential candidate (or equivalent), various years ([Clubb et al ICPSR][clubb]) 
* breckinridge1860: percent of total vote for Breckinridge in the 1860 election ([Clubb et al ICPSR][clubb]) 
* wallace68.alt: total votes for George Wallace in 1968 ([Clubb et al ICPSR][clubb])  divided by white potential vote in 1960 (1960 Census)
* thurmond48: percent of the total vote to Strom Thurmond in 1948
* wht.obama.vote: estimated share of the white vote accrued to Obama, 2008
* stot1860: total slaves in 1860, interpolated to 2000
* whtot1860: total whites in 1860, interpolated to 2000
* wbincratio2014: ratio of white median income to black median income, 2014 (5-year ACS)
* w.med.income2014: white median income, 2014 (5-year ACS)
* b.med.income2014: black median income, 2014 (5-year ACS)
* outflow: total outflow from county, 2005-2009 ([ACS Migration][])
* inflow: total inflow to county, 2005-2009 ([ACS Migration][])

## CCES individual measures

* dem: 1 if respondent identifies with the Democratic party (including leaners), 0 otherwise
* rep: 1 if respondent identifies with the Republican party (including leaners), 0 otherwise
* affirm: 1 if respondent supports affirmative action, 0 otherwise (from 5-point scale)
* resent: in 2010, average of the two CCES racial resentment questions (diagreement with the "Generations of slavery" question and agreement with the "The Irish, Italians, and Jews" question) on a 5-point scale. In 2011, only agreement with the "The Irish..." question.
* educ: 6 category education variable (1=no HS, 2=HS, 3=Some College, 4=2-year College Degree, 5=4-year College Degree, 6=Postgrad Degree)
* inc.cat: categorical variable for income (character variable, see data for levels)
* inc.mid: income coded at the mid-point of the CCES bins (see CCES documentation for bins)
* religion: 1 if respondent said religion was "Very Important" or "Somewhat Important," 0 otherwise (in the 2006 CCES this variable is binary and we take "Important" to be a 1)
* female: 1 if the respondent is female, 0 if the respondent is male
* age: respondent age in years
* year: wave of the CCES survey
* white: 1 if the respondent self-identifies as white, 0 otherwise
* black: 1 if the respondent self-identifies as black, 0 otherwise
* latino: 1 if the respondent self-identifies as Hispanic or Latino, 0 otherwise
* weights: CCES survey weights

## Contextual measures for CCES respondents

* blkprop.z00: proportion black in respondent zip code, 2000 (2000 US Census)
* medinc.z10: median income in respondent zip code, 2008-2013 (ACS, 2008-2013)
* w.unemp.rate2014: white unemployment rate in respondent county, 2010-2014 (ACE, 2010-2014)

## CCES aggregated variables

* dem, rep, affirm, resent: weighted within-county averages of the individual-level measure, using the survey weights
* sample.size: sum of the within-county sampling weights (weights)
* sample.size.res: sum of the within-county sampling weights (weights) for respondents to the racial resentment questions

## NES individual measures

* wtherm: thermometer score for whites as a group (0-100)
* btherm: thermometer score for blacks as a group (0-100)
* therm.diff: difference between wtherm and btherm
* age: age in years
* inc.mid: income coded to the midpoint of the ANES bins (see ANES documentation for exact bins)
* religion: 1 if respondent said religion was "Important" to their lives, 0 otherwise
* white: 1 if the respondent self-identifies as white, 0 otherwise
* black: 1 if the respondent self-identifies as black, 0 otherwise
* weight: ANES survey weight
* year: wave of the CCES survey

## NES aggregated measures

* wtherm, btherm, therm.diff: weighted within-county averages of the individual-level measure, using the survey weights
* sample.size: sum of the within-county sampling weights (weight)
* sample.size.td: sum of the within-county sampling weights (weight) for respondents to both thermometer score questions
* sample.size.wt: sum of the within-county sampling weights (weight) for respondents to the wtherm question
* sample.size.bt: sum of the within-county sampling weights (weight) for respondents to the btherm question

## White Obama Vote Share

To calculate the white Obama vote, we took his total vote within a county subtracted off the estimated black vote for Obama. This quantity was calculated as the proportion of black respondents saying they voted for Obama in the CCES in that state multiplied by the total black turnout in that county, as measured by Catalist LLC. We divided this number by the total two-party vote minus the black turnout as measured Catalist. 

[2000shp]: https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
[haines]: http://www.icpsr.umich.edu/icpsrweb/DSDR/studies/2896
[hornbeck-naidu]: https://www.aeaweb.org/articles.php?doi=10.1257/aer.104.3.963
[atack]: https://my.vanderbilt.edu/jeremyatack/data-downloads/
[FAO]: http://www.fao.org/nr/gaez/en/
[BLS]: http://www.bls.gov/lau/#cntyaa
[ipums-slave]: https://usa.ipums.org/usa/slavepums/index.html
[Project HAL]: http://people.uncw.edu/hinese/HAL/HAL%20Web%20Page.htm
[haines-agri]: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35206
[clubb]: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/08611
[ACS Migration]: https://www.census.gov/hhes/migration/files/acs/county-to-county/CtyxCty_US.txt
[atlas]: http://publications.newberry.org/ahcbp/
[ICPSR 4532]: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/4532
[ctytocty]: https://www.census.gov/population/www/cen2000/ctytoctyflow/
