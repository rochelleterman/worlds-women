# This script:
# 1) Finds mean topic proportions for country.year and region-year levels using a variety of metrics
# 2) Merges country.year topic proportions with other country.year variables
# 3) Imputes missing values using nearest-value and Amelia techniques

rm(list=ls())
setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

library(plyr)
library("MASS")
library("xtable")
library(plm)
library(ggplot2)
library(reshape2)
library(stargazer) # pretty tables
library(statar) # for missing values
library(sandwich) # for robust standard errors
library(lmtest) # for robust standard errors
library(Amelia) # for missing valus
library(data.table)  # for missing valus
library(sampleSelection)


meta.topics <- read.csv("Data/meta-topics.csv")
meta.topics$X <- NULL
meta.topics$X.1 <- NULL

#################################################
###### 1) Topic quantities by country.year ######
#################################################

## Various ways to operationalize the DV:

# Option 1) Mean topic distribution per country.year (weighted by words per doc)
x <- subset(meta.topics,select=c(business:politics,COUNTRY_CODE,YEAR,REGION,number.of.non.stop.words))
names(x)[16:18] <- c("iso3c","year","region")
x[,1:15] <- x[,1:15]*x$number.of.non.stop.words
country.year.means <- ddply(.data=x, .variables=.(iso3c,year), numcolwise(mean,na.rm = TRUE))
# add 'number docs' column
y <- ddply(.data=x,.variables=.(iso3c,year), .fun=nrow) 
country.year.means$n <- y$V1
head(country.year.means) 
sum(country.year.means[1,3:17]) # 182
write.csv(country.year.means,"Results/15.1/country.year-means.csv")

# Option 2) Mean topic distribution per country.year (NOT weighted by words per doc)
x <- subset(meta.topics,select=c(business:politics,COUNTRY_CODE,YEAR,REGION,number.of.non.stop.words))
names(x)[16:18] <- c("iso3c","year","region")
country.year.means.unweighted <- ddply(.data=x, .variables=.(iso3c,year), numcolwise(mean,na.rm = TRUE))
# add 'number docs' column
y <- ddply(.data=x,.variables=.(iso3c,year), .fun=nrow) 
country.year.means.unweighteds$n <- y$V1
head(country.year.means.unweighted) 
sum(country.year.means.unweighted[1,3:17]) # test

# Option 3) Mean Number of Top-Topic Documents per country.year
x <- subset(meta.topics,select=c(COUNTRY_CODE,YEAR,REGION,top.topic))
names(x)[1:3] <- c("iso3c","year","region")
country.year.top <- ddply(.data=x, .variables=.(iso3c,year,top.topic), .fun=nrow)
sum(country.year.top$V1) # testing
country.year.top <- dcast(country.year.top,iso3c + year ~ top.topic)
head(country.year.top,20)
# add 'n' column
y <- ddply(.data=x,.variables=.(iso3c,year), .fun=nrow) 
country.year.top$n <- y$V1
# take mean TODO: Or Make NA <- 0?
country.year.top[,3:17] <- country.year.top[,3:17]/country.year.top$n
head(country.year.top)

# Option 4) Total Number of Words Devoted to Each Topic per country.year
x <- subset(meta.topics,select=c(business:politics,COUNTRY_CODE,YEAR,REGION,number.of.non.stop.words))
names(x)[16:18] <- c("iso3c","year","region")
x[,1:15] <- x[,1:15]*x$number.of.non.stop.words
 
###  Region-Year Playing ###
############################

# region-year means
region.year.means <- ddply(.data=x, .variables=.(year,region), numcolwise(mean,na.rm = TRUE))
y <- ddply(.data=x,.variables=.(year,region), .fun=nrow) 
region.year.means$n <- y$V1
write.csv(region.year.means,"Results/15.1/region-year-means.csv")

# plotting
ggplot(data=region.year.means, aes(x=year,y=rights,group=region,color=region)) + geom_line()

#################################
###### Assign DV and explore ####
#################################

#decide which one you're going to use for rest of analysis
country.year <- country.year.means

# duplicates?
x <- data.frame(cbind(country.year$iso3c,country.year$year))
which(duplicated(x))

# see countries with highest means
country.means <- ddply(country.year, .(iso3c), summarize, mean = mean(rights,na.rm=T))
country.means <- arrange(country.means,desc(mean))
head(country.means,10)
ggplot(country.means, aes(x = iso3c, y = mean)) + geom_bar(stat = "identity")

###############################################
###### 2) Prepare Country-level variables #####
###############################################

## Load country.year database
rt <- read.csv("../country-year-database/rt.no.us.csv")
names(rt)
rt.null <- rt

## merge
rt <- merge(rt,country.year,by=c("year","iso3c"),all.x=T)

## countries in text but not rt
which(!country.year$iso3c %in% rt$iso3c)
country.year$iso3c[1238]

## number unique countries
length(unique(rt$rt_code)) # 197

## write CSV of all country-year observations
x <- rt[,c("year","country","ccode","iso3c")]
write.csv(x,"Data/country-year-obs.csv")

## MENA dummy variable
rt$mena <- 0
rt$mena[rt$region=="MENA"] <- 1
summary(rt$mena)

## majority Muslim dummy
rt$muslim.maj <- 0
rt$muslim.maj[rt$muslim>=.50] <- 1
summary(rt$muslim.maj)

## Adjust n from NA to 0
rt$n[is.na(rt$n)] <- 0
rt$n[rt$year==1979] <- NA
summary(rt$n)

## Make n binary
rt$n.binary <- rt$n
rt$n.binary[rt$n > 0] <- 1
rt$n.binary <- as.factor(rt$n.binary)
summary(rt$n.binary)

### subset 
rt <- rt[,c("ccode","year","n","n.binary","rights","muslim","muslim.maj","mena","polity2","physint","amnesty","statedept","gdp.pc.un","pop.wdi","wopol","wosoc","wecon","domestic9","lnreportcount", "region")]

# Write
rt.orig <- rt
write.csv(rt.orig,"Data/regression-data/regression-rights.csv", row.names = F)

# Test
cor(rt$muslim, rt$mena, use="complete.obs") #0.6626975
cor(rt$amnesty,rt$statedept,use="complete.obs") #0.8019385

#############################
##### 3) Missing Values #####
#############################

rt <- rt.orig

# Option 1) Imputing values using nearest value
###############################################

# write function that passes variable and fills in missing values based on nearest value

# NEEDS WORK

rt <- rt.orig
rt <- arrange(rt, ccode)
summary(rt$muslim)

impute <- function(var){
  DT <- data.table(
    id    = rt$ccode,
    date  = as.numeric(as.character(rt$year)),
    value = var
  )
  setna(DT, along_with = date, by = id, roll="nearest")
  return(DT$value)
}

rt$physint <- impute(rt$physint)
rt$statedept <- impute(rt$statedept)
rt$amnesty <- impute(rt$amnesty)
rt$polity2 <- impute(rt$polity2)
rt$gdp.pc.un <- impute(rt$gdp.pc.un)
rt$domestic9<- impute(rt$domestic9)
rt$wopol <- impute(rt$wopol)
rt$wosoc <- impute(rt$wosoc)
rt$wecon <- impute(rt$wecon)
rt$pop.wdi <- impute(rt$pop.wdi)
rt$lnreportcount <- impute(rt$lnreportcount)

summary(rt$lnreportcount)
summary(rt.orig$lnreportcount)

cor(rt$muslim, rt$mena, use="complete.obs") #0.6365522

# save and write
rt.nearest <- rt
write.csv(rt.nearest, "Data/regression-data/regression-rights-nearest.csv", row.names = F)

#rt.nearest <- read.csv("Data/country-year/country-year-rights-nearest.csv")

# Option 2)  Amelia method
##########################

rt <- rt.orig

# subset
rt <- rt[,c(1,2,9:19)]
rt$year <- as.integer(as.character((rt$year)))
# model 1
set.seed(1234)
a.out <- amelia(x=rt,cs="ccode",ts="year",
                log=c("gdp.pc.un","pop.wdi"))
# model 2
a.out.time <- amelia(rt, ts = "year", cs = "ccode", 
                     log=c("gdp.pc.un","pop.wdi"), polytime = 1)
# plot
tscsPlot(a.out.time, cs = "20",
         var = "wosoc", ylim = c(-1, 4))
tscsPlot(a.out, cs = "20",
         var = "wosoc", ylim = c(-1, 4))
# Extract the imputed datasets from the amelia object
datasets<-a.out$imputations
# map missing values
missmap(a.out)
# diagnostics
overimpute(a.out, var = "wosoc")
# take 1st imputation
rt.impute <- a.out$imputations[[1]]
# overwrite dependent variables -- we don't want to impute those.
rt.impute$rights <- rt.orig$rights
rt.impute$n <- rt.orig$n
rt.impute$n.binary <- rt.orig$n.binary
rt.impute$region <- rt.orig$region
rt.impute$muslim.maj <- rt.orig$muslim.maj
rt.impute$muslim <- rt.orig$muslim
rt.impute$mena <- rt.orig$mena

# test
cor(rt.impute$muslim, rt.impute$mena, use="complete.obs") #0.6366512

## write
write.csv(rt.impute,"Data/regression-data/regression-rights-imputed.csv", row.names = F)
