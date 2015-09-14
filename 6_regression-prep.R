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

# load data
meta.topics <- read.csv("Data/meta-topics.csv")
meta.topics$X <- NULL
names(meta.topics)

# prep docs
docs <- subset(meta.topics,select=c(1:15,COUNTRY_CODE,YEAR,REGION,number.of.non.stop.words))
names(docs)[16:18] <- c("iso3c","year","region")
# from topic proportions to number of words
docs[,1:15] <- docs[,1:15]*docs$number.of.non.stop.words
sum(docs[1,1:15]) # 306

#################################################
###### 1) Topic quantities by country.year ######
#################################################

## Various ways to operationalize the DV:
# Option 1) Mean topic distribution per country.year (weighted by words per doc)

country.year.means <- ddply(.data=docs, .variables=.(iso3c,year), numcolwise(sum,na.rm = TRUE))
# normalize
country.year.means[,3:17] <- country.year.means[,3:17]/country.year.means$number.of.non.stop.words
# add 'n = number docs' column
country.year.means$n <- (ddply(.data=docs,.variables=.(iso3c,year), .fun=nrow))$V1 
# test
head(country.year.means) 
sum(country.year.means[1,3:17]) # should be 1
write.csv(country.year.means,"Results/stm/country.year-means.csv")

# Option 2) Total Number of Words Devoted to Each Topic per country.year
country.year.sums <- ddply(.data=docs, .variables=.(iso3c,year), numcolwise(sum,na.rm = TRUE))
# add 'n = number docs' column
country.year.sums$n <- (ddply(.data=docs,.variables=.(iso3c,year), .fun=nrow))$V1 
# test
head(country.year.sums) 
sum(country.year.sums[3,3:17]) # sum words for 3strow = 1390
write.csv(country.year.means,"Results/stm/country.year-sums.csv")

############################
### Region-Year Playing ###
############################

# region-year means
region.year.means <- ddply(.data=docs, .variables=.(year,region), numcolwise(sum,na.rm = TRUE))
region.year.means[3:17] <- region.year.means[3:17]/region.year.means$number.of.non.stop.words
region.year.means$n <- (ddply(.data=docs,.variables=.(year,region), .fun=nrow))$V1 
write.csv(region.year.means,"Results/stm/region-year-means.csv")

# region-year sums
region.year.sums <- ddply(.data=docs, .variables=.(year,region), numcolwise(sum,na.rm = TRUE))
region.year.sums$n <- (ddply(.data=docs,.variables=.(year,region), .fun=nrow))$V1 
write.csv(region.year.means,"Results/stm/region-year-sums.csv")

# plotting
ggplot(data=region.year.sums, aes(x=year,y=religion,group=region,color=region)) + geom_line()

#################################
###### Assign DV and explore ####
#################################

# assign 2 Rights DVs
country.year <- cbind(country.year.means[,c(1,2,14)], country.year.sums[,c(14,18,19)])
names(country.year) <- c("iso3c", "year","rights.mean","rights.sum","n.words","n.docs")

# duplicates?
x <- data.frame(cbind(country.year$iso3c,country.year$year))
which(duplicated(x))

# see countries with highest means
country.means <- ddply(country.year, .(iso3c), summarize, mean = mean(rights.mean,na.rm=T))
country.means <- arrange(country.means,desc(mean))
head(country.means,10)
ggplot(country.means, aes(x = iso3c, y = mean)) + geom_bar(stat = "identity")

###############################################
###### 2) Prepare Country-level variables #####
###############################################

## Load country.year database
rt <- read.csv("../country-year-database/rt.no.us.csv")
rt.null <- rt

## merge with rights
rt <- merge(rt,country.year,by=c("year","iso3c"),all.x=T)
names(rt)

# merge with NYT count
nyt <- read.csv("Data/Regression-Data/n_nyt.csv")
nyt.sub <- nyt[,c("count", "ccode", "year")]
rt <- merge(rt, nyt.sub, by = c("ccode", "year"))
summary(rt$count)

## countries in text but not rt
which(!country.year$iso3c %in% rt$iso3c)

## number unique countries
length(unique(rt$rt_code)) # 197

## write CSV of all country-year observations
x <- rt[,c("year","country","ccode","iso3c")]
write.csv(x,"Data/Regression-Data/country-year-obs.csv")

## MENA dummy variable
rt$mena <- 0
rt$mena[rt$region=="MENA"] <- 1
summary(rt$mena)

## majority Muslim dummy
rt$muslim.maj <- 0
rt$muslim.maj[rt$muslim>=.50] <- 1
summary(rt$muslim.maj)

## Adjust n from NA to 0
rt$n.doc[is.na(rt$n.docs)] <- 0
rt$n.doc[rt$year==1979] <- NA
summary(rt$n.doc)

## Make n binary
rt$n.binary <- rt$n.docs
rt$n.binary[rt$n.docs > 0] <- 1
rt$n.binary[is.na(rt$n.docs)] <- 0
rt$n.binary <- as.factor(rt$n.binary)
summary(rt$n.binary)

### subset 
rt <- rt[,c("ccode","year","n.docs","n.binary","rights.sum","rights.mean","muslim","muslim.maj","mena","polity2","physint","amnesty","statedept","gdp.pc.un","pop.wdi","wopol","wosoc","wecon","domestic9","lnreportcount", "count","region")]

# Write
rt.orig <- rt
write.csv(rt.orig,"Data/Regression-Data/regression-rights.csv", row.names = F)

# Test
cor(rt$muslim, rt$mena, use="complete.obs") #0.6626975
cor(rt$amnesty,rt$statedept,use="complete.obs") #0.8019385
cor(rt$lnreportcount,rt$count,use="complete.obs")
summary(rt$rights.mean)
summary(rt$rights.sum)

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
write.csv(rt.nearest, "Data/Regression-Data/regression-rights-nearest.csv", row.names = F)

#rt.nearest <- read.csv("Data/country-year/country-year-rights-nearest.csv")

# Option 2)  Amelia method
##########################

rt <- rt.orig
names(rt)
# subset
rt <- rt[,c(1,2,10:20)]
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
rt.impute$rights.sum <- rt.orig$rights.sum
rt.impute$rights.mean <- rt.orig$rights.mean
rt.impute$n.docs <- rt.orig$n.docs
rt.impute$n.binary <- rt.orig$n.binary
rt.impute$region <- rt.orig$region
rt.impute$muslim.maj <- rt.orig$muslim.maj
rt.impute$muslim <- rt.orig$muslim
rt.impute$mena <- rt.orig$mena
rt.impute$count <- rt.orig$count

# test
cor(rt.impute$muslim, rt.impute$mena, use="complete.obs") #0.6366512

## write
write.csv(rt.impute,"Data/Regression-Data/regression-rights-imputed.csv", row.names = F)
