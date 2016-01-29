## Merge corpus with country-year variables to create two datasets:
## 1. unit of observation: country-year
## 2. unit of observation: document

# prep
rm(list=ls())
setwd("~/Dropbox/berkeley/Git-Repos/worlds-women")

require(plyr)
require("MASS")
require("xtable")
require(plm)
require(ggplot2)
require(reshape2)
require(stargazer) # pretty tables
require(statar) # for missing values
require(sandwich) # for robust standard errors
require(lmtest) # for robust standard errors
require(Amelia) # for missing valus
require(data.table)  # for missing valus
require(sampleSelection)

# load data
meta.topics <- read.csv("Data/topic-proportions/meta-topics.csv")
names(meta.topics)

# prep docs
docs <- subset(meta.topics,select=c(1:15,iso3c,year,region,n.words))

# from topic proportions to number of words
docs[,1:15] <- docs[,1:15]*docs$n.words
sum(docs[1,1:15]) # 331

##########################################
###### 1) Prepare Corpus Topic Data ######
##########################################

# Option 1) Mean topic distribution per country.year (weighted by words per doc)
country.year.means <- ddply(.data=docs, .variables=.(iso3c,year), numcolwise(sum,na.rm = TRUE))
country.year.means[,3:17] <- country.year.means[,3:17]/country.year.means$n.words # normalize
country.year.means$n.docs <- (ddply(.data=docs,.variables=.(iso3c,year), .fun=nrow))$V1  # add 'n = number docs' column
head(country.year.means) # test
sum(country.year.means[1,3:17]) # should be 1

# Option 2) Boolean label - rights or not - on article using DTM
rights.boolean <- read.csv("Data/topic-proportions/alt-dv-country-year.csv")
rights.boolean <- rights.boolean[,c("iso3c", "year", "alt.dv.perc")]

# see countries with highest means for "rights"
country.means <- ddply(country.year.means, .(iso3c), summarise, mean = mean(rights,na.rm=T))
country.means <- arrange(country.means,desc(mean))
head(country.means,10)
ggplot(country.means, aes(x = iso3c, y = mean)) + geom_bar(stat = "identity")

##########################################
###### 2) Prepare Country-level Data #####
##########################################

## Load country.year database
rt <- read.csv("../country-year-database/rt.no.us.csv")

## number unique countries
length(unique(rt$ccode)) # 199

## write CSV of all country-year observations
x <- rt[,c("year","country","ccode","iso3c")]
write.csv(x,"NYT-scraping/country-year-obs.csv")

# merge with NYT count
nyt <- read.csv("NYT-scraping/n_nyt.csv")
nyt.sub <- nyt[,c("count", "ccode", "year")]
nyt.sub <- nyt.sub[-which(duplicated(nyt.sub)),]
rt <- merge(rt, nyt.sub, by = c("ccode", "year"), all.x = T)

## MENA dummy variable
rt$mena <- 0
rt$mena[rt$region=="MENA"] <- 1
rt$mena <- as.factor(rt$mena)
summary(rt$mena)

## majority Muslim dummy
rt$muslim.maj <- 0
rt$muslim.maj[rt$muslim>=.50] <- 1
rt$muslim.maj[is.na(rt$muslim)] <- NA
summary(rt$muslim.maj)
summary(rt$muslim)

unique(rt$country[is.na(rt$muslim)])

# save a copy of rt
rt.null <- rt

#####################################################
###### 3) Merging: Document Unit of Observation #####
#####################################################

# remove duplicate columns
rt$country <- NULL
rt$region <- NULL

# recode meta.topics iso3c
levels(meta.topics$iso3c) <- c(levels(meta.topics$iso3c), "CZE")
meta.topics$iso3c[meta.topics$country=="Czechoslovakia" & meta.topics$year > 1993] <- "CZE"
meta.topics$iso3c[meta.topics$country=="Hong Kong"] <- "CHN"

# merge with topics
meta.topics.country.year <- merge(meta.topics,rt,by=c("year","iso3c"),all.x=T)
names(meta.topics.country.year)

# entries left out in rt
unique(cbind(as.character(meta.topics.country.year$country[is.na(meta.topics.country.year$mena)]),meta.topics.country.year$year[is.na(meta.topics.country.year$mena)]))

# rearrange columns
meta.topics.country.year <- meta.topics.country.year[,c(3:17,1:2,18:60)]

# write
write.csv(meta.topics.country.year, "Data/topic-proportions/meta-topics-country-year.csv", row.names = F)

#########################################################
###### 4) Merging: Country-Year Unit of Observation #####
#########################################################

# get back old rt
rt <- rt.null

## merge with topics
rt <- merge(rt,country.year.means,by=c("year","iso3c"),all.x=T)
summary(rt$mena)

## merge with alt.dv
rt <- merge(rt,rights.boolean,by=c("year","iso3c"),all.x=T)
names(rt)

## countries in corpus but not rt
which(!country.year.means$iso3c %in% rt$iso3c)

## Adjust n from NA to 0
rt$n.docs[is.na(rt$n.docs)] <- 0
rt$n.docs[rt$year==1979] <- NA
summary(rt$n.docs)

## Make n binary
rt$n.binary <- rt$n.docs
rt$n.binary[rt$n.docs > 0] <- 1
rt$n.binary[is.na(rt$n.docs)] <- NA
rt$n.binary <- as.factor(rt$n.binary)
summary(rt$n.binary)

# Test
rt$mena <- as.numeric(as.character(rt$mena))
cor(rt$muslim, rt$mena, use="complete.obs") #0.6361743
cor(rt$amnesty,rt$statedept,use="complete.obs") #0.8052263
cor(rt$lnreportcount,rt$count,use="complete.obs") #0.4008974
summary(lm(count ~ lnreportcount, data = rt))
summary(rt$rights)
summary(rt$idealpoint)
summary(rt$mena)

# Write
rt.orig <- rt
write.csv(rt.orig,"Data/country-year/original.csv", row.names = F)

## NEEDS WORK!

##################################################
##### 5) Missing Values: Amelia Mult. Impute #####
##################################################

rt <- rt.orig
names(rt)
# subset
rt <- rt[,c('year', 'ccode', 'pop.wdi', 'gdp.pc.un','polity2', 'domestic9', 'wecon', 'wopol', 'wosoc', 'physint')]
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
rt.impute$alt.dv.perc <- rt.orig$alt.dv.perc
rt.impute$gbv <- rt.orig$gbv
rt.impute$n.docs <- rt.orig$n.docs
rt.impute$n.words <- rt.orig$n.words
rt.impute$n.binary <- rt.orig$n.binary
rt.impute$region <- rt.orig$region
rt.impute$muslim.maj <- rt.orig$muslim.maj
rt.impute$muslim <- rt.orig$muslim
rt.impute$mena <- as.numeric(rt.orig$mena)
rt.impute$count <- rt.orig$count

# test
cor(rt.impute$muslim, rt.impute$mena, use="complete.obs") #0.6366512

## write
write.csv(rt.impute,"Data/country-year/imputed.csv", row.names = F)

# ====================================================================

############################################
##### 6) Missing Values: Nearest Value #####
############################################

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
rt$idealpoint <- impute(rt$idealpoint)

# test
summary(rt$lnreportcount)
summary(rt.orig$lnreportcount)
cor(rt$muslim, rt$mena, use="complete.obs") #0.6365522

# save and write
rt.nearest <- rt
write.csv(rt.nearest, "Data/country-year/nearest.csv", row.names = F)

