# This script:
# 1) Finds mean topic proportions for country-year and region-year levels
# 2) Merges country-year topic proportions with other country-year variables
# 3) Imputes missing values using nearest-value and Amelia techniques
# 4) Lags variables
# 5) Runs a few different regressiong modles

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
library(Amelia) # for missing valus

meta.topics <- read.csv("Data/meta-topics.csv")
meta.topics$X <- NULL
meta.topics$X.1 <- NULL

############################################
###### 1) Topic means by country-year ######
############################################

x <- subset(meta.topics,select=c(business:politics,COUNTRY_CODE,YEAR,REGION))
names(x)[16:18] <- c("iso3c","year","region")

# country-year means
country.year.means <- ddply(.data=x, .variables=.(iso3c,year), numcolwise(mean,na.rm = TRUE))
# add 'n' column
y <- ddply(.data=x,.variables=.(iso3c,year), .fun=nrow) 
country.year.means$n <- y$V1
write.csv(country.year.means,"Results/15.1/country-year-means.csv")

# country-year number of top topic articles
x <- subset(meta.topics,select=c(COUNTRY_CODE,YEAR,REGION,top.topic))
names(x)[1:3] <- c("iso3c","year","region")
country.year.topic.top <- ddply(.data=x, .variables=.(iso3c,year,top.topic), .fun=nrow)
head(country.year.topic.top,20)
sum(country.year.topic.top$V1) # testing
country.year.topic.top <- dcast(country.year.topic.top,iso3c + year ~ top.topic)
head(country.year.topic.top)
# add 'n' column
y <- ddply(.data=x,.variables=.(iso3c,year), .fun=nrow) 
country.year.topic.top$n <- y$V1
# take mean
country.year.topic.top[,3:17] <- country.year.topic.top[,3:17]/country.year.topic.top$n
head(country.year.topic.top)

# region-year means
region.year.means <- ddply(.data=x, .variables=.(year,region), numcolwise(mean,na.rm = TRUE))
y <- ddply(.data=x,.variables=.(year,region), .fun=nrow) 
region.year.means$n <- y$V1
write.csv(region.year.means,"Results/15.1/region-year-means.csv")

# plotting
ggplot(data=region.year.means, aes(x=year,y=rights,group=region,color=region)) + geom_line()

###############################################
###### 2) Prepare Country-level variables #####
###############################################

#### Load country-year database
rt <- read.csv("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database/rt.no.us.csv")
names(rt)
rt$X <- NULL

#### Merge
# take only 1980-2013 topic data
country.year.means <- country.year.means[country.year.means$year<2014,]
# merge
rt <- merge(rt,country.year.means,by=c("year","iso3c"),all.x=T)
## Find missing countries 
rt.merge <- merge(rt,country.year.means,by=c("year","iso3c"),all.y=T)
rt.merge <- arrange(rt.merge,desc(year),ccode)
x<- data.frame(cbind(rt.merge$year,rt.merge$ccode))
x <- which(duplicated(x))
unique(rt.merge$iso3c[x]) # iceland, ukraine, malta, barbados, grenada, samoa, Seychelles, Brunei, Monaco

# MENA dummy variable
rt$mena <- 0
rt$mena[rt$region=="MENA"] <- 1

# number unique countries
length(unique(rt$rt_code))

# removing cases with no rights DV
rt <- rt[!is.na(rt$rights),]

#############################
##### 3) Missing Values #####
#############################

rt.na.muslim <- rt[is.na(rt$muslim),]
rt.na.polity <- rt[is.na(rt$polity2),]
rt.na.physint <- rt[is.na(rt$physint),]
rt.na.gdp <- rt[is.na(rt$gdp.pc.un),]
rt.na.wopol <- rt[is.na(rt$wopol),]
rt.na.wosoc <- rt[is.na(rt$wosoc),]
rt.na.wecon <- rt[is.na(rt$wecon),]
rt.na.domestic9 <- rt[is.na(rt$domestic9),]

# Imputing values using nearest value
#####################################

# write function that passes variable and fills in missing values based on nearest value
impute <- function(var){
  DT <- data.table(
    id    = rt$ccode,
    date  = as.numeric(as.character(rt$year)),
    value = var
  )
  x <- setna(DT, along_with = date, by = id,roll="nearest")
  return(x$value)
}

rt$physint <- impute(rt$physint)
rt$polity2 <- impute(rt$polity2)
rt$gdp.pc.un <- impute(rt$gdp.pc.un)
rt$domestic9 <- impute(rt$domestic9)
rt$wopol <- impute(rt$wopol)
rt$wosoc <- impute(rt$wosoc)
rt$wecon <- impute(rt$wecon)
rt$muslim <- impute(rt$muslim)

# Amelia method
###############

# subset
rt <- rt[,c("ccode","year","rights","muslim","mena","polity2","physint","gdp.pc.un","pop.wdi","wopol","wosoc","wecon","domestic9")]
rt$year <- as.integer(as.character((rt$year)))
# model 1
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
# Apply the regression over each element of the list
lapply(X=datasets, FUN=function(x){
  summary(plm(rights ~ wopol+muslim+polity2+physint+log(gdp.pc.un)+log(pop.wdi)+domestic9+mena,data = x,model = "pooling",index = c("ccode","year")))
})
# map missing values
missmap(a.out)
# diagnostics
overimpute(a.out, var = "polity2")
# take 1st imputation
rt <- a.out$imputations[[1]]

###########################
### 4) Lagged Variables ###
###########################

# make panel data
rt <- pdata.frame(rt, c("ccode","year"))
names(rt)

# lag
rt$rights.lagged <- lag(rt$rights,1)
rt$gdp.pc.un <- lag(rt$gdp.pc.un,1)
rt$pop.wdi <- lag(rt$pop.wdi)
rt$physint <- lag(rt$physint,1)
rt$domestic9 <- lag(rt$domestic9,1)
rt$wopol <- lag(rt$wopol,1)
rt$wosoc <- lag(rt$wosoc,1)
rt$wecon <- lag(rt$wecon,1)
rt$muslim <- lag(rt$muslim,1)

###################
#### 5) Model #####
###################

# Testing
lm1 <- lm(rights ~ rights.lagged+wopol+muslim+polity2+physint+log(gdp.pc.un)+log(pop.wdi)+domestic9+mena,
          data = rt) 
summary(lm1)

random <- plm(rights ~ wopol+muslim+polity2+physint+log(gdp.pc.un)+log(pop.wdi)+domestic9+mena,data = rt,model = "random",index = c("ccode","year"))
pool <- plm(rights ~ wopol+muslim+polity2+physint+log(gdp.pc.un)+log(pop.wdi)+domestic9+mena,data = rt,model = "pooling",index = c("ccode","year"))
fixed <- plm(rights ~ wopol+muslim+polity2+physint+log(gdp.pc.un)+log(pop.wdi)+domestic9+mena,data = rt,model = "within",index = c("ccode","year"))

phtest(fixed, random)

coeftest(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC1"))
coeftest(pm1, vcov=function(x) vcovHC(x, cluster="time", type="HC1"))

# plm - 1
pm1 <- plm(rights ~ rights.lagged+wopol+muslim+mena+polity2+physint+log(gdp.pc.un)+log(pop.wdi)+domestic9,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm1)

pbgtest(pm1)
plmtest(pm1, type=c("bp"))

coeftest(pm1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
scoeftest(pm1, vcovHC(pm1, type = "HC3"))

# plm - 2
pm2 <- plm(rights ~ rights.lagged+wosoc+muslim+polity2+physint+log(gdp.pc.un)+log(pop.wdi)+domestic9+mena,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm2)

pbgtest(pm1)
coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# plm - 3
pm3 <- plm(rights ~ rights.lagged+wecon+muslim+polity2+physint+log(gdp.pc.un)+log(pop.wdi)+domestic9+mena,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm3)
names(rt)
coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

stargazer(x, type = "text", title="Descriptive statistics", digits=4, out="table2.txt")

# regressing on number of articles total

pm4 <- plm(n ~ lnreportcount+cinc+muslim+polity2+statedept+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt,model = "random",index = c("ccode","year"))
summary(pm4)
coeftest(pm4, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

