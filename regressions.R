setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

library(plyr)
library("MASS")
library("xtable")
library(plm)
library(ggplot2)
library(reshape2)
library(stargazer)

meta.topics <- read.csv("Data/meta-topics.csv")
meta.topics$X <- NULL
meta.topics$X.1 <- NULL

############################################
######## Topic means by country-year #######
############################################

names(meta.topics)
x <- subset(meta.topics,select=c(business:politics,COUNTRY_CODE,YEAR,REGION))
names(x)[16:18] <- c("iso3c","year","region")

# country-year means
country.year.means <- ddply(.data=x, .variables=.(iso3c,year), numcolwise(mean,na.rm = TRUE))

# add 'n' column
y <- ddply(.data=x,.variables=.(iso3c,year), .fun=nrow) 
country.year.means$n <- y$V1
names(country.year.means)
write.csv(country.year.means,"Results/15.1/country-year-means.csv")

# region-year means

region.year.means <- ddply(.data=x, .variables=.(year,region), numcolwise(mean,na.rm = TRUE))
names(region.year.means)
y <- ddply(.data=x,.variables=.(year,region), .fun=nrow) 
region.year.means$n <- y$V1

write.csv(region.year.means,"Results/15.1/region-year-means.csv")

# country-year number of top topic articles
names(meta.topics)
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
names(country.year.means)

# take mean
country.year.topic.top[,3:17] <- country.year.topic.top[,3:17]/country.year.topic.top$n
head(country.year.topic.top)

# plotting

ggplot(data=region.year.means, aes(x=year,y=rights,group=region,color=region)) + geom_line()

###############################################
######## Prepare Country-level variables ######
###############################################

#### Load country-year database
rt <- read.csv("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database/rt.no.us.csv")
names(rt)
rt$X <- NULL

### merge
# take only 1980-2013 topic data
country.year.means <- country.year.means[country.year.means$year<2014,]
# merge
rt <- merge(rt,country.year.means,by=c("year","iso3c"),all.x=T)

## Find missing values 

rt.merge <- merge(rt,country.year.means,by=c("year","iso3c"),all.y=T)
rt.merge <- arrange(rt.merge,desc(year),ccode)
x<- data.frame(cbind(rt.merge$year,rt.merge$ccode))
x <- which(duplicated(x))
unique(rt.merge$iso3c[x]) # iceland, ukraine, malta, barbados, grenada, samoa, Seychelles, Brunei, Monaco
countries[countries$iso3c=="MCO",]
country.year.means$year[country.year.means$iso3c=="IRQ"] %in% rt$year[rt$iso3c=="IRQ"]
rt[rt$country=="Malt"]
names(rt)

# make dummy variable for mena
rt$mena <- 0
rt$mena[rt$region=="MENA"] <- 1

# make panel data
rt <- pdata.frame(rt, c("ccode","year"))
names(rt)

# lag
rt$rights.lagged <- lag(rt$rights,1)
rt$worldbank <- lag(rt$worldbank,1)
rt$gdp.pc.wdi <- lag(rt$gdp.pc.wdi,1)
rt$pop.wdi <- lag(rt$pop.wdi)
rt$physint <- lag(rt$physint,1)
rt$domestic9 <- lag(rt$domestic9,1)
rt$statedept <- lag(rt$statedept,1)
rt$amnesty <- lag(rt$amnesty,1)
rt$cinc <- lag(rt$cinc,1)
rt$wopol <- lag(rt$wopol,1)
rt$wosoc <- lag(rt$wosoc,1)
rt$wecon <- lag(rt$wecon,1)
rt$muslim <- lag(rt$muslim,1)

# number unique countries
length(unique(rt$rt_code))

# removing cases with no rights DV
rt <- rt[!is.na(rt$rights)]

###################
###### Model ######
##################

# Testing
lm1 <- lm(rights ~ rights.lagged+wopol+muslim+polity2+physint+new_empinx+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt) 
summary(lm1)

random <- plm(rights ~ rights.lagged+wopol+muslim+polity2+physint+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt,model = "random",index = c("ccode","year"))
pool <- plm(rights ~ rights.lagged+wopol+muslim+polity2+physint+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt,model = "pooling",index = c("ccode","year"))
fixed <- plm(rights ~ rights.lagged+wopol+muslim+polity2+physint+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt,model = "within",index = c("ccode","year"))

phtest(fixed, random)

coeftest(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC1"))
coeftest(pm1, vcov=function(x) vcovHC(x, cluster="time", type="HC1"))

# Missing Values
lm1 <- lm(rights ~ gdp.pc.wdi,data = rt) 
summary(lm1)

rt.na.muslim <- rt[is.na(rt$muslim),]
rt.na.polity <- rt[is.na(rt$polity2),]
rt.na.physint <- rt[is.na(rt$physint),]
rt.na.gdp <- rt[is.na(rt$gdp.pc.un),]
rt.na.wopol <- rt[is.na(rt$wopol),]
rt.na.wosoc <- rt[is.na(rt$wosoc),]
rt.na.wecon <- rt[is.na(rt$wecon),]
rt.na.domestic9 <- rt[is.na(rt$domestic9),]

# plm - 1
pm1 <- plm(rights ~ rights.lagged+wopol+muslim+polity2+physint+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm1)

pbgtest(pm1)
plmtest(pm1, type=c("bp"))

coeftest(pm1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
coeftest(pm1, vcovHC(pm1, type = "HC3"))

# plm - 2
pm2 <- plm(rights ~ rights.lagged+wosoc+muslim+polity2+physint+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm2)

pbgtest(pm1)
coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# plm - 3
pm3 <- plm(rights ~ rights.lagged+wecon+muslim+polity2+physint+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm3)
names(rt)
x<- coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

stargazer(x, type = "text", title="Descriptive statistics", digits=4, out="table2.txt")

# regressing on number of articles total

pm4 <- plm(n ~ lnreportcount+cinc+muslim+polity2+statedept+log(gdp.pc.wdi)+log(pop.wdi)+domestic9+mena,data = rt,model = "random",index = c("ccode","year"))
summary(pm4)
coeftest(pm4, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

