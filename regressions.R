setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

library(plyr)
library("MASS")
library("xtable")
library(plm)

#meta.topics <- read.csv("Data/meta-topics.csv")

############################################
######## Topic means by country-year #######
############################################

names(meta.topics)
x <- subset(meta.topics,select=c(business:politics,COUNTRY_CODE,YEAR))
names(x)[16:17] <- c("iso3c","year")

country.year.means <- ddply(.data=x, .variables=.(iso3c,year), numcolwise(mean,na.rm = TRUE))

y <- ddply(.data=x,.variables=.(iso3c,year), .fun=nrow)
country.year.means$n <- y$V1
names(country.year.means)

#### Load country-year database
rt <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database/rt.csv")
names(rt)

### merge
rt.merge <- merge(rt,country.year.means,by=c("year","iso3c"),all.x=T)
rt <- rt.merge
names(rt)

### regress

# PLM
plm <- plm.data(rt, c("ccode","year"))
plm.est <- plm(business ~ polity+autoc+physint+speech+new_empinx+wecon+wopol+wosoc+log(gdp.pc.wdi)+pop.wdi+statedept+cinc+domestic9+muslim,data = rt,model = "within")
summary(plm.est)

# LM
glm.est<-glm(sport ~ polity+autoc+physint+speech+new_empinx+wecon+wopol+wosoc+log(gdp.pc.wdi)+pop.wdi+statedept+milper+cinc+domestic9+muslim,data = rt, na.action=na.omit) 
summary(glm.est)
fixef(plm.est)
pFtest(plm.est, glm.est) 

