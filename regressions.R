setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

library(plyr)
library("MASS")
library("xtable")
library(plm)
library(ggplot2)
library(reshape2)

#meta.topics <- read.csv("Data/meta-topics.csv")

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

# just mena

mena <- region.year.means[region.year.means$region=="MENA",c("year","region","rights","marriage","religion","politics","combat")]
mena$region <- NULL
names(mena)
mena <- melt(mena,id="year")
names(mena) <- c("year","topic","value")
mean.sub <- mena[mena$topic=="religion" | mena$topic=="religion" |]

# plotting

ggplot(data=mena, aes(x=year,y=value,group=topic,color=topic)) + geom_line()

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

