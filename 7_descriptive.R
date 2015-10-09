#### This script runs some basic descriptive stats

rm(list=ls())
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")
library(plyr)
library(ggplot2)
library(reshape2)

# Load data
docs <- read.csv("Data/topic-proportions/meta-topics.csv")
rt <- read.csv("Data/country-year/original.csv")

# from topic proportions to number of words
docs[,1:15] <- docs[,1:15]*docs$n.words
sum(docs[1,1:15]) # 348

########################################
######## Quick Sums and Barplots #######
########################################

# number of articles per paper
n.paper<- ddply(.data=docs, .variables=.(publication), .fun=nrow)
n.paper

# number of articles per region
n.region <- ddply(.data=docs, .variables=.(region), .fun=nrow)

# plot
jpeg(filename = "Results/descriptive/n-region-barplot.jpeg",width=900,height=600,type="quartz")
barplot(summary(docs$region))
dev.off()

# number of articles per country
n.country <- ddply(.data=docs, .variables=.(country), .fun=nrow)
n.country <- arrange(n.country,desc(V1))
write.csv(n.country,"Results/descriptive/n-country.csv")

# total number of countries in dataset
length(unique(rt$ccode)) # 198

# number of obs with a document
summary(as.factor(rt$n.binary))

# number of obs > 1979
nrow(rt[rt$year > 1979,])# 6226

############################################################
##### Plotting  number of articles per year per region #####
############################################################

# Number of Documents Per Region Over Time
n.region.year <- ddply(.data = rt, .variables = .(year, region), .fun=summarize, "women" = sum(n.docs, na.rm = T), "total" = sum(count, na.rm =T) )
n.region.year <- n.region.year[!is.na(n.region.year$region) & n.region.year$year > 1979,]

# Plot
jpeg(filename = "Results/descriptive/n-region-plot.jpg",width=900,height=500,type="quartz")
ggplot(data=n.region.year, aes(x=year,y=women,group=region,color=region)) + geom_line() + labs(x = "Year", y = "Number of Articles about Women") + theme(axis.text=element_text(size=12), axis.title=element_text(size=16))
dev.off()

# percentage of document about women
jpeg(filename = "Results/descriptive/n-region-plot-percentage.jpeg",width=900,height=600,type="quartz")
ggplot(data=n.region.year, aes(x=year,y=women/total,group=region,color=region)) + geom_line()
dev.off()

##################################################
### Plotting distribution of Rights language ###
##################################################

# region-year means
region.year.means <- ddply(.data=docs, .variables=.(year,region), numcolwise(sum,na.rm = TRUE))
region.year.means[3:17] <- region.year.means[3:17]/region.year.means$n.words
sum(region.year.means[1,3:17])
region.year.means$n <- (ddply(.data=docs,.variables=.(year,region), .fun=nrow))$V1 
write.csv(region.year.means,"Results/stm/region-year-means.csv")

# region-year sums
region.year.sums <- ddply(.data=docs, .variables=.(year,region), numcolwise(sum,na.rm = TRUE))
region.year.sums$n <- (ddply(.data=docs,.variables=.(year,region), .fun=nrow))$V1 
write.csv(region.year.means,"Results/stm/region-year-sums.csv")

# plotting
ggplot(data=region.year.means, aes(x=year,y=religion,group=region,color=region)) + geom_line()

# smoothering
ggplot(data=region.year.sums, aes(x=year,y=rights,group=region,color=region)) + geom_line() + stat_smooth()

