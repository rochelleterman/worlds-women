#### This script runs some basic summary stats on women-foreign.csv
rm(list=ls())
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")
library(plyr)
library(ggplot2)
library(reshape2)

# Load data
women <- read.csv("Data/Corpora/women-foreign.csv")
names(women)
women$REGION <- as.factor(women$REGION)

########################################
######## Quick Sum and Barplots #######
########################################

# number of articles per paper
n.paper<- ddply(.data=women, .variables=.(PUBLICATION), .fun=nrow)
n.paper

# number of articles per region
n.region <- ddply(.data=women, .variables=.(REGION), .fun=nrow)
write.csv(n.region,"Results/descriptive/region_year.csv")

# plot
jpeg(filename = "Results/descriptive/n-region-barplot.jpeg",width=900,height=600,type="quartz")
barplot(summary(women$REGION))
dev.off()

# number of articles per country
n.country <- ddply(.data=women, .variables=.(COUNTRY_FINAL), .fun=nrow)
n.country <- arrange(n.country,desc(V1))
write.csv(n.country,"Results/descriptive/n-country.csv")

#################################################################
##### Plotting women number of articles per year per region #####
#################################################################

# Number of Documents Per Region Over Time
n.region.year <- ddply(.data=women, .variables=.(YEAR), .fun=summarize,"MENA"=sum(REGION=="MENA"),"Asia"=sum(REGION=="Asia"),"Africa"=sum(REGION=="Africa"),"EECA"=sum(REGION=="EECA"),"West"=sum(REGION=="West"),"LA"=sum(REGION=="LA"))
n.region.year
melted <- melt(n.region.year,id.vars="YEAR",measure.vars=c("MENA","Asia","Africa","EECA","West","LA"))
names(melted) <- c("year","region","count")
melted

# Plot
jpeg(filename = "Results/descriptive/n-region-plot.jpeg",width=900,height=600,type="quartz")
ggplot(data=melted, aes(x=year,y=count,group=region,color=region)) + geom_line()
dev.off()

