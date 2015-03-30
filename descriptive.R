#### This script runs some basic summary stats on women-foreign.csv
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")
library(plyr)
library(ggplot2)
library(reshape2)

# Load data (optional)
women <- read.csv("Data/women-foreign.csv")
names(women)

########################################
######## Quick Sum and Barplots #######
########################################

# number of articles per paper
n.paper<- ddply(.data=women, .variables=.(PUBLICATION), .fun=nrow)
n.paper
# number of articles per region

n.region <- ddply(.data=women, .variables=.(REGION), .fun=nrow)
barplot(summary(women$REGION))

### Number of articles per country

ddply(.data=women, .variables=.(COUNTRY_FINAL), .fun=nrow)

#################################################################
##### Plotting women number of articles per year per region #####
#################################################################

n.region.year <- ddply(.data=women, .variables=.(YEAR), .fun=summarize,"MENA"=sum(REGION=="MENA"),"Asia"=sum(REGION=="Asia"),"Africa"=sum(REGION=="Africa"),"EECA"=sum(REGION=="EECA"),"West"=sum(REGION=="West"),"LA"=sum(REGION=="LA"))
n.region.year

melted <- melt(n.region.year,id.vars="YEAR",measure.vars=c("MENA","Asia","Africa","EECA","West","LA"))
names(melted) <- c("year","region","count")
melted

# regions over time.
ggplot(data=melted, aes(x=year,y=count,group=region,color=region)) + geom_line() + ggtitle("Number of article per region over time")

write.csv(n.region,"Results/region_year.csv")

