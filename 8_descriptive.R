#### This script runs some basic descriptive stats

rm(list=ls())
setwd("~/Dropbox/berkeley/Git-Repos/worlds-women")
library(plyr)
library(ggplot2)
library(reshape2)

# Load data
docs <- read.csv("Data/topic-proportions/meta-topics.csv")
rt <- read.csv("Data/country-year/original.csv")

# from topic proportions to number of words
docs[,1:15] <- docs[,1:15]*docs$n.words
sum(docs[1,1:15]) # 331

########################################
######## Quick Sums and Barplots #######
########################################

# number of articles per paper
n.paper<- ddply(.data=docs, .variables=.(publication), .fun=nrow)
n.paper

# number of articles per region
n.region <- ddply(.data=docs, .variables=.(region), .fun=nrow)

# plot: articles per region
jpeg(filename = "Results/descriptive/n-region-barplot.jpeg",width=900,height=600,type="quartz")
barplot(summary(docs$region))
dev.off()

# number of articles per country
n.country <- ddply(.data=docs, .variables=.(country), .fun=nrow)
n.country <- arrange(n.country,desc(V1))
write.csv(n.country,"Results/descriptive/n-country.csv")

# total number of countries in dataset
length(unique(rt$ccode)) # 199

# number of obs with a document
summary(as.factor(rt$n.binary))

# number of obs > 1979
nrow(rt[rt$year > 1979,])# 6292

##################################
######## Words per Topics  #######
##################################

# average number of words devoted to rights
mean(docs$rights) # 31.92353, per doc
rt$rights.words <- rt$rights * rt$n.words
mean(rt$rights.words, na.rm = T) # 96.74622, per country-year

# average prevalence of rights
mean(docs$rights) / mean(docs$n.words) # 0.09255028, per doc
mean(rt$rights, na.rm = T) # 0.08688917, per country year

# average number of words devoted to rights in MENA
mean(docs$rights[docs$region=="MENA"]) # 40.71

# average number of words devoted to rights in NOT MENA
mean(docs$rights[!docs$region=="MENA"]) # 29.52

.035 * mean(rt$n.words, na.rm = T) # 36.49178
mean(rt$n.words, na.rm = T) # 1042.622
mean(docs$n.words) # 344.9318
.035 * mean(docs$n.words)

############################################################
##### Plotting: number of articles per year per region #####
############################################################

# Number of Documents Per Region Over Time
n.region.year <- ddply(.data = rt, .variables = .(year, region), .fun=summarise, "women" = sum(n.docs, na.rm = T), "total" = sum(count, na.rm =T) )
n.region.year <- n.region.year[!is.na(n.region.year$region) & n.region.year$year > 1979,]

year.max <- ddply(n.region.year, .(year), summarize, max = region[which.max(women)])

# Plot
jpeg(filename = "Results/descriptive/n-region-plot.jpg",width=900,height=500,type="quartz")
ggplot(data=n.region.year, aes(x=year,y=women,group=region,color=region)) + geom_line() + labs(x = "Year", y = "Number of Articles about Women") + theme(axis.text=element_text(size=12), axis.title=element_text(size=16))
dev.off()

# percentage of documents about women
jpeg(filename = "Results/descriptive/n-region-plot-percentage.jpeg",width=900,height=600,type="quartz")
ggplot(data=n.region.year, aes(x=year,y=women/total,group=region,color=region)) + geom_line() + stat_smooth() + labs(x = "Year", y = "Percentage of Articles about Women") + theme(axis.text=element_text(size=12), axis.title=element_text(size=16))
dev.off()

# how much of Asia was after 2012?
asia <- docs[docs$region=="Asia",]
asia.2012 <- asia[asia$year>=2012,]
nrow(asia.2012) / nrow(asia)
nrow(asia.2012[asia.2012$country=="India",]) / nrow(asia.2012)

##################################################
### Plotting distribution of Topical language ###
##################################################

# region-year means
region.year.means <- ddply(.data=docs, .variables=.(year,region), numcolwise(sum,na.rm = TRUE))
region.year.means[3:17] <- region.year.means[3:17]/region.year.means$n.words
sum(region.year.means[1,3:17]) # 1
region.year.means$n <- (ddply(.data=docs,.variables=.(year,region), .fun=nrow))$V1 
# write.csv(region.year.means,"Results/stm/region-year-means.csv")

# region-year sums
region.year.sums <- ddply(.data=docs, .variables=.(year,region), numcolwise(sum,na.rm = TRUE))
region.year.sums$n <- (ddply(.data=docs,.variables=.(year,region), .fun=nrow))$V1 
# write.csv(region.year.means,"Results/stm/region-year-sums.csv")
names(region.year.sums)
# plotting
ggplot(data=region.year.means, aes(x=year,y=rights,group=region,color=region)) + geom_line() + stat_smooth()

# correlations
cor.islam.eq <- ddply(.data=docs, .variables=.(year,region), summarize, islam.equality = cor(islam, rape))

ggplot(data=cor.islam.eq, aes(x=year,y=islam.equality,group=region,color=region)) + geom_line() + stat_smooth()


