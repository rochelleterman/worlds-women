rm(list=ls())
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

### Load Data

women <- read.csv('Data/women-processed.csv')
names(women)
summary(women$number.of.porter.stemmed.positive.words)

### create new columns for pos + neg rates
women$pos.rate <- women$number.of.porter.stemmed.positive.words / women$number.of.non.stop.words
women$neg.rate <-women$number.of.porter.stemmed.negative.words / women$number.of.non.stop.words

summary(women$pos.rate)
summary(women$neg.rate)

### Make data frame of means per region

regions <- unique(women$REGION)
sent <- data.frame(regions)
rownames(sent) <- regions

pos.mean <- function(region){
  mean <- mean(women$pos.rate[women$REGION==region],na.rm=TRUE)
  return(mean)
}
sent$pos <- lapply(sent$regions,pos.mean)
sent$pos <- as.numeric(sent$pos)

neg.mean <- function(region){
  mean <- mean(women$neg.rate[women$REGION==region],na.rm=TRUE)
  return(mean)
}
sent$neg <- lapply(sent$regions,neg.mean)
sent$neg <- as.numeric(sent$neg)

## plotting

melted <- melt(sent,id.vars="regions",measure.vars=c("pos","neg"))
melted

ggplot(data=melted, aes(x=regions,y=value,group=variable,fill=variable)) + geom_bar(stat="identity",position=position_dodge()) + ylab("word distribution") + ggtitle("Sentiments Per Region")


### Linear Modeling
pos.model <- glm(pos.rate ~ (relevel(REGION,6)), data = women, na.action=na.omit) 
summary(pos.model)
neg.model <- glm(neg.rate ~ (relevel(REGION,6)), data = women, na.action=na.omit) 
summary(neg.model)
