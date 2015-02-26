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

### Linear Modeling
pos.model <- glm(pos.rate ~ (relevel(REGION,6)), data = women, na.action=na.omit) 
summary(pos.model)
neg.model <- glm(neg.rate) ~ (relevel(REGION,6)), data = women, na.action=na.omit) 
summary(neg.model)

### Means per region

regions <- unique(women$REGION)
sent <- data.frame(regions)
rownames(sent) <- regions

pos.mean <- function(region){
  mean <- mean(women$pos.rate[women$REGION==region],na.rm=TRUE)
  return(mean)
}
pos.mean("EECA")
sent$pos <- lapply(sent$regions,pos.mean)
sent$pos <- as.numeric(sent$pos)
summary(sent$pos)

neg.mean <- function(region){
  mean <- mean(women$neg.rate[women$REGION==region],na.rm=TRUE)
  return(mean)
}
neg.mean("EECA")
sent$neg <- lapply(sent$regions,neg.mean)
sent$neg <- as.numeric(sent$neg)
summary(sent$neg)
sent$region

barplot(sent$pos, names.arg=sent$region )
barplot(sent$neg, names.arg=sent$region)
