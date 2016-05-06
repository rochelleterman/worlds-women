# This script estimates the STM on women-processed.csv

rm(list=ls())
setwd("~/Dropbox/berkeley/Git-Repos/worlds-women")
library(stm)
library(plyr)

### Load Data
women <- read.csv('Data/Corpora/women-processed.csv')

####################################
######### Pre-processing ###########
####################################

# custom stopwords
countries <- read.csv("Data/country_codes.csv")
stopwords.country <- c(as.character(countries$Key), "saudi", "german", "ese", "ian")
stopwords.country <- tolower(stopwords.country)

# process
temp<-textProcessor(documents=women$TEXT.NO.NOUN,metadata=women,customstopwords=stopwords.country)
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta, lower.thresh=10)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

# Removing 29737 of 37390 terms (66329 of 1073578 tokens) due to frequency 
# Your corpus now has 4531 documents, 7653 terms and 1007249 tokens.

##################################
######### Choose Model ###########
##################################

mod.15.15 <- stm(docs,vocab, 15, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 15, max.em.its = 50)

# assign model
model <- mod.15.15

# rerun model to convergence
model <- stm(docs,vocab, 15, prevalence=~REGION+s(YEAR), data=meta, seed = 15, max.em.its = 200)

# label
labelTopics(model)

# Topic Quality plot
jpeg("Results/stm/exclusivity-and-cohesiveness.jpeg",width=750,height=500,type="quartz")
topicQuality(model=model, documents=docs)
dev.off()

# Topic Labels plot
jpeg("Results/stm/labels-1.jpeg",width=700,height=1000,type="quartz")
plot.STM(model,type="labels",topics=1:10,width=75)
dev.off()

jpeg("Results/stm/labels-2.jpeg",width=700,height=1000,type="quartz")
plot.STM(model,type="labels",topics=11:15,width=75)
dev.off()

##################################
######### Label Topics ###########
##################################

# Labels
labelTopics(model)

# Example Docs
findThoughts(model,texts=meta$TITLE,n=3,topics=1:15)

# Add labels
labels = c("Business", "Sports", "Reproductive Health", "Travel", "Fashion", "UN", "Sexual Assault", "Combat", "Women's Rights and Gender Equality", "Politics", "Profiles", "Human Interest", "Marriage & Family", "Religion", "Cancer")

# save data
save(docs, vocab, meta, labels, model, file = "Data/stm.RData")
