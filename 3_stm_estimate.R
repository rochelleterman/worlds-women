# This script estimates the STM on women-processed.csv

setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")
library(stm)
library(plyr)
rm(list=ls())

### Load Data
women <- read.csv('Data/Corpora/women-processed.csv')
names(women)

####################################
######### Pre-processing ###########
####################################

# custom stopwords
countries <- read.csv("Data/country_codes.csv")
stopwords.country <- c(as.character(countries$Key), "saudi", "german")
stopwords.country <- tolower(stopwords.country)

# process
temp<-textProcessor(documents=women$TEXT.NO.NOUN,metadata=women,customstopwords=stopwords.country)
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta, lower.thresh=2)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

#Removing 22185 of 37485 terms (26344 of 1073997 tokens) due to frequency 
#Your corpus now has 4522 documents, 15300 terms and 1047653 tokens.

##################################
######### Choose Model ###########
##################################

# mod.15.new <- stm(docs,vocab, 15, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 22222, max.em.its = 200)

# Topic Quality plot
jpeg("Results/stm/exclusivity-and-cohesiveness.jpeg",width=750,height=500,type="quartz")
topicQuality(model=mod.15.new, documents=docs)
dev.off()

# Topic Labels plot
jpeg("Results/stm/labels-1.jpeg",width=700,height=1000,type="quartz")
plot.STM(mod.15.new,type="labels",topics=1:10,width=75)
dev.off()

jpeg("Results/stm/labels-2.jpeg",width=700,height=1000,type="quartz")
plot.STM(mod.15.new,type="labels",topics=11:15,width=75)
dev.off()

# assign model for later
model <-mod.15.new

####################################
######### Explore Topics ###########
####################################

# Example Docs
thoughts1 <- findThoughts(model,texts=meta$TITLE,n=3,topics=1)$docs[[1]]
thoughts2 <- findThoughts(model,texts=meta$TITLE,n=4,topics=2)$docs[[1]]
thoughts3 <- findThoughts(model,texts=meta$TITLE,n=3,topics=3)$docs[[1]]
thoughts4 <- findThoughts(model,texts=meta$TITLE,n=3,topics=4)$docs[[1]]
thoughts5 <- findThoughts(model,texts=meta$TITLE,n=3,topics=5)$docs[[1]]
thoughts6 <- findThoughts(model,texts=meta$TITLE,n=3,topics=6)$docs[[1]]
thoughts7 <- findThoughts(model,texts=meta$TITLE,n=3,topics=7)$docs[[1]]
thoughts8 <- findThoughts(model,texts=meta$TITLE,n=3,topics=8)$docs[[1]]
thoughts9 <- findThoughts(model,texts=meta$TITLE,n=3,topics=9)$docs[[1]]
thoughts10 <- findThoughts(model,texts=meta$TITLE,n=3,topics=10)$docs[[1]]
thoughts11 <- findThoughts(model,texts=meta$TITLE,n=3,topics=11)$docs[[1]]
thoughts12 <- findThoughts(model,texts=meta$TITLE,n=3,topics=12)$docs[[1]]
thoughts13 <- findThoughts(model,texts=meta$TITLE,n=3,topics=13)$docs[[1]]
thoughts14 <- findThoughts(model,texts=meta$TITLE,n=3,topics=14)$docs[[1]]
thoughts15 <- findThoughts(model,texts=meta$TITLE,n=3,topics=15)$docs[[1]]

# Labels
labelTopics(model)

# representative titles
plotQuote(thoughts1, width=40, main="Topic 1") 
plotQuote(thoughts2, width=40, main="Topic 2")  
plotQuote(thoughts3, width=40, main="Topic 3")  
plotQuote(thoughts4, width=40, main="Topic 4") 
plotQuote(thoughts5, width=40, main="Topic 5") 
plotQuote(thoughts6, width=40, main="Topic 6") 
plotQuote(thoughts7, width=40, main="Topic 7") 
plotQuote(thoughts8, width=40, main="Topic 8") 
plotQuote(thoughts9, width=40, main="Topic 9") 
plotQuote(thoughts10, width=40, main="Topic 10") 
plotQuote(thoughts11, width=40, main="Topic 11") 
plotQuote(thoughts12, width=40, main="Topic 12") 
plotQuote(thoughts13, width=40, main="Topic 13") 
plotQuote(thoughts14, width=40, main="Topic 14") 
plotQuote(thoughts15, width=40, main="Topic 15") 

# assign hand labels
labels = c("Cancer", "Reproductive Health", "Religion", "Business & Work", "Marriage & Family", "Arts", "Migration", "Gender-Based Violence", "War & Combat", "Literature", "Personal Interest", "Women's Rights & Gender Equality","Politics", "Sports", "Fashion")

# save data
save(docs, vocab, meta, labels, mod.15.new, file = "Data/stm.RData")