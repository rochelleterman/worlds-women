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

load("Data/stm.RData")
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
  
##################################
######### Plot Topics  ###########
##################################

# Corpus Summary of Topic Proportions
jpeg("Results/stm/corpus-summary.jpeg",width=1000,height=1000,type="quartz")
plot.STM(model,type="summary",custom.labels=labels,main="")
dev.off()

# Topic Correlation
mod.out.corr<-topicCorr(model)
plot.topicCorr(mod.out.corr)

###########################################
######### MetaData Relatioships  ##########
###########################################

#prep
prep <- estimateEffect(1:15 ~ REGION+s(YEAR),model,meta=meta,uncertainty="Global",documents=docs)

# topics over time
plot.estimateEffect(prep,covariate="YEAR",method="continuous",topics=c(3),printlegend=TRUE,xlab="Year",xlim=c(1980,2014),main = "Comparing Topics over Time",labeltype="custom",custom.labels=c("Religions"),ylim=c(0,.25),nsims=200)

# topics over region
regions = c("Asia","EECA","MENA","Africa","West","LA")
plot.estimateEffect(prep,"REGION",method="pointestimate",topics=8,printlegend=TRUE,labeltype="custom",custom.labels=regions,main="Sexual Assault",ci.level=.95,nsims=100)

# Write Topic Proportion Estimates by Region
for (i in 1:15){
  file <- file.path("Results/stm/region-proportion-plots",paste(as.character(i),".png",sep = ""))
  jpeg(file,width=400,height=300,type="quartz")
  plot.estimateEffect(prep,"REGION",method="pointestimate",topics=i,printlegend=TRUE,labeltype="custom",custom.labels=regions,main=labels[i],ci.level=.95,nsims=100)
  dev.off()
}

#### Interactions

# fit model
mod.15.int <- stm(docs,vocab, 15, prevalence=~REGION*s(YEAR), data=meta, model=model)
labelTopics(mod.15.int)
topicQuality(model=mod.15.int, documents=docs)

# estimate effect
prep.int <- estimateEffect(1:15 ~ REGION * YEAR,mod.15.int,meta=meta,uncertainty="Global") 

# plot topics over time by region
plot.estimateEffect(prep,covariate="YEAR",method="continuous",topics=15,moderator="REGION",moderator.value="MENA",linecol="red", add=F,ylim=c(0,.2),printlegend=F)

plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=15,moderator="REGION",moderator.value="MENA",linecol="blue", add=T, ylim=c(0,.2),printlegend=F)

legend("topleft","(x,y)",legend=c("MENA","EECA"),fill=c("red","blue"))

#######################################################
######### Combine Meta Data + Topic Distributions #####
#######################################################

#Number of Documents by Number of Topics matrix of topic proportions
topic.docs <- as.data.frame(mod.15.new$theta) 
colnames(topic.docs) <- c("cancer", "reproductive", "religion", "business", "marriage", "arts", "migration", "rape", "war", "literature", "personal", "rights","politics", "sports", "fashion")

topic.docs$docs <- rownames(topic.docs)
meta.topics <- cbind(topic.docs,meta)
names(meta.topics)

# add column for top topic for each article
meta.topics$top.topic <- names(topic.docs)[apply(topic.docs, 1, which.max)] 

#write csv for later
write.csv(meta.topics,"Data/meta-topics.csv", row.names = F)

##############################################
####### Topic-Document Proportion Tables #####
##############################################

# Proportion of topics represented by each region
topic.docs$docs <- NULL
topic.docs$region <- meta$REGION
topic.distr <- ddply(.data=topic.docs, .variables=.(region), numcolwise(sum,na.rm = TRUE))
rownames(topic.distr) <- topic.distr$region
topic.distr$region <- NULL
topic.distr
norm<-function(x){
  return (x/sum(x) * 100)
}
topic.distr <- apply(topic.distr,2,norm)
colnames(topic.distr) <- labels
topic.distr <- t(topic.distr)
topic.distr <- as.data.frame(topic.distr)
topic.distr$total <- 100
topic.distr <- round(topic.distr,2)
topic.distr
write.csv(topic.distr,"Results/stm/region-distributions-per-topic.csv")

#Proportion of region represented by each topic
names(meta)
topic.docs$region <- meta$REGION
names(topic.docs)
mean.regions <- ddply(.data=topic.docs, .variables=.(region), numcolwise(mean,na.rm = TRUE))
mean.regions
mean.regions <- as.data.frame(mean.regions)
rownames(mean.regions) <- mean.regions$region
mean.regions$region <- NULL
mean.regions <- t(mean.regions)
mean.regions <- mean.regions*100
colSums(mean.regions)
mean.regions <- rbind(mean.regions,colSums(mean.regions))
rownames(mean.regions)[16] <- "Total"
mean.regions <- round(mean.regions,2)
mean.regions
write.csv(mean.regions,"Results/stm/mean-regions.csv")

###################################
####### Save Data for Later #######
###################################

save(docs, vocab, meta, meta.topics, labels, mod.15.new, file = "Data/stm.RData")
