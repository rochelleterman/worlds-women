# This script analyses the estimated STM

setwd("~/Dropbox/berkeley/Git-Repos/worlds-women")
library(stm)
library(plyr)
library(xtable)
rm(list=ls())

### Load Data
load("Data/stm.RData")
model <- mod.15.new

############################
###### Label Topics ########
############################

# make a table with top words
prob <- labelTopics(model, n= 10)$prob
frex <- labelTopics(model, n=10)$frex
dat <- data.frame(labels)
dat$Probability <- apply( prob, 1 , paste , collapse = ", " )
dat$FREX <- apply( frex, 1 , paste , collapse = ", " )

# export to Latex
xtable(dat)

##################################
######### Plot Topics  ###########
##################################

# Corpus Summary of Topic Proportions
jpeg("Results/stm/corpus-summary.jpeg",width=700,height=550,type="quartz")
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
plot.estimateEffect(prep,covariate="YEAR",method="continuous",topics=c(12),printlegend=TRUE,xlab="Year",xlim=c(1980,2014),main = "Comparing Topics over Time",labeltype="custom",custom.labels=c("Rights"),ylim=c(0,.25),nsims=200)

# topics over region
regions = c("Asia","EECA","MENA","Africa","West","LA")
plot.estimateEffect(prep,"REGION",method="pointestimate",topics=12,printlegend=TRUE,labeltype="custom",custom.labels=regions,main="Women's Rights",ci.level=.95,nsims=100)

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

# estimate effect
prep.int <- estimateEffect(1:15 ~ REGION * YEAR,mod.15.int,meta=meta,uncertainty="Global") 

# plot topics over time by region
plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=3,moderator="REGION",moderator.value="MENA",linecol="red", add=F,ylim=c(0,.2),printlegend=F)

plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=12,moderator="REGION",moderator.value="EECA",linecol="blue", add=T, ylim=c(0,.2),printlegend=F)

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

# add number of words
n.words <- function(doc){
  return(sum(doc[2,]))
}
meta.topics$n.words <- as.numeric(lapply(docs, n.words))
cor(meta.topics$number.of.non.stop.words, meta.topics$n.words)

# subset
names(meta.topics)
meta.topics <- meta.topics[,c(1:18,23:26,28,30,31,37,38,39,40)]
names(meta.topics)[16:29] <- c("doc.number", "publication", "title", "entities", "iso3c", "text", "country", "year", "text.no.noun", "region", "type", "subject", "top.topic", "n.words")

#write csv for later
write.csv(meta.topics,"Data/topic-proportions/meta-topics.csv", row.names = F)

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
topic.distr$Total <- 100
topic.distr <- round(topic.distr,2)

xtable(topic.distr)
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


