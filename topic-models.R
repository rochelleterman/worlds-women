setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/women")

### Load Data

women <- read.csv('women-no-nouns.csv')
names(women)

####################################
######### Topic Modeling ###########
####################################

library(stm)

temp<-textProcessor(documents=women$TEXT.NO.NOUN,metadata=women)
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

set.seed(02138)

##### 8

mod.8 <- stm(docs, vocab, 8, prevalence=~REGION, data=meta)
quality.8 <- topicQuality(model=mod.8, documents=docs)
labels.8 <- labelTopics(mod.8)
labels.8

##### 9

mod.9 <- stm(docs, vocab, 9, prevalence=~REGION, data=meta)
quality.9 <- topicQuality(model=mod.9, documents=docs)
labels.9 <- labelTopics(mod.9)
labels.9

##### 10

mod.10 <- stm(docs, vocab, 10, prevalence=~REGION, data=meta)
quality.10 <- topicQuality(model=mod.10, documents=docs)
labels.10 <- labelTopics(mod.9)
labels.10

##### 11

mod.11 <- stm(docs, vocab, 11, prevalence=~REGION, data=meta)
quality.11 <- topicQuality(model=mod.11, documents=docs)
labels.11 <- labelTopics(mod.9)
labels.11

##### 12 ## I really like this one

mod.12 <- stm(docs, vocab, 12, prevalence=~REGION, data=meta)
quality.12 <- topicQuality(model=mod.12, documents=docs)
labels.12 <- labelTopics(mod.12)
labels.12

##### 13 ## like this too

mod.13 <- stm(docs, vocab, 13, prevalence=~REGION, data=meta)
quality.13 <- topicQuality(model=mod.13, documents=docs)
labels.13 <- labelTopics(mod.13)
labels.13


##### 13 Content 
mod.13 <- stm(docs, vocab, 13, prevalence=~REGION, content=~REGION,data=meta)
quality.13 <- topicQuality(model=mod.13, documents=docs)
labels.13 <- labelTopics(mod.13)
labels.13
sageLabels(mod.13)

##### 14

mod.14 <- stm(docs, vocab, 14, prevalence=~REGION, data=meta)
quality.14 <- topicQuality(model=mod.14, documents=docs)
labels.14 <- labelTopics(mod.14)
labels.14


#### 15
mod.15 <- stm(docs, vocab, 15, prevalence=~REGION, data=meta)
quality.15 <- topicQuality(model=mod.15, documents=docs)
labels.15 <- labelTopics(mod.15)
labels.15



### EstimateEffect
prep <- estimateEffect(c(1) ~ REGION,mod.10,meta)

plot.estimateEffect(prep, "REGION", model=mod.10,
                    method="pointestimate")
plot.estimateEffect(prep, "REGION", model=mod.10,
                    method="difference",cov.value1="MENA",cov.value2="EECA")

cloud(mod.10,6)

thoughts <- findThoughts(mod.15,texts=as.character(meta$TITLE),topics=15,n=2)
plotQuote(thoughts)
thoughts

### select model
mod.out <- selectModel(docs, vocab, K=3, prevalence=~REGION, 
                       data=meta, runs=5, verbose=TRUE)
plotModels(mod.out)
mod.out$semcoh
selected<-mod.out$runout[[1]]
summary(selected)      

### Topic Corr

cormat <- topicCorr(mod.12)
plot(cormat)
           
k = c(1,15,20)
kresult <- searchK(docs,vocab,k,prevalence=~REGION,data=meta)