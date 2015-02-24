setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

### Load Data

women <- read.csv('women-no-nouns.csv')
names(women)

####################################
######### Pre-processing ###########
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
meta$TITLE <- as.character(meta$TITLE)

set.seed(02138)

plotRemoved(out$documents,lower.thresh=seq(1,200,by=100))

##################################
######### Choose Model ###########
##################################


### Model search across numbers of topics

storage <- manyTopics(docs,vocab,K=c(13,14,15),prevalence=~REGION+s(YEAR),data=meta,runs=10,max.em.its=50)

mean(storage$semcoh[[1]])

mod.13 <- storage$out[[1]] # most coherent
mod.14 <- storage$out[[2]] # most exclusive 
mod.15 <- storage$out[[3]]

### Select Model with fixed number of topics
mod.14.select <- selectModel(docs,vocab,K=14,prevalence=~REGION+s(YEAR),max.em.its=50,data=meta,runs=10,seed=12345)

plotModels(mod.14.select) # Plot of selectModel results. Numerals represent the average for each model, and dots represent topic specific scores

mod.14.select <- mod.14.select$runout[[2]] # choose model
summary(mod.14.select)

### Tryin other number of topics

mod.13.select <- selectModel(docs,vocab,K=13,prevalence=~REGION+s(YEAR),data=meta,runs=15,seed=33333)
plotModels(mod.13.select)
mod.13 <- mod.13.select$runout[[1]]
topicQuality(model=mod.13, documents=docs)
labelTopics(mod.13)

mod.13.fit <- stm(docs,vocab, 13, prevalence=~REGION+s(YEAR), data=meta, seed = 12345 )
labelTopics(mod.13.fit)
topicQuality(model=mod.13.fit, documents=docs)

mod.20 <- stm(docs,vocab, 20, prevalence=~REGION+s(YEAR), data=meta, seed = 12345 )
labelTopics(mod.20)
topicQuality(model=mod.20, documents=docs)

### Content covariate
mod.13.content <- stm(docs, vocab, 13, prevalence=~REGION+s(YEAR), content=~REGION, data=meta, max.em.its=75, seed=12345)
topicQuality(model=mod.13.content, documents=docs)
labelTopics(mod.13.content)


####################################
######### Explore Topics ###########
####################################

model <- mod.20

# Labels
labelTopics(model)

# Example Docs

thoughts1 <- findThoughts(model,texts=as.character(meta$TITLE),n=2,topics=1)$docs[[1]]
thoughts2 <- findThoughts(model,texts=meta$TITLE,n=2,topics=2)$docs[[1]]
thoughts3 <- findThoughts(model,texts=meta$TITLE,n=2,topics=3)$docs[[1]]
thoughts4 <- findThoughts(model,texts=meta$TITLE,n=2,topics=4)$docs[[1]]
thoughts5 <- findThoughts(model,texts=meta$TITLE,n=2,topics=5)$docs[[1]]
thoughts6 <- findThoughts(model,texts=meta$TITLE,n=2,topics=6)$docs[[1]]
thoughts7 <- findThoughts(model,texts=meta$TITLE,n=2,topics=7)$docs[[1]]
thoughts8 <- findThoughts(model,texts=meta$TITLE,n=2,topics=8)$docs[[1]]
thoughts9 <- findThoughts(model,texts=meta$TITLE,n=2,topics=9)$docs[[1]]
thoughts10 <- findThoughts(model,texts=meta$TITLE,n=2,topics=10)$docs[[1]]
thoughts11 <- findThoughts(model,texts=meta$TITLE,n=2,topics=11)$docs[[1]]
thoughts12 <- findThoughts(model,texts=meta$TITLE,n=2,topics=12)$docs[[1]]
thoughts13 <- findThoughts(model,texts=meta$TITLE,n=2,topics=13)$docs[[1]]
thoughts14 <- findThoughts(model,texts=meta$TITLE,n=2,topics=14)$docs[[1]]
thoughts15 <- findThoughts(model,texts=meta$TITLE,n=2,topics=15)$docs[[1]]
thoughts16 <- findThoughts(model,texts=meta$TITLE,n=2,topics=16)$docs[[1]]
thoughts17 <- findThoughts(model,texts=meta$TITLE,n=2,topics=17)$docs[[1]]
thoughts18 <- findThoughts(model,texts=meta$TITLE,n=2,topics=18)$docs[[1]]
thoughts19 <- findThoughts(model,texts=meta$TITLE,n=2,topics=19)$docs[[1]]
thoughts20 <- findThoughts(model,texts=meta$TITLE,n=2,topics=20)$docs[[1]]


plotQuote(thoughts1, width=40, main="Topic 1")# books
plotQuote(thoughts2, width=40, main="Topic 2") # sexual assault
plotQuote(thoughts3, width=40, main="Topic 3") # media
plotQuote(thoughts4, width=40, main="Topic 4") # pol. violence 
plotQuote(thoughts5, width=40, main="Topic 5") #war + rights?
plotQuote(thoughts6, width=40, main="Topic 6") #religion + rights?
plotQuote(thoughts7, width=40, main="Topic 7") #trafficking + FGM
plotQuote(thoughts8, width=40, main="Topic 8") #politics + elections
plotQuote(thoughts9, width=40, main="Topic 9") #film, art, theater
plotQuote(thoughts10, width=40, main="Topic 10") #travel
plotQuote(thoughts11, width=40, main="Topic 11") #sports
plotQuote(thoughts12, width=40, main="Topic 12") #education
plotQuote(thoughts13, width=40, main="Topic 13") #gender equality
plotQuote(thoughts14, width=40, main="Topic 14") #diplomacy? international relations?
plotQuote(thoughts15, width=40, main="Topic 15") # obit
plotQuote(thoughts16, width=40, main="Topic 16") # reproductive + maternal health
plotQuote(thoughts17, width=40, main="Topic 17") # personal stories
plotQuote(thoughts18, width=40, main="Topic 18") # business
plotQuote(thoughts19, width=40, main="Topic 19") # fashion
plotQuote(thoughts20, width=40, main="Topic 20") # cancer



### Topics, meta data relationships

#prep
prep <- estimateEffect(1:20 ~ REGION + s(YEAR),model,meta=meta,uncertainty="Global")

# topics over time
plot.estimateEffect(prep,"YEAR",method="continuous",topics=11,printlegend=TRUE,xlab="Year",xlim=c(1990,2014))

# topics over region
regions = c("Asia","EECA","MENA","Africa","LA","West")
plot.estimateEffect(prep,"REGION",method="pointestimate",topics=9,printlegend=TRUE,labeltype="custom",custom.labels=regions,main="Topic X",xlab="topic proportions")

# difference
plot.estimateEffect(prep,"REGION",method="difference",topics=c(1,2,4),cov.value1="MENA",cov.value="LA",printlegend=TRUE,main="Topic 2")


#### Interactions

mod.20.int <- stm(docs,vocab, 20, prevalence=~REGION*s(YEAR), data=meta, seed = 12345 )
labelTopics(mod.20)
topicQuality(model=mod.20, documents=docs)

prep.int <- estimateEffect(1:20 ~ REGION * YEAR,mod.20.int,meta=meta,uncertainty="Global")

# topics over time by region
plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=1,moderator="REGION",moderator.value="West",linecol="red", add=F,ylim=c(0,.25))

plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=1,moderator="REGION",moderator.value="MENA",linecol="blue", add=T, ylim=c(0,.25))

#### Corpus level summaries

plot.STM(model,type="summary")

# Topic Correlation
mod.out.corr<-topicCorr(model)
plot.topicCorr(mod.out.corr)


