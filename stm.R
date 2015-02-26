setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

### Load Data

women <- read.csv('Data/women-processed.csv')
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

storage <- manyTopics(docs,vocab,K=c(10,15,20),prevalence=~REGION+s(YEAR),data=meta,runs=10,max.em.its=50)

mean(storage$exclusivity[[3]])

mod.10 <- storage$out[[1]] # most coherent
mod.15 <- storage$out[[2]] 
mod.20 <- storage$out[[3]] # most exclusive 

### Select Model with fixed number of topics
mod.14.select <- selectModel(docs,vocab,K=14,prevalence=~REGION+s(YEAR),max.em.its=50,data=meta,runs=10,seed=12345)

plotModels(mod.14.select) # Plot of selectModel results. Numerals represent the average for each model, and dots represent topic specific scores

mod.14.select <- mod.14.select$runout[[2]] # choose model
summary(mod.14.select)

### Tryin other number of topics

mod.20.select <- selectModel(docs,vocab,K=20,prevalence=~REGION+s(YEAR),data=meta,runs=15,seed=33333)
plotModels(mod.20)
mod.20.fit <- mod.20.select$runout[[1]]
topicQuality(model=mod.20.fit, documents=docs)
labelTopics(mod.20.fit)

mod.20 <- stm(docs,vocab, 20, prevalence=~REGION+s(YEAR), data=meta, seed = 22222)
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


plotQuote(thoughts1, width=40, main="Topic 1")# per. stories
plotQuote(thoughts2, width=40, main="Topic 2") # fgm
plotQuote(thoughts3, width=40, main="Topic 3") #  rights / protests
plotQuote(thoughts4, width=40, main="Topic 4") # politics
plotQuote(thoughts5, width=40, main="Topic 5") # law, abortion
plotQuote(thoughts6, width=40, main="Topic 6") #religion
plotQuote(thoughts7, width=40, main="Topic 7") # rape
plotQuote(thoughts8, width=40, main="Topic 8") # medicine
plotQuote(thoughts9, width=40, main="Topic 9") # sports
plotQuote(thoughts10, width=40, main="Topic 10") #maternal health
plotQuote(thoughts11, width=40, main="Topic 11") #war ???
plotQuote(thoughts12, width=40, main="Topic 12") #buxiness
plotQuote(thoughts13, width=40, main="Topic 13") #political violence
plotQuote(thoughts14, width=40, main="Topic 14") # art, theater
plotQuote(thoughts15, width=40, main="Topic 15") # sex tourism
plotQuote(thoughts16, width=40, main="Topic 16") # books
plotQuote(thoughts17, width=40, main="Topic 17") # marriage + family
plotQuote(thoughts18, width=40, main="Topic 18") # travel
plotQuote(thoughts19, width=40, main="Topic 19") # fashion
plotQuote(thoughts20, width=40, main="Topic 20") # education

labels = c("Personal stories","FGM","Rights","Politics","Law","Religion","Sexual Assault","Medicine","Sports","Reproductive Health","War","Business","Political Violence","Art, Theater","Prostitution","Literature","Marriage + Family","Travel","Fashion","Education")

#### Corpus level summaries

plot.STM(model,type="summary",custom.labels=labels,covarlevels=regions)

plot.STM(model,type="hist",custom.labels=labels)

plot.STM(model,type="labels",width=100)


# Topic Correlation
mod.out.corr<-topicCorr(model)
plot.topicCorr(mod.out.corr)

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



