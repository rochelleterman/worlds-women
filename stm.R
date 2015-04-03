setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

library(stm)
rm(list=ls())

### Load Data
names(women)
women <- read.csv('Data/women-processed.csv')
#x <- subset(women,select=c(TITLE,YEAR))
#d <- x[which(duplicated(x)),]


names(women)
women$X <- NULL

####################################
######### Pre-processing ###########
####################################

# custom stopwords

countries <- read.csv("country_codes.csv")
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

#Removing 21353 of 36515 terms (25506 of 1047553 tokens) due to frequency 
#Your corpus now has 4411 documents, 15162 terms and 1022047 tokens.


##################################
######### Choose Model ###########
##################################

### Set K = 20

# selectModel
mod.20.select <- selectModel(docs,vocab,K=20,prevalence=~REGION+s(YEAR)+PUBLICATION,data=meta,runs=15,seed=33333)
plotModels(mod.20.select)
mod.20.fit <- mod.20.select$runout[[1]]
topicQuality(model=mod.20.fit, documents=docs)
labelTopics(mod.20.fit)


# straight STM = 15
mod.15 <- stm(docs,vocab, 15, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 11111)
labelTopics(mod.15)
topicQuality(model=mod.15, documents=docs)

mod.15.1 <- stm(docs,vocab, 15, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 00001)
labelTopics(mod.15.1)
topicQuality(model=mod.15.1, documents=docs)

jpeg("Results/stms/15.1-1.jpeg",width=700,height=1000,type="quartz")
plot.STM(mod.15.1,type="labels",topics=1:10,width=75)
dev.off()

jpeg("Results/stms/15.1-2.jpeg",width=700,height=1000,type="quartz")
plot.STM(mod.15.1,type="labels",topics=11:15,width=75)
dev.off()

mod.15.2 <- stm(docs,vocab, 15, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 12345)
labelTopics(mod.15.2)
topicQuality(model=mod.15, documents=docs)

jpeg("Results/stms/15.2-1.jpeg",width=700,height=1000,type="quartz")
plot.STM(mod.15.2,type="labels",topics=1:10,width=75)
dev.off()

jpeg("Results/stms/15.2-2.jpeg",width=700,height=1000,type="quartz")
plot.STM(mod.15.2,type="labels",topics=11:15,width=75)
dev.off()

mod.15.3 <- stm(docs,vocab, 15, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 12345)
labelTopics(mod.15.3)
topicQuality(model=mod.15.3, documents=docs)

jpeg("Results/stms/15.3-1.jpeg",width=700,height=1000,type="quartz")
plot.STM(mod.15.3,type="labels",topics=1:10,width=75)
dev.off()

jpeg("Results/stms/15.3-2.jpeg",width=700,height=1000,type="quartz")
plot.STM(mod.15.3,type="labels",topics=11:15,width=75)
dev.off()


# straight STM - 20
mod.20 <- stm(docs,vocab, 20, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 11111)
labelTopics(mod.20)
topicQuality(model=mod.20, documents=docs)

mod.20.1 <- stm(docs,vocab, 20, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 12345)
labelTopics(mod.20.1)
topicQuality(model=mod.20.1, documents=docs)

mod.20.2 <- stm(docs,vocab, 20, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 44444)
labelTopics(mod.20.2)
topicQuality(model=mod.20.2, documents=docs)

mod.20.3 <- stm(docs,vocab, 20, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 000001)
labelTopics(mod.20.3)
topicQuality(model=mod.20.3, documents=docs)

mod.20.4 <- stm(docs,vocab, 20, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 222222)
labelTopics(mod.20.4) #best
topicQuality(model=mod.20.4, documents=docs) 

# 19

mod.19 <- stm(docs,vocab, 19, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 000001)
labelTopics(mod.19)
topicQuality(model=mod.19, documents=docs)

mod.19.1 <- stm(docs,vocab, 19, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 22222)
labelTopics(mod.19.1)
topicQuality(model=mod.19.1, documents=docs)

mod.19.2 <- stm(docs,vocab, 19, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 33333)
labelTopics(mod.19.2)
topicQuality(model=mod.19.2, documents=docs)

mod.19.3 <- stm(docs,vocab, 19, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 12345)
labelTopics(mod.19.3)
topicQuality(model=mod.19.3, documents=docs)

mod.19.4 <- stm(docs,vocab, 19, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 11111)
labelTopics(mod.19.4)
topicQuality(model=mod.19.4, documents=docs)

## 18

mod.18 <- stm(docs,vocab, 18, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 000001)
labelTopics(mod.18)
topicQuality(model=mod.18, documents=docs)

mod.18.1 <- stm(docs,vocab, 18, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 22222)
labelTopics(mod.18.1)
topicQuality(model=mod.18.1, documents=docs)

mod.18.2 <- stm(docs,vocab, 18, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 33333)
labelTopics(mod.18.2)
topicQuality(model=mod.18.2, documents=docs)

### Content covariate

mod.13.content <- stm(docs, vocab, 13, prevalence=~REGION+s(YEAR), content=~REGION, data=meta, max.em.its=75, seed=12345)
topicQuality(model=mod.13.content, documents=docs)
labelTopics(mod.13.content)


####################################
######### Explore Topics ###########
####################################

model <-mod.15.1


# Example Docs

thoughts1 <- findThoughts(model,texts=as.character(meta$TITLE),n=3,topics=1)$docs[[1]]
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
#thoughts16 <- findThoughts(model,texts=meta$TITLE,n=3,topics=16)$docs[[1]]
#thoughts17 <- findThoughts(model,texts=meta$TITLE,n=3,topics=17)$docs[[1]]
#thoughts18 <- findThoughts(model,texts=meta$TITLE,n=3,topics=18)$docs[[1]]
#thoughts19 <- findThoughts(model,texts=meta$TITLE,n=3,topics=19)$docs[[1]]
#thoughts20 <- findThoughts(model,texts=meta$TITLE,n=3,topics=20)$docs[[1]]
# Labels
labelTopics(model)

plotQuote(thoughts1, width=40, main="Topic 1") # religion
plotQuote(thoughts2, width=40, main="Topic 2") # fashion
plotQuote(thoughts3, width=40, main="Topic 3") # business
plotQuote(thoughts4, width=40, main="Topic 4") # sex and fgm
plotQuote(thoughts5, width=40, main="Topic 5") # cancer
plotQuote(thoughts6, width=40, main="Topic 6") # gender equality / workplace
plotQuote(thoughts7, width=40, main="Topic 7") # tourism
plotQuote(thoughts8, width=40, main="Topic 8") # art
plotQuote(thoughts9, width=40, main="Topic 9") # personal 
plotQuote(thoughts10, width=40, main="Topic 10") # rape
plotQuote(thoughts11, width=40, main="Topic 11") # sports
plotQuote(thoughts12, width=40, main="Topic 12") # maternal health
plotQuote(thoughts13, width=40, main="Topic 13") # natural disasters?
plotQuote(thoughts14, width=40, main="Topic 14") # UN
plotQuote(thoughts15, width=40, main="Topic 15") # obit / family
#plotQuote(thoughts16, width=40, main="Topic 16") # books
#plotQuote(thoughts17, width=40, main="Topic 17") # war
#plotQuote(thoughts18, width=40, main="Topic 18") # activism
#plotQuote(thoughts19, width=40, main="Topic 19") # politics
#plotQuote(thoughts20, width=40, main="Topic 20") # Obit

labels = c("Business & Work","Women's Rights & Gender Equality","Marriage & Family","Religion","Human Interest","Literature","Cancer","Maternal Health & Population","Tourism","Rape & Sexual Assault","Arts","Sports","Combat","Fashion","Politics")

#### Corpus level summaries
regions = c("Asia","EECA","MENA","Africa","LA","West")
plot.STM(model,type="summary",custom.labels=labels,main="")

plot.STM(model,type="hist")


# Topic Correlation
mod.out.corr<-topicCorr(model)
plot.topicCorr(mod.out.corr)

### Topics, meta data relationships

#prep
model <- mod.15.1
prep <- estimateEffect(1:15 ~ REGION + s(YEAR),model,meta=meta,uncertainty="Global")


# topics over time
plot.estimateEffect(prep,covariate="YEAR",method="continuous",topics=c(15),printlegend=TRUE,xlab="Year",xlim=c(1980,2014),main = "Comparing Topics over Time",labeltype="custom",custom.labels=c("Politics"),ylim=c(0,.25))


# topics over region
regions = c("Asia","EECA","MENA","Africa","LA","West")
set.seed(11)
plot.estimateEffect(prep,"REGION",method="pointestimate",topics=3,printlegend=TRUE,labeltype="custom",custom.labels=regions,main="Politics")

# difference
plot.estimateEffect(prep,"REGION",method="difference",topics=c(1,2,4),cov.value1="MENA",cov.value="LA",printlegend=TRUE,main="Topic 2")

#### Interactions

# fit model
mod.15.int <- stm(docs,vocab, 15, prevalence=~REGION*s(YEAR), data=meta, model=mod.15.1)
labelTopics(mod.15.int)
topicQuality(model=mod.15.1, documents=docs)

# estimate effect
prep.int <- estimateEffect(1:15 ~ REGION * YEAR,mod.15.int,meta=meta,uncertainty="Global") 

# plot topics over time by region
plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=15,moderator="REGION",moderator.value="West",linecol="red", add=F,ylim=c(0,.25),printlegend=F,main="Sexual Assault over Time by Region")

plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=15,moderator="REGION",moderator.value="Africa",linecol="blue", add=T, ylim=c(0,.25),printlegend=F)

legend("topleft","(x,y)",legend=c("Asia","West"),fill=c("red","blue"))

# find documents in a particular topic

library(plyr)
topic.docs <- as.data.frame(mod.15.1$theta) #Number of Documents by Number of Topics matrix of topic proportions
colnames(topic.docs)
topic.docs$docs <- rownames(topic.docs)

# get 200 docs with highest distributions
topic.docs <- arrange(topic.docs,desc(V2))
proportions <- topic.docs[1:200,"V2"]
docs.index <- as.integer(topic.docs$docs[1:200]) 

# get the metadata for those 200 docs  s
meta.topic <- meta[docs.index,]
meta.topic$proportions <- proportions

#### regional mean topic distributions
names(meta)
topic.docs$region <- meta$REGION
names(topic.docs)
mean.regions <- ddply(.data=topic.docs, .variables=.(region), numcolwise(mean,na.rm = TRUE))
names(mean.regions)[2:16] <- labels
mean.regions
mean.regions <- as.data.frame(mean.regions)
rownames(mean.regions) <- mean.regions$region
mean.regions$region <- NULL
mean.regions <- t(mean.regions)
mean.regions
mean.regions <- round(mean.regions,4)
write.csv(mean.regions,"Results/mean.regions.csv")
