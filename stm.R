setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

library(stm)
#rm(list=ls())

### Load Data

women <- read.csv('Data/women-processed.csv')
names(women)

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

#Removing 20989 of 35946 terms (25007 of 1047373 tokens) due to frequency 
#Your corpus now has 4438 documents, 14957 terms and 1022366 tokens.


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
labelTopics(mod.20.4)
topicQuality(model=mod.20.4, documents=docs)

# 19

mod.19 <- stm(docs,vocab, 19, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 000001)
labelTopics(mod.19)
topicQuality(model=mod.19, documents=docs)

mod.19.1 <- stm(docs,vocab, 19, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 12345)
labelTopics(mod.19.1)
topicQuality(model=mod.19.1, documents=docs)

mod.19.2 <- stm(docs,vocab, 19, prevalence=~REGION+s(YEAR)+PUBLICATION, data=meta, seed = 11111)
labelTopics(mod.19.2)
topicQuality(model=mod.19.2, documents=docs)

### Content covariate

mod.13.content <- stm(docs, vocab, 13, prevalence=~REGION+s(YEAR), content=~REGION, data=meta, max.em.its=75, seed=12345)
topicQuality(model=mod.13.content, documents=docs)
labelTopics(mod.13.content)


####################################
######### Explore Topics ###########
####################################

model <- mod.20.4

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

# Labels
labelTopics(model)

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
plotQuote(thoughts12, width=40, main="Topic 12") #business
plotQuote(thoughts13, width=40, main="Topic 13") #political violence
plotQuote(thoughts14, width=40, main="Topic 14") # art, theater
plotQuote(thoughts15, width=40, main="Topic 15") # sex tourism
plotQuote(thoughts16, width=40, main="Topic 16") # books
plotQuote(thoughts17, width=40, main="Topic 17") # marriage + family
plotQuote(thoughts18, width=40, main="Topic 18") # travel
plotQuote(thoughts19, width=40, main="Topic 19") # fashion
plotQuote(thoughts20, width=40, main="Topic 20") # education

labels = c("1. Personal stories","2. FGM","3. Rights","4. Politics","5. Law","6. Religion","7. Sexual Assault","8. Medicine","9. Sports","10. Reproductive Health","11. War","12. Business","13. Political Violence","14. Art, Theater","15. Prostitution","16. Literature","17. Marriage + Family","18. Travel","19. Fashion","20. Education")

#### Corpus level summaries
regions = c("Asia","EECA","MENA","Africa","LA","West")
plot.STM(model,type="summary",custom.labels=labels)

plot.STM(model,type="hist")

plot.STM(model,type="labels",topic.names=labels[1:10],topics=1:10,width=75)
plot.STM(model,type="labels",topic.names=labels[11:20],topics=11:20,width=75)

# Topic Correlation
mod.out.corr<-topicCorr(model)
plot.topicCorr(mod.out.corr)

### Topics, meta data relationships

#prep
model <- mod.20
prep <- estimateEffect(1:20 ~ REGION + s(YEAR),model,meta=meta,uncertainty="Global")
prep2 <- estimateEffect(1:5 ~ REGION,model,meta=meta,uncertainty="Global")


# topics over time
plot.estimateEffect(prep,covariate="YEAR",method="continuous",topics=c(1,15),printlegend=TRUE,xlab="Year",xlim=c(1990,2014),main = "Comparing Topics over Time",labeltype="custom",custom.labels=c("FGM","Rape"),ylim=c(0,.2))

# topics over region
regions = c("Asia","EECA","MENA","Africa","LA","West")
plot.estimateEffect(prep,"REGION",method="pointestimate",topics=7,printlegend=TRUE,labeltype="custom",custom.labels=regions,main="Topic 7: Rape",xlab="topic proportions")

plot.estimateEffect(prep,"REGION",method="pointestimate",topics=13,printlegend=TRUE,labeltype="custom",custom.labels=regions,main="Topic 10: Reproductive Health",xlab="topic proportions")

plot.estimateEffect(prep,"REGION",method="pointestimate",topics=20,printlegend=TRUE,labeltype="custom",custom.labels=regions,main="Topic 12: Business",xlab="topic proportions")

plot.estimateEffect(prep,"REGION",method="pointestimate",topics=4,printlegend=TRUE,labeltype="custom",custom.labels=regions,main="Topic 4: Electoral Politics",xlab="topic proportions")

# difference
plot.estimateEffect(prep,"REGION",method="difference",topics=c(1,2,4),cov.value1="MENA",cov.value="LA",printlegend=TRUE,main="Topic 2")

#### Interactions

# fit model
mod.20.int <- stm(docs,vocab, 20, prevalence=~REGION*s(YEAR), data=meta, seed = 22222 )
labelTopics(mod.20.int)
topicQuality(model=mod.20.int, documents=docs)

# estimate effect
prep.int <- estimateEffect(1:20 ~ REGION * YEAR,mod.20.int,meta=meta,uncertainty="Global") 

# plot topics over time by region
plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=6,moderator="REGION",moderator.value="West",linecol="red", add=F,ylim=c(0,.25),printlegend=F,main="Religion over Time by Region")

plot.estimateEffect(prep.int,covariate="YEAR",method="continuous",topics=6,moderator="REGION",moderator.value="MENA",linecol="blue", add=T, ylim=c(0,.25),printlegend=F)

legend("topleft","(x,y)",legend=c("West","Mena"),fill=c("red","blue"))

# find documents in a particular topic

library(plyr)
topic.docs <- as.data.frame(mod.20$theta) #Number of Documents by Number of Topics matrix of topic proportions
colnames(topic.docs)
topic.docs$docs <- rownames(topic.docs)

# get 200 docs with highest distributions
topic.docs <- arrange(topic.docs,desc(V13))
docs.rape.index <- as.integer(topic.docs$docs[1:200]) 
docs.rape.index

# get the metadata for those 200 docs  s
meta.rape <- meta[docs.rape.index,]
