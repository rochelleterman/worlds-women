### This script analyzes how the same topic is discussed differently depending on ### the region it covers. It does this by:
### 1. Subsets the meta.topics data into topic-specific lists of documents (i.e. about "Marriage")
### documents with that topic as its top topic.
### 2. Performs word seaprating algorithms
### 3. Gets reprsentative tites

#  Prepping.
rm(list=ls())
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")
library("matrixStats")
library(RTextTools)
library(tm)
require(rJava) # needed for stemming function 
library(Snowball) # also needed for stemming function

##############################
######## DTM function ########
##############################

# this function passes a subcorpus of the meta-topics.csv data for documents on a specific topic, and makes a DTM out of it. There are many ways one could subset. See below for options.

make.dtm <- function(data){
  a <- Corpus(VectorSource(data[["TEXT.NO.NOUN"]]))
  a <- tm_map(a, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
  a <- tm_map(a, tolower) # convert all text to lower case
  a <- tm_map(a, removePunctuation) 
  a <- tm_map(a, removeNumbers)
  a <- tm_map(a, removeWords, c(stopwords("english"),stopwords.country),mc.cores=1)
  a <- tm_map(a, stemDocument, language = "english",mc.cores=1) # converts terms to tokens
  a<- TermDocumentMatrix(a, control=list()) #convert to term document matrix, words have to be in at least minDocFreq to appear, I set it to 5, but you can change this.
  a <- removeSparseTerms(a, sparse=0.999) #remove sparse terms, the sparse number should be higher with a large number of documents, smaller with small number of documents, always less than 1
  a <- as.data.frame(inspect(a)) # convert document term matrix to data frame
  a <- t(a) #transpose matrix
  a <- as.data.frame(a) # convert back to dataframe
  a$region <-data[["REGION"]] # add col for region
  return(a)
}

##########################################
######## Word Separating Function ########
##########################################

# The following function inputs a region and returns the scores of three word separating algorithms - Linear Discriminant Analysis, Standardized Mean Difference and Standardized Log Odds. First you have to assign uni.dtm to the topic-specific dtm you made using the function above. See below.

distinctive.words <- function(region){
  corp.1.uni <- uni.dtm[grep(region,uni.dtm$region),]
  corp.1.uni$region <- NULL
  corp.2.uni <- uni.dtm[-(grep(region,uni.dtm$region)),]
  corp.2.uni$region <- NULL
  
  # calculate means and vars
  means.corp.1 <- colSums(corp.1.uni) / sum(colSums(corp.1.uni))
  var.corp.1 <- colVars(as.matrix(corp.1.uni))
  means.corp.2 <- colSums(corp.2.uni) / sum(colSums(corp.2.uni))
  var.corp.2 <- colVars(as.matrix(corp.2.uni))
  
  n.corp.1 <- sum(colSums(corp.1.uni))
  n.corp.2 <- sum(colSums(corp.2.uni))
  x.corp.1 <- colSums(corp.1.uni)
  x.corp.2 <- colSums(corp.2.uni)
  
  #################################################
  #### Independent linear discriminant measure ####
  #### used in Mosteller and Wallace (1963) #######
  #################################################
  
  # This function takes in the corp.1 and corp.2 document term matricizes and outputs the independent linear discriminant score to each word.
  
  l.d.scores <- function(corp.1,corp.2){ # where x,y args are dtm dataframes for the two authors
    #calculate overall score
    scores <- (means.corp.1 - means.corp.2) / (var.corp.1 + var.corp.2)
    
    #putting that score in a dataframe
    scores <- data.frame(cbind(scores))
    names(scores) <- "ld"
    return(scores)
  }
  
  uni <- l.d.scores(corp.1.uni,corp.2.uni) # apply to unigrams count dtm
  uni$words <- rownames(uni)
  
  ######################################
  #### Standardized mean difference ####
  ######################################
  
  # A function to get Standardized mean difference scores
  s.m.d.scores <- function(corp.1,corp.2){ # where x,y args are dtm dataframes for the two authors
    score <- (means.corp.1 - means.corp.2) / sqrt((var.corp.1/n.corp.1) + (var.corp.2/n.corp.1))
    return(score)
  }
  
  uni$smd <- s.m.d.scores(corp.1.uni,corp.2.uni) # unigrams count dtm
  
  #################################
  ##### Standardized Log Odds #####
  #################################
  
  # A function to find Standardized Log Odds scores
  s.l.o.scores <- function(corp.1,corp.2){ # where x,y args are dtm dataframes for the two authors
    # calculate means and vars
    pi.corp.1 <- (x.corp.1 + 1) / (n.corp.1 + ncol(corp.1)-1)
    pi.corp.2 <- (x.corp.2 + 1) / (n.corp.2 + ncol(corp.2)-1)  
    log.odds.ratio <- log(pi.corp.1/(1-pi.corp.1)) - log(pi.corp.2 / (1-pi.corp.2))
    st.log.odds <- log.odds.ratio/sqrt(var(log.odds.ratio))
    return(st.log.odds)
  }
  
  uni$stlogoddse <- s.l.o.scores(corp.1.uni,corp.2.uni)
  
  # Write the words. Note that you have to change the file name as per what corpus your analyzing (see above). 
  uni$words <- NULL
  return(uni)
}

##############################
###### make sub-corpus #######
##############################

# Option 1) get docs with distribution of topics >.5
get.highest.docs <- function(x){
  docs <- subset(meta.topics,topic.docs[[x]]>.4,select=c(x,"PUBLICATION","TITLE","YEAR","COUNTRY_FINAL","REGION","SUBJECT","TEXT","TEXT.NO.NOUN"))
  docs <- docs[order(docs[[x]],decreasing = TRUE),]
  return(docs)
}
rights <- get.highest.docs("rights")

# Option 2) find documents for each topic using top topic
rights <- meta.topics[meta.topics$top.topic == "rights",]

# Option 3) Get 20 most representative articles for each region on the topic
highest <- arrange(meta.topics,REGION,desc(rights)) 
rights <- rbind(head(highest[highest$REGION=="Africa",],20),head(highest[highest$REGION=="Asia",],20),head(highest[highest$REGION=="EECA",],20),head(highest[highest$REGION=="LA",],20),head(highest[highest$REGION=="MENA",],20),head(highest[highest$REGION=="West",],20))

##############################
###### distinctive words #####
##############################

# call function
rights <- make.dtm(rights)

# Get Discrimianting Words
uni.dtm <- rights

# apply function
mena.uni <- distinctive.words("MENA")
eeca.uni <- distinctive.words("EECA")
west.uni <- distinctive.words("West")
africa.uni <- distinctive.words("Africa")
la.uni <- distinctive.words("LA")
asia.uni <- distinctive.words("Asia")

### write csv of all regions' top words
names(asia.uni)
top.200.smd <- function(data){
  return(rownames(data[order(data[,"smd"],decreasing=TRUE)[1:200],]))
}
topic.dist <- data.frame(top.200.smd(africa.uni),top.200.smd(asia.uni),top.200.smd(eeca.uni),top.200.smd(la.uni),top.200.smd(mena.uni),top.200.smd(west.uni))

names(topic.dist) <- c("Africa","Asia","EECA","LA","MENA","West")

write.csv(topic.dist,"Results/distinctive-words/rights-dist.csv")

# write CSVs of individual regions with their 3 scores
setwd("Results/distinctive-words/Rape")

write.order <- function(data,filename){
  order <- data[order(data[,"smd"],decreasing=TRUE)[1:200],]
  order <- subset(order,select="smd")
  write.table(order,file = filename,col.names = FALSE,sep = ":")
}

write.order(mena.uni,"mena.txt")
write.order(la.uni,"la.txt")
write.order(eeca.uni,"eeca.txt")
write.order(west.uni,"west.txt")
write.order(asia.uni,"asia.txt")
write.order(africa.uni,"africa.txt")

# find n for each region

names(rights)
summary(rights$REGION)
rights$TITLE[rights$REGION=="LA"]

##############################################
######### Find docs per Region/topic #########
##############################################

# find most representative titles for topic by region (i.e. marriage)
highest <- arrange(meta.topics,REGION,desc(rights)) 

highest <- data.frame(head(highest[highest$REGION=="Africa","TITLE"],20),head(highest[highest$REGION=="Asia","TITLE"],20),head(highest[highest$REGION=="EECA","TITLE"],20),head(highest[highest$REGION=="LA","TITLE"],20),head(highest[highest$REGION=="MENA","TITLE"],20),head(highest[highest$REGION=="West","TITLE"],20))

names(highest) <- c("Africa","Asia","EECA","LA","MENA","West")

write.csv(highest,"Results/titles/rights-highest.csv")

# random sample titles by top-topic.
sample<- meta.topics[meta.topics$top.topic=="rights",]
names(sample)
sample <- data.frame(sample(sample[sample$REGION=="Africa","TITLE"],5),sample(sample[sample$REGION=="Asia","TITLE"],5),sample(sample[sample$REGION=="EECA","TITLE"],5),sample(sample[sample$REGION=="LA","TITLE"],5),sample(sample[sample$REGION=="MENA","TITLE"],5),sample(sample[sample$REGION=="West","TITLE"],5))
sample
write.csv(sample,"Results/titles/rights-sample.csv")

# find all titles (and articles) in sub-corpus.
rights <- subset(meta.topics, top.topic=="rights",select=c(TITLE,REGION,TEXT,YEAR,COUNTRY_FINAL))
rights <- arrange(rights,REGION)
write.csv(rights,"Results/Rights-articles.csv")

