### This script analyzes how the same topic is discussed differently depending on ### the region it covers. It does this by:
### 1. Subsets the meta.topics data into topic-specific lists of documents (i.e.)
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

# this function passes data derived from the meta-topics data

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

#test
dtm <- make.dtm(meta)

##############################
######## W-S function ########
##############################

# The following function inputs a region and returns the scores of three word separating algorithsms - Linear Discriminant analysis, Standardized Mean Difference and Standardized Log Odds

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
######## make dtms ###########
##############################

# find documents for each topic
religion <- meta.topics[meta.topics$top.topic == "religion",]
marriage <- meta.topics[meta.topics$top.topic == "marriage",]
rape <- meta.topics[meta.topics$top.topic == "rape",]
rights <- meta.topics[meta.topics$top.topic == "rights",]

# call function
religion.dtm <- make.dtm(religion)
marriage.dtm <- make.dtm(marriage)
rape.dtm <- make.dtm(rape)
rights.dtm <- make.dtm(rights)


# Get Discrimianting Words

uni.dtm <- rights.dtm

# apply function
mena.uni <- distinctive.words("MENA")
eeca.uni <- distinctive.words("EECA")
west.uni <- distinctive.words("West")
africa.uni <- distinctive.words("Africa")
la.uni <- distinctive.words("LA")
asia.uni <- distinctive.words("Asia")

# write CSVs
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

### write csv of all region top words

top.200.smd <- function(data){
  return(rownames(data[order(data[,"smd"],decreasing=TRUE)[1:200],]))
}
rights.dist <- data.frame(top.200.smd(africa.uni),top.200.smd(asia.uni),top.200.smd(eeca.uni),top.200.smd(mena.uni),top.200.smd(la.uni),top.200.smd(west.uni))

write.csv(rights.dist,"Results/distinctive-words/rights-dist.csv")

# write function to get top 200 words for a particular score

top.200 <- function(data,score){
  return(rownames(data[order(score,decreasing=TRUE),])[1:200])
}
top.200(asia.uni,asia.uni$smd)
head(sort(mena.uni$smd),10)

# find n for each region

names(marriage)
summary(rape$REGION)

##############################################
######### Find docs per Region/topic #########
##############################################

# get docs with distribution of topics >.5 - used for other scripts
get.highest.docs <- function(x){
  docs <- subset(meta.topics,topic.docs[[x]]>.5,select=c(x,"PUBLICATION","TITLE","YEAR","COUNTRY_FINAL","REGION","SUBJECT","TEXT","TEXT.NO.NOUN"))
  docs <- docs[order(docs[[x]],decreasing = TRUE),]
  return(docs)
}

# find most representative titles for topic by region (i.e. marriage)

marriage.highest <- arrange(meta.topics,REGION,desc(marriage)) 

marriage.highest <- data.frame(head(marriage.highest[marriage.highest$REGION=="Africa","TITLE"],10),head(marriage.highest[marriage.highest$REGION=="Asia","TITLE"],10),head(marriage.highest[marriage.highest$REGION=="MENA","TITLE"],10))

names(marriage.highest) <- c("Africa","Asia","MENA")

write.csv(marriage.highest,"Results/titles/marriage-highest.csv")
