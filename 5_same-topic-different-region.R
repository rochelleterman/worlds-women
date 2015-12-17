### This script analyzes how the same topic is discussed differently depending on 
### the region it covers. Specifically, it:
### 1. Subsets the corpus into topic-specific lists of documents (i.e. about "Rights")
### 2. Performs word separating algorithms to get distinctive words for each region
### 3. Gets reprsentative tites for each region / topic combo.

#  Prepping.
rm(list=ls())
setwd("~/Dropbox/berkeley/Git-Repos/worlds-women")
library("matrixStats")
library(RTextTools)
library(tm)
require(rJava) # needed for stemming function 
library(SnowballC) # also needed for stemming function

# Load Data
meta.topics <- read.csv("Data/topic-proportions/meta-topics.csv")

##############################
######## DTM function ########
##############################

# this function passes a subcorpus of the meta-topics.csv data for documents on a specific topic, 
# and makes a DTM out of it. There are many ways one could subset. See below for options.
make.dtm <- function(data){ 
  dtm <- create_matrix(data[["TEXT.NO.NOUN"]], language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.999, toLower = TRUE, 
                            removePunctuation = TRUE)
  dtm <- as.data.frame(as.matrix(dtm))
  dtm$region <- data[["REGION"]]
  dtm[,1] <- NULL
  return(dtm)
}


##########################################
######## Word Separating Function ########
##########################################

# The following function inputs a region and returns the scores of three word separating algorithms - 
# Linear Discriminant Analysis, Standardized Mean Difference and Standardized Log Odds. 
# The function passes two arguments: 
# 1) `region` (e.g. "mena"), and 2) `uni.dtm`, i.e. a DTM of a topical subcopus

distinctive.words <- function(region, uni.dtm){
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
    score <- (means.corp.1 - means.corp.2) / sqrt((var.corp.1/n.corp.1) + (var.corp.2/n.corp.2))
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
  
  uni$st.log.odds <- s.l.o.scores(corp.1.uni,corp.2.uni)
  
  # Write the words.
  uni$words <- NULL
  return(uni)
}

############################
##### Results Functions ####
############################

# this function passes the word scores (made above) and the specific score you want to order
# and returns the top 200 most distinctive words for that score
top.200 <- function(data,score){
  return(rownames(data[order(data[,score],decreasing=TRUE)[1:200],]))
}

# this function passes a score and returns a dataframe with each region's 
# top 200 words for that score
top.score <- function(score){
  df <- data.frame(matrix(data=NA,nrow=200,ncol=6))
  unis <- list(africa.uni, asia.uni, eeca.uni, la.uni, mena.uni, west.uni)
  for (i in 1:6){
    df[,i] <- top.200(unis[[i]],score)
  }
  names(df) <- c("Africa","Asia","EECA","LA","MENA","West")
  return(df)
}

##############################
###### make sub-corpus #######
##############################
names(meta.topics)
# Option 1) find documents for each topic using top topic
topic.subset <- meta.topics[meta.topics$top.topic == "rape",]

# Option 2), get docs with distribution of topics >.4, .5 etc
get.highest.docs <- function(x, n=.5){
  docs <- meta.topics[meta.topics[[x]]>n,c(x,"publication","title","year","country","region","subject","text","text.no.noun")]
  docs <- docs[order(docs[[x]],decreasing = TRUE),]
  return(docs)
}
topic.subset <- get.highest.docs("rights", .3)

# Option 3) Get 20 most representative articles for each region on the topic
highest <- arrange(meta.topics,REGION,desc(rights)) 
topic.subset <- rbind(head(highest[highest$REGION=="Africa",],20),head(highest[highest$REGION=="Asia",],20),head(highest[highest$REGION=="EECA",],20),head(highest[highest$REGION=="LA",],20),head(highest[highest$REGION=="MENA",],20),head(highest[highest$REGION=="West",],20))

# find n - how many documents in each region in the subcorpus?
summary(rights$REGION)

#############################
##### distinctive words #####
#############################

# Make DTM from topical subcorpus made above
topic.subset <- make.dtm(topic.subset)

# Get Distinctive Words Scores
mena.uni <- distinctive.words("MENA",topic.subset)
eeca.uni <- distinctive.words("EECA",topic.subset)
west.uni <- distinctive.words("West",topic.subset)
africa.uni <- distinctive.words("Africa",topic.subset)
la.uni <- distinctive.words("LA",topic.subset)
asia.uni <- distinctive.words("Asia",topic.subset)

#########################
##### write results #####
#########################

# get top 200 words for SMD
top.smd <- top.score("smd")

## get top 200 words for Standardized Log Odd
top.slo <- top.score("st.log.odds")

##############################################
######### Find docs per region/topic #########
##############################################

# Option 1) find most representative titles for topic by region (i.e. marriage)
highest <- arrange(meta.topics,REGION,desc(rights)) 
highest <- data.frame(head(highest[highest$REGION=="Africa","TITLE"],20),head(highest[highest$REGION=="Asia","TITLE"],20),head(highest[highest$REGION=="EECA","TITLE"],20),head(highest[highest$REGION=="LA","TITLE"],20),head(highest[highest$REGION=="MENA","TITLE"],20),head(highest[highest$REGION=="West","TITLE"],20))
names(highest) <- c("Africa","Asia","EECA","LA","MENA","West")

# Option 2) random sample titles by top-topic.
sample<- meta.topics[meta.topics$top.topic=="rights",]
names(sample)
sample <- data.frame(sample(sample[sample$REGION=="Africa","TITLE"],5),sample(sample[sample$REGION=="Asia","TITLE"],5),sample(sample[sample$REGION=="EECA","TITLE"],5),sample(sample[sample$REGION=="LA","TITLE"],5),sample(sample[sample$REGION=="MENA","TITLE"],5),sample(sample[sample$REGION=="West","TITLE"],5))
sample

# Option 3) find all titles (and articles) in sub-corpus.
rights <- subset(meta.topics, top.topic=="rights",select=c(TITLE,REGION,TEXT,YEAR,COUNTRY_FINAL))
rights <- arrange(rights,REGION)

##################################
#### Alt. Rights Classifier ######
##################################

## how many docs have the words "right" or "equal"

big.dtm <- make.dtm(meta.topics)
index <- which(big.dtm$right > 0 | big.dtm$equal | big.dtm$sexist > 0 | big.dtm$sexism > 0) # 2379 docs have the word "right" or "equal"

length(index) / nrow(big.dtm)

meta.topics$rights.boolean <- 0
meta.topics$rights.boolean[index] <- 1
names(meta.topics)
subset <- meta.topics[,c("iso3c", "year", "rights.boolean")]

require(plyr)
rights.country.year <- ddply(.data=meta.topics, .variables=.(iso3c,year), numcolwise(sum,na.rm = TRUE))
rights.country.year$n.docs <- (ddply(.data=docs,.variables=.(iso3c,year), .fun=nrow))$V1
rights.country.year$rights.perc <- rights.country.year$rights.boolean / rights.country.year$n.docs
write.csv(rights.country.year,"Data/topic-proportions/rights-boolean-country-year.csv")

summary(rights.country.year$rights.perc)
hist(rights.country.year$rights.perc)

require(ggplot2)
ggplot(rights.country.year, aes(x = iso3c, y = rights.perc)) + geom_bar(stat = "identity")
