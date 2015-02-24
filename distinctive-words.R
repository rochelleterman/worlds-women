### This script analyzes human rights coverage with word separating algorithms.

#  Prepping.
rm(list=ls())
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")
library("matrixStats")

# read in data
uni.dtm<- read.csv("dtm.csv")
uni.dtm$year <- NULL
uni.dtm$region <- as.character(uni.dtm$region)
uni.dtm[,2:10001] <- lapply(uni.dtm[,2:10001],as.integer)
corp.1.uni <- uni.dtm[grep("MENA",uni.dtm$region),]
corp.1.uni$region <- NULL
means.corp.1 <- colSums(corp.1.uni) / sum(colSums(corp.1.uni))
corp.2.uni <- uni.dtm[-(grep("MENA",uni.dtm$region)),]
corp.2.uni$region <- NULL

# The following function inputs a region and returns the scores of three word separating algorithsms - Linear Discriminant analysis, Standardized Mean Difference and Standardized Log Odds

distinctive.words <- function(region){
  corp.1.uni <- uni.dtm[grep(region,uni.dtm$region),]
  corp.1.uni$region <- NULL
  corp.2.uni <- uni.dtm[-(grep(region,uni.dtm$region)),]
  corp.2.uni$region <- NULL
  
  #################################################
  #### Independent linear discriminant measure ####
  #### used in Mosteller and Wallace (1963) #######
  #################################################
  
  # This function takes in the corp.1 and corp.2 document term matricizes and outputs the independent linear discriminant score to each word.
  
  l.d.scores <- function(corp.1,corp.2){ # where x,y args are dtm dataframes for the two authors
    # calculate means and vars
    means.corp.1 <- colSums(corp.1) / sum(colSums(corp.1))
    var.corp.1 <- colVars(as.matrix(corp.1))
    means.corp.2 <- colSums(corp.2) / sum(colSums(corp.2))
    var.corp.2 <- colVars(as.matrix(corp.2))
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
    # calculate means and vars
    n.corp.1 <- sum(colSums(corp.1))
    n.corp.2 <- sum(colSums(corp.2))
    means.corp.1 <- colSums(corp.1) / n.corp.1
    var.corp.1 <- colVars(as.matrix(corp.1))
    means.corp.2 <- colSums(corp.2) / n.corp.2
    var.corp.2 <- colVars(as.matrix(corp.2))
    
    #calculate overall score
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
    n.corp.1 <- sum(colSums(corp.1))
    n.corp.2 <- sum(colSums(corp.2))
    x.corp.1 <- colSums(corp.1)
    x.corp.2 <- colSums(corp.2)
    pi.corp.1 <- (x.corp.1 + 1) / (n.corp.1 + ncol(corp.1))
    pi.corp.2 <- (x.corp.2 + 1) / (n.corp.2 + ncol(corp.2))  
    log.odds.ratio <- log(pi.corp.1/(1-pi.corp.1)) - log(pi.corp.2 / (1-pi.corp.2))
    st.log.odds <- log.odds.ratio/sqrt(var(log.odds.ratio))
    return(st.log.odds)
  }
  
  uni$stlogoddse <- s.l.o.scores(corp.1.uni,corp.2.uni)
  
  # Write the words. Note that you have to change the file name as per what corpus your analyzing (see above). 
  uni$words <- NULL
  return(uni)
}

# apply function
mena.uni <- distinctive.words("MENA")
eeca.uni <- distinctive.words("EECA")
west.uni <- distinctive.words("West")
africa.uni <- distinctive.words("Africa")
la.uni <- distinctive.words("LA")
asia.uni <- distinctive.words("Asia")

# write CSVs
setwd("Results/distinctive\ words")

write.csv(mena.uni,"mena.csv")
write.csv(eeca.uni,"eeca.csv")
write.csv(west.uni,"west.csv")
write.csv(africa.uni,"africa.csv")
write.csv(asia.uni,"asia.csv")
write.csv(la.uni,"la.csv")

# write function to get top 200 words for a particular score

top.200 <- function(data,score){
  return(rownames(data[order(score,decreasing=TRUE),])[1:200])
}
top.200(mena.uni,mena.uni$ld)
