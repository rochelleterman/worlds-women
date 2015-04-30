# clustering
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

dtm$region <- NULL

set.seed(1234)

clust <- kmeans(dtm,10)

# label

centers <- as.data.frame(clust$centers) #make dataframe of cluster centers

# write a function that inputs cluser number k (e.g. k = 2) outputs the top 10 words as per the definition given
top10words <- function(k){
  theta.k <- centers[k,] # define theta-k, i.e.  row k of cluster centers dataframe
  theta.notk <- colSums(centers[-(k),])/5 # define theta-not-k, i.e. rows not-k of cluster centers divided by 5 (number of clusters - 1)
  diffk <- as.data.frame(theta.k - theta.notk) # define difference diffk
  return(colnames(diffk[,order(diffk,decreasing=TRUE)][1:10])) # order decreasing, take top 10
}
keywords<- matrix(NA, nrow=10, ncol=10) # set up a matrix to contain data
for (i in 1:10){
  keywords[,i] <- top10words(i)
}
keywords

# sample

set.seed(1234)
sample(names(which(clust$cluster==15)),4) 
meta$TITLE[1298]
