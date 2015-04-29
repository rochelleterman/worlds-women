####### makes a dtm of a set of documents

# read in data

#women <- read.csv('Data/women-no-nouns.csv')

library(RTextTools)
library(tm)
require(rJava) # needed for stemming function 
library(Snowball) # also needed for stemming function

# write a fuction that passes a vector from my meta 

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
x <- make.dtm(business)

