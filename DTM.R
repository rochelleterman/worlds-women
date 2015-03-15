##### create DTM straight in R

# read in data

women <- read.csv('Data/women-no-nouns.csv')


library(RTextTools)
library(tm)

a <- Corpus(VectorSource(meta.rape$TEXT.NO.NOUN))
a <- tm_map(a, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
a <- tm_map(a, tolower) # convert all text to lower case
a <- tm_map(a, removePunctuation) 
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removeWords, stopwords("english"),mc.cores=1)

require(rJava) # needed for stemming function 
library(Snowball) # also needed for stemming function
a <- tm_map(a, stemDocument, language = "english",mc.cores=1) # converts terms to tokens
a.dtm <- TermDocumentMatrix(a, control=list()) #convert to term document matrix, words have to be in at least minDocFreq to appear, I set it to 5, but you can change this.
inspect(a.dtm[1:2,1:2]) # have a quick look at the term document matrix
findFreqTerms(a.dtm, lowfreq=1000) # have a look at common words
nrow(a.dtm) #gives you the number of unique words
a.dtm.sp <- removeSparseTerms(a.dtm, sparse=0.999) #remove sparse terms, the sparse number should be higher with a large number of documents, smaller with small number of documents, always less than 1
nrow(a.dtm.sp) #compare the number of unique words after removing sparse terms
a.dtm.sp.df <- as.data.frame(inspect(a.dtm.sp)) # convert document term matrix to data frame
nrow(a.dtm.sp.df) # check to see how many words are left
a.dtm.sp.df <- t(a.dtm.sp.df) #transpose matrix
a.dtm.sp.df <- as.data.frame(a.dtm.sp.df) # convert back to dataframe
nrow(a.dtm.sp.df) # check
meta.rape$REGION[1]
a.dtm.sp.df$region <- meta.rape$REGION # add col for region
a.dtm.sp.df$region[2] # checking

uni.dtm <- a.dtm.sp.df

# write.csv(a.dtm.sp.df,"dtm-r.csv")
