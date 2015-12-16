library(tm)
library(ggplot2)
library(lsa)
library(scatterplot3d)
library(SnowballC)

rm(list=ls())
setwd("~/Dropbox/berkeley/Git-Repos/worlds-women")

#------------------------------------------------------------------------------

# prepare
meta.topics <- read.csv("Data/topic-proportions/meta-topics.csv")
#sub <- meta.topics[1:100,]
docs <- Corpus(VectorSource(meta.topics$text))
tdm <- TermDocumentMatrix(docs,
                          control = list(stopwords = TRUE,
                                         tolower = TRUE,
                                         removeNumbers = TRUE,
                                         removePunctuation = TRUE,
                                         stemming=TRUE))
dim(tdm)
tdm.s <- removeSparseTerms(tdm, .99)
dim(tdm.s)
td.mat <- as.matrix(tdm.s)


#------------------------------------------------------------------------------

# MDS with LSA
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
dist.mat.lsa  # check distance mantrix

lsaSpace$tk['women',]

#------------------------------------------------------------------------------

# MDS
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + 
  geom_point(data = points, aes(x = x, y = y, color = sub$region)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(sub)))


#------------------------------------------------------------------------------

# plot
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 3)
#colors <- rep(c("blue", "green", "red"), each = 3)
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], 
              pch = 16, main = "Semantic Space Scaled to 3D", xlab = "x", ylab = "y", 
              zlab = "z", type = "h")

#================================================================================

# WORDSPACE

library(wordspace)
library(reshape2)

# make co-occurance matrix with td.mat
x <- apply(td.mat, 2,  function(x) as.numeric(x > 0))  #recode as 0/1
v <- x %*% t(x)                                   #the magic matrix 
diag(v) <- 0                                      #repalce diagonal
dimnames(v) <- list(rownames(td.mat), rownames(td.mat))                #name the dimensions
Triples <- melt(v)

# make DSM object
#Triples <- subset(DSM_VerbNounTriples_BNC, mode == "written")
VObj <- dsm(target=Triples$Var2, feature=Triples$Var1, score=Triples$value,
              raw.freq=TRUE, sort=TRUE)
dim(VObj)
VObj <- subset(VObj, nnzero >= 3, nnzero >= 3, recursive=TRUE)
dim(VObj)
VObj <- dsm.score(VObj, score="simple-ll", transform="log", normalize=TRUE)
VObj300 <- dsm.projection(VObj, method="svd", n=100)
pair.distances("right", "muslim", VObj300, method="cosine", convert=FALSE)
nearest.neighbours(VObj, "woman", n=15) # defaults to angular distance

eval.similarity.correlation(RG65, VObj300, format="HW")
plot(eval.similarity.correlation(RG65, VObj300, format="HW", details=TRUE))

nn <- nearest.neighbours(VObj300, "muslim", n=15)
nn.terms <- c("muslim", names(nn)) # nn = distances labelled with the neighbour terms
nn.dist <- dist.matrix(VObj300, terms=nn.terms, method="cosine")
nn.dist
library(MASS) # a standard R package that includes two MDS implementations
mds <- isoMDS(nn.dist, p=2)
plot(mds$points, pch=20, col="red")
text(mds$points, labels=nn.terms, pos=3)
