#  Prepping.
rm(list=ls())
setwd("~/Dropbox/berkeley/Git-Repos/worlds-women")
require("matrixStats")
require(RTextTools)
require(tm)
require(rJava) # needed for stemming function 
require(SnowballC) # also needed for stemming function
require(plyr)
require(ggplot2)

# Load Data
meta.topics <- read.csv("Data/topic-proportions/meta-topics.csv")

###############################
#### Alt. DV. Classifier ######
###############################

## how many docs have the words: right, equal, sexist, sexism
toMatch <- c("right", "equal", "discriminat")

# find matches
matches <- unique(grep(paste(toMatch,collapse="|"), 
                        meta.topics$text.no.noun, ignore.case = T))
length(matches) / nrow(meta.topics) # 0.5795481

# add variable to meta-topics
meta.topics$alt.dv <- 0
meta.topics$alt.dv[matches] <- 1
names(meta.topics)

# country-year format
subset <- meta.topics[,c("iso3c", "year", "alt.dv")]
alt.country.year <- ddply(.data=subset, .variables=.(iso3c,year), numcolwise(sum,na.rm = TRUE))
alt.country.year$n.docs <- (ddply(.data=meta.topics,.variables=.(iso3c,year), .fun=nrow))$V1
alt.country.year$alt.dv.perc <- alt.country.year$alt.dv / alt.country.year$n.docs
write.csv(alt.country.year,"Data/topic-proportions/alt-dv-country-year.csv", row.names = F)

# summarize
summary(alt.country.year$alt.dv.perc)
hist(alt.country.year$alt.dv.perc)
ggplot(alt.country.year, aes(x = iso3c, y = alt.dv.perc)) + geom_bar(stat = "identity")
