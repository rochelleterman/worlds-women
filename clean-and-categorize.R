### This script:
### 1. Loads Raw Data
### 2. Cleans it
### 3. Applies YEAR column
### 4. Applies COUNTRY column
### 5. Applies REGION column
### 6. Exports data

rm(list=ls())
library("foreign")
setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

####################
#### Load Data #####
####################

# load data
nyt <- read.csv('Data/raw-CSVs/raw-nyt.csv')
wp1 <- read.csv('Data/raw-CSVs/raw-wp1.csv')
wp2 <- read.csv('Data/raw-CSVs/raw-wp2.csv')

# subset 

nyt <- subset(nyt,select=c(PUBLICATION,DATE,TITLE,BYLINE,COUNTRY,LENGTH,TYPE,SUBJECT,TEXT))
wp1 <- subset(wp1,select=c(PUBLICATION,DATE,TITLE,BYLINE,COUNTRY,LENGTH,TYPE,SUBJECT,TEXT))
wp2 <- subset(wp2,select=c(PUBLICATION,DATE,TITLE,BYLINE,COUNTRY,LENGTH,TYPE,SUBJECT,TEXT))
women <- rbind(nyt,wp1,wp2)

# get rid of fake records

levels(women$PUBLICATION)
women$PUBLICATION <- as.character(women$PUBLICATION)
nyt.index <- grep("New York Times",women$PUBLICATION,ignore.case=T)
women$PUBLICATION[nyt.index] <- "NYT"
wp.index <- grep("Washington",women$PUBLICATION,ignore.case=T)
women$PUBLICATION[wp.index] <- "WP"
u <- union(nyt.index,wp.index)
women <- women[u,]

# get rid of paid death notices records

paid <- grep("Paid Notice",women$TITLE,ignore.case=T)
women <- women[-paid,]

###############################
########## Year ###############
###############################

women$DATE <- as.character(women$DATE)
women$YEAR <- substr(women$DATE, nchar(women$DATE)-4, nchar(women$DATE))
women$YEAR <- as.integer(women$YEAR)
summary(women$YEAR)

####################################
########## Countries ###############
####################################

# This country code spreadsheet will help me categorize countries + regions
countries <- read.csv("Data/country_codes.csv")
countries$Key <- as.character(countries$Key)
countries$iso3c <- as.character(countries$iso3c)

# initialize columns
women$COUNTRY_MAJOR <- NA # all major countries terms (>85%)
women$COUNTRY_TOP_PERCENT <- NA # just the string with the top percent
women$COUNTRY_FINAL <- NA #the country in COUNTRY_TOP_PERCENT in standardized format
women$COUNTRY_CODE <- NA #the iso3c code representing the country in COUNTRY_FINAL

############################
### MAJOR Country Terms ###
###########################

# Define function to take all major country terms (>85%)
country.major <- function(x){
  country <- as.character(women$COUNTRY[x]) # take one row
  countries <- unlist(strsplit(country, ';\\s*')) # split on ';'
  country.percents <- sub('.*\\((\\d+)%.*', '\\1', countries) # list of just the percentages
  country.percents <- as.list(country.percents) # make a list
  country.percents[(nchar(country.percents) > 2)] <- NA # take out weirdo-s
  country.no.percents <- sub('\\s\\((\\d+)%.', '\\1', countries) # remove parantheses
  country.no.percents <- sub('\\d+', '\\1', country.no.percents) # list just the subject
  country.percents.top <- country.percents > 85 #take only the ones > 85
  country.percents.top <- unlist(country.percents.top) #unlist
  countries.top <- country.no.percents[country.percents.top==TRUE] # take only subjects with perc > 85
  countries.top <- countries.top[!is.na(countries.top)] # remove NAs
  countries.top <- unlist(countries.top)
  return(countries.top)
}

country.major(2)

# apply function to data
row <- nrow(women)
n = seq(1:row)
women$COUNTRY_MAJOR[1:row] <- lapply(n,country.major)

### HOW MANY ARTICLES WITH MORE THAN 1 MAJOR COUNTRY?

# get indexes of articles with more than 1 major country
x <- vector()
for (i in 1:nrow(women)){
  if (length(women$COUNTRY_MAJOR[[i]])>1){
    x <- c(x,i)
  }
}
more <- women[x,] 
rownames(more)

# turn major countries into a data frame
country.list <- more$COUNTRY_MAJOR
x <- matrix(nrow=5295,ncol=25)
for (i in 1:length(country.list)){
  temp.list <- country.list[[i]]
  for (y in 1:length(temp.list)){
    x[i,y] <- temp.list[y]
  }
}
x <- data.frame(x)
rownames(x) <- rownames(more)

# get rid of regions
x[x=="ASIA"] <- NA
x[x=="NORTH AMERICA"] <- NA
x[x=="AFRICA"] <- NA
x[x=="EUROPE"] <- NA
x[x=="SOUTH AMERICA"] <- NA
x[x=="CENTRAL EUROPE"] <- NA
x[x=="SOUTHERN ASIA"] <- NA
x[x=="MIDDLE EAST"] <- NA
x[x=="LATIN AMERICA"] <- NA

# shift to the left
x[]<-t(apply(x,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])}))
more <- x[!is.na(x$X2),]
more.index <- rownames(more)
more.women <- women[more.index,]

# percentage of articles with more than 1 major country
length(more.index)/nrow(women) #0.0907867

#########################
### TOP Country Terms ###
########################

# Define function to take top-percentage country
country.percentages <- function(x){
  geo <- as.character(women$COUNTRY[x]) # take one row
  countries <- unlist(strsplit(geo, ';\\s*')) # split on ';'
  country.percents <- sub('.*\\((\\d+)%.*', '\\1', countries) # list of just the percentages
  country.percents <- as.list(country.percents) # make a list
  country.percents[(nchar(country.percents) > 2)] <- NULL # take out weirdo-s
  country.percents[country.percents < 85] <- NULL
  country.percents <- unlist(country.percents)
  country <- grep(max(country.percents), countries, value=T)[1]
  return(country)
}

# apply function to data
row <- nrow(women)
n = seq(1:row)
women$COUNTRY_TOP_PERCENT[1:row] <- lapply(n,country.percentages)
women$COUNTRY_TOP_PERCENT <- as.character(women$COUNTRY_TOP_PERCENT)

####################################
##### Standardize Country Names ####
####################################

# Define function for turning value in COUNTRY_TOP_PERCENT into standardized format and putting it into COUNTRY_PERCENT_ST
country.standard <- function(x,y,z){
  country.index <- (grepl(x, z$COUNTRY_TOP_PERCENT,ignore.case=T))
  z$COUNTRY_FINAL[country.index] <- as.character(y)
  return(z$COUNTRY_FINAL)
}

# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in 1:n){
  women$COUNTRY_FINAL <- country.standard(countries$Key[i],countries$Value[i],women)
}

sum(is.na(women$COUNTRY_FINAL)) # 17134

#####################
### Country Codes ###
#####################

# Define function to get country code (ccode) from COUNTRY_FINAL and put it in column COUNTRY_CODE

country.code <- function(x,y,z){
  country.index <- (grepl(x, z$COUNTRY_FINAL,ignore.case=T))
  z$COUNTRY_CODE[country.index] <- as.character(y)
  return(z$COUNTRY_CODE)
}

# Apply function to all countries in the key-value list
n <- nrow(countries)
 for(i in 1:n){
  women$COUNTRY_CODE <- country.code(countries$Key[i],countries$iso3c[i],women)
}

# Fix problematic codes

unique(women$COUNTRY_FINAL[is.na(women$COUNTRY_CODE)])

women$COUNTRY_CODE[women$COUNTRY_FINAL=="DRC"] <- "COD"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Macedonia" & women$YEAR < 1992] <- "MKD"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Macedonia" & women$YEAR > 1991] <- "MAC"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Yugoslavia" & women$YEAR < 2003] <- "MKD"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Yugoslavia" & women$YEAR > 2002] <- "YUG"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Yugoslavia" & women$YEAR > 2005] <- "SRB"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Serbia" & women$YEAR < 2003] <- "MKD"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Serbia" & women$YEAR > 2002] <- "YUG"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Serbia" & women$YEAR > 2005] <- "SRB"
women$COUNTRY_CODE[women$COUNTRY_FINAL=="Kosovo" & women$YEAR > 2007] <- "MNE"


#####################
### Apply Regions ###
#####################

### This method is from my country_codes.csv file. It only applies a region from key, i.e. "FINAL COUNTRY".
names(countries)
women$REGION <- NA
for (i in 1:n){
  country <- as.character(countries$Key[i])
  women$REGION[women$COUNTRY_FINAL==country]<-as.character(countries$Region[i])
}

unique(women$COUNTRY_FINAL[is.na(women$REGION)])

# Fixing problematic Regions
women$REGION[women$COUNTRY_FINAL=="lebanon"] <- "MENA"
women$REGION[women$COUNTRY_FINAL=="Myanmar (Burma)"] <- "Asia"

#######################################
###### Subsetting and Writing #########
#######################################

# Subsetting Data
women.foreign <- subset(women,!COUNTRY_CODE=="USA")
women.foreign <- subset(women.foreign,!is.na(COUNTRY_CODE))

# with only 1 major country
women.foreign.1 <- women.foreign[ !(rownames(women.foreign) %in% more.index), ] 
women.foreign.more <- women.foreign[ (rownames(women.foreign) %in% more.index), ] 
more.index <- which(rownames(women.foreign) %in% more.index)

# Prepare to write
women.foreign$REGION <- as.factor(women.foreign$REGION)
women.foreign$COUNTRY_FINAL <- as.factor(women.foreign$COUNTRY_FINAL)
women.foreign$COUNTRY_MAJOR <- as.character(women.foreign$COUNTRY_MAJOR)
women$COUNTRY_MAJOR <- as.character(women$COUNTRY_MAJOR)
women.foreign.1$COUNTRY_MAJOR <- as.character(women.foreign.1$COUNTRY_MAJOR)

#### WRITE FILES #####

write.csv(women,"Data/women-all.csv") # write all

con<-file('Data/women-foreign.csv',encoding="utf8")
write.csv(women.foreign,file=con,fileEncoding="UTF8")

con<-file('Data/women-foreign-1.csv',encoding="utf8")
write.csv(women.foreign.1,file=con,fileEncoding="UTF8")

