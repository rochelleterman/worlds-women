### This script:
### 1. Loads Raw Data
### 2. Cleans it
### 3. Applies YEAR column
### 4. Applies COUNTRY column
### 5. Applies REGION column
### 6. Exports data

rm(list=ls())
library("foreign")
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

# load data
nyt <- read.csv('data/raw-nyt.csv')
wp1 <- read.csv('data/raw-wp1.csv')
wp2 <- read.csv('data/raw-wp2.csv')

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
countries <- read.csv("country_codes.csv")
countries$Key <- as.character(countries$Key)
countries$iso3c <- as.character(countries$iso3c)

# get rid of OMAN because it will match "women"

countries <- countries[!countries$Value=="Oman",]

##########################
### Countries By TITLE ###
##########################

# initialize columns
#women$COUNTRY_TITLE <- NA
#women$COUNTRY_TITLE <- as.character(women$COUNTRY_TITLE)

# Define function for taking country in title
#country.title <- function(x,y,z){
#  country.index <- (grepl(x, z$TITLE,ignore.case=T))
#  z$COUNTRY_TITLE[country.index] <-as.character(y)
#  return(z$COUNTRY_TITLE)
#}

## Apply function to all countries in the key-value list
#n <- nrow(countries)
#for(i in 1:n){
#  women$COUNTRY_TITLE <- country.title(countries$Key[i],countries$Value[i],women)
#}

#sum(is.na(women$COUNTRY_TITLE)) # 33222

############################################
### Countries by LexisNexis COUNTRY term ###
############################################

# initialize columns
women$COUNTRY_TOP_PERCENT <- NA # This column just takes the string with the top percent
women$COUNTRY_PERCENT_ST <- NA # This column will display the country in COUNTRY_TOP_PERCENT in standardized format

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

# Define function for turning value in COUNTRY_TOP_PERCENT into standardized format and putting it into COUNTRY_PERCENT_ST
country.percent <- function(x,y,z){
  country.index <- (grepl(x, z$COUNTRY_TOP_PERCENT,ignore.case=T))
  z$COUNTRY_FINAL[country.index] <- as.character(y)
  return(z$COUNTRY_FINAL)
}

# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in 1:n){
  women$COUNTRY_FINAL <- country.percent(countries$Key[i],countries$Value[i],women)
}

sum(is.na(women$COUNTRY_FINAL)) # 17663

#######################
### Final Countries ###
#######################

# Takes COUNTRY_TITLE as priority, and then COUNTRY_PERCENT_ST
#women$COUNTRY_FINAL <- NA
#women$COUNTRY_FINAL <- women$COUNTRY_TITLE
#na.index <- which(is.na(women$COUNTRY_FINAL))
#women$COUNTRY_FINAL[na.index] <- women$COUNTRY_PERCENT_ST[na.index]

#nrow(women[women$COUNTRY_FINAL=="United States of America",]) #30516

#####################
### Country Codes ###
#####################

women$COUNTRY_CODE <- NA

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
###### Subsetting and WRiting #########
#######################################

### Subsetting Data

women.foreign <- women[!women$COUNTRY_CODE=="USA",]
women.foreign <- women.foreign[!is.na(women.foreign$COUNTRY_CODE),]

women.foreign$REGION <- as.factor(women.foreign$REGION)
women.foreign$COUNTRY_FINAL <- as.factor(women.foreign$COUNTRY_FINAL)

#### WRITE FILES #####

write.csv(women,"Data/women-all.csv") # write all

con<-file('Data/women-foreign.csv',encoding="utf8")
write.csv(women.foreign,file=con,fileEncoding="UTF8")


