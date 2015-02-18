### This script will load, clean and country code articles about women.

rm(list=ls())

library("foreign")

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/women")

women <- read.csv('data/women-processed.csv')
names(women)
##
# index <- which(!grepl(("[0-9]"),women$DATE))

# Remove duplicates
women <- women[!duplicated(women$TITLE),]

# Add new column for year
women$DATE <- as.character(women$DATE)
women$YEAR <- substr(women$DATE, nchar(women$DATE)-4, nchar(women$DATE))
women$YEAR <- as.integer(women$YEAR)
summary(women$YEAR)

names(women)
women$GEOGRAPHIC <- NULL

####################################
########## Countries ###############
####################################

# This country code spreadsheet will help me categorize countries + regions
countries <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/country_codes.csv")
countries$Key <- as.character(countries$Key)
countries$iso3c <- as.character(countries$iso3c)

# get rid of OMAN

countries <- countries[!countries$Value=="Oman",]

##########################
### Countries By TITLE ###
##########################

# initialize columns
women$COUNTRY_TITLE <- NA
women$COUNTRY_TITLE <- as.character(women$COUNTRY_TITLE)

# Define function for taking country in title
country.title <- function(x,y,z){
  country.index <- (grepl(x, z$TITLE,ignore.case=T))
  z$COUNTRY_TITLE[country.index] <-as.character(y)
  return(z$COUNTRY_TITLE)
}

# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in 1:n){
  women$COUNTRY_TITLE <- country.title(countries$Key[i],countries$Value[i],women)
}

sum(is.na(women$COUNTRY_TITLE)) # 18462

###############################################
### Countries by LexisNexis GEOGRAPHIC term ###
###############################################

# initialize columns
women$COUNTRY_TOP_PERCENT <- NA # This column just takes the string with the top percent
women$COUNTRY_PERCENT_ST <- NA # This column will display the country in COUNTRY_TOP_PERCENT in standardized format

# Define function to take top-percentage country
country.percentages <- function(x){
  geo <- as.character(women$COUNTRY[x])
  countries <- unlist(strsplit(geo, ';\\s*'))
  country.percents <- sub('.*\\((\\d+)%.*', '\\1', countries)
  country.percents <- as.list(country.percents)
  country.percents[(nchar(country.percents) > 2)] <- NULL
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
  z$COUNTRY_PERCENT_ST[country.index] <- as.character(y)
  return(z$COUNTRY_PERCENT_ST)
}

# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in seq(1,n)){
  women$COUNTRY_PERCENT_ST <- country.percent(countries$Key[i],countries$Value[i],women)
}

sum(is.na(women$COUNTRY_PERCENT_ST)) # 4576

#######################
### Final Countries ###
#######################

# Takes COUNTRY_TITLE as priority, and then COUNTRY_PERCENT_ST
women$COUNTRY_FINAL <- NA
women$COUNTRY_FINAL <- women$COUNTRY_TITLE
na.index <- which(is.na(women$COUNTRY_FINAL))
women$COUNTRY_FINAL[na.index] <- women$COUNTRY_PERCENT_ST[na.index]

nrow(women[women$COUNTRY_FINAL=="United States of America",]) #16728

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

### This method is from my country_codes.csv file. It only applies a region from ISO3c code, i.e. "COUNTRY_CODE".

women$REGION <- NA
for (i in 1:n){
  country <- as.character(countries$iso3c[i])
  women$REGION[women$COUNTRY_CODE==country]<-as.character(countries$Region[i])
}

unique(women$COUNTRY_FINAL[is.na(women$REGION)])

# Fixing problematic Regions
women$REGION[women$COUNTRY_CODE=="SRB"] <- "EECA"
women$REGION[women$COUNTRY_CODE=="MAC"] <- "EECA"
women$REGION[women$COUNTRY_CODE=="YUG"] <- "EECA"


## This method is from my original method. It applies regions directly from COUNTRY_FINAL, including cases with no specific country, i.e. "Balkans"

# Defining Regions

africa <- c("burundi","comoros","dijibouti","eritrea","ethiopia","kenya","madagascar","kenya","malawi","mauritius","mayotte","mozambique","reunion","rwanda","seqychelles","somalia","south sudan", "uganda","tanzania","united republic of tanzania","zambia","zimbabwe","angola","cameroon","central african republic","chad","congo","democratic republic of congo","equatorial guinea","gabon","sao tome and principe","botswana","lesotho","namibia","south africa","swaziland","benin","burkina faso","cabo verde","cote d'ivoire","gambia","ghana","guinea","gunea-bissau","liberia","mali","mauritania","niger","nigeria","saint helena","senegal","sierra leone","togo","cape verde","guinea-bissau","djibouti","democratic republic of the congo","ivory coast","africa","losotho","ziare")
africa <- paste(africa, collapse='|')

latin.america <- c("Caribbean", "anguilla","antigua and barbuda","aruba","bahamas","barbados","bonaire,saint eustatius and saba","british virgin islands","cuba","curacao","dominica","dominican republic","grenada","guadeloupe","haiti","jamaica","montserrat","puerto rico","saint-barthelemy","saint kitts and nevis","saint lucia","trinidad and tobago","trinidad","turks and calcos islands","united states virgin islands","belize","costa rica","el salvador","guatemala","honduras","mexico","nicaragua","panama","argentina", "bolivia","brazil","chile","colombia","ecuador","falkland islands","malvinas","french guiana","paraguay","peru","suriname","uruguay","venezuela","mexico","uruguay","st. kitts and nevis","seychelles","antigua & barbuda","st. vincent and the grenadines","st. lucia","guyana","uruguay","Samoa","latin america","south america","maldives","ST KITTS-NEVIS","ST LUCIA","ST VINCENT","fiji","central america")
latin.america <- paste(latin.america, collapse='|')

central.asia <- c("kazakhstan","kyrgyzstan","tajikistan","turkmenistan","uzbekistan","belarus","bulgaria","czech republic","hungary","poland","republic of moldova","romania","rumania","russian federation","slovakia","ukraine","albania","andorra","bosnia","croatia","gibralter","greece","holy see","malta","montenegro","san marino","serbia","slovenia","yugoslavia","hungary","czechoslovakia","macedonia","kosovo","moldova","russia","georgia","cyprus","azerbaijan","chechnya","soviet union","kosovo","yugoslavia","croatia","czech","serbia","soviet","estonia","ussr","u.s.s.r.","kazakstan","SLOVAK REPUBLIC")
central.asia <- paste(central.asia, collapse='|')

mena <- c("armenia","bahrain","iraq","jordan","kuwait","lebanon","oman","qatar","saudi arabia","palestine","state of palestine","syria","syrian arab republic","turkey","united arab emirates","yemen","algeria","egypt","libya","morocco","sudan","tunisia","western sahara","iran","afghanistan","yemen arab republic","yemen people's republic","turkish","palestine","palestinian","israel","middle east","mena")
mena <- paste(mena, collapse='|')


west <- c("channel islands","denmark","estonia","finland","iceland","isle of man","jersey","latvia","lithuania","norway","sark","svaldbard and jan mayen islands","sweden","united kingdom","united kingdom of great britain and northern ireland","canada","japan", "australia", "new zealand","ireland","netherlands","belgium","luxembourg","france","monaco","liechtenstein","switzerland","spain","italy","andorra","portugal","germany","german federal republic","german democratic republic","austria-hungary","austria","papal states","irish","Europe","england","west","Nordic Countries", "United States of America")
west <- paste(west, collapse='|')

asia <- c("cambodia","china","japan","sri lanka","vietnam","india","pakistan","bangladesh","malaysia","indonesia","korea","laos","burma","myanmar","philippines","korea","thailand","TIMOR-LESTE","timor","tibet","bhutan","nepal","Singapore","asia","viet nam","samoa","taiwan","kashmir","hong kong","mongolia","Mariana islands","Solomon Islands","MACAO","guam")
asia <- paste(asia,collapse='|')

# Appying regions based on COUNTRY_FINAL

women$REGION2 <- NA

asia.index <- (grepl(asia, women$COUNTRY_FINAL,ignore.case=T))
women$REGION2[asia.index] <- "Asia"

la.index <- (grepl(latin.america, women$COUNTRY_FINAL,ignore.case=T))
women$REGION2[la.index] <- "LA"

mena.index <- (grepl(mena, women$COUNTRY_FINAL,ignore.case=T))
women$REGION2[mena.index] <- "MENA"

africa.index <- (grepl(africa, women$COUNTRY_FINAL,ignore.case=T))
women$REGION2[africa.index] <- "Africa"

ca.index <- (grepl(central.asia, women$COUNTRY_FINAL,ignore.case=T))
women$REGION2[ca.index] <- "EECA"

west.index <- (grepl(west, women$COUNTRY_FINAL,ignore.case=T))
women$REGION2[west.index] <- "West"

women$REGION2 <- as.character(women$REGION)

### basic analysis / summary stats

women.foreign <- women[!women$COUNTRY_CODE=="USA",]
women.foreign <- women.foreign[!is.na(women.foreign$COUNTRY_CODE),]

women.foreign$REGION <- as.factor(women.foreign$REGION)
women.foreign$COUNTRY_FINAL <- as.factor(women.foreign$COUNTRY_FINAL)


barplot(summary(women.foreign$REGION))
summary(women.foreign$COUNTRY_FINAL)

#### WRITE FILES #####
write.csv(women, file="Data/women-processed.csv")

con<-file('women-foreign.csv',encoding="utf8")
write.csv(women.foreign,file=con,fileEncoding="UTF8")
