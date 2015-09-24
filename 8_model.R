# 1) Plots
# 2) Heckman
# 3) Panel Models
# 4) Linear & Diagnostics
# 5) Poisson & Negative Binomial

cor(rt$women_composite, rt$mena, use = "complete.obs")
library(usdm)

coeftest(lm(women_composite ~ mena, data = rt ))

rm(list=ls())
setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

library(plyr)
library("MASS")
library("xtable")
library(plm)
library(ggplot2)
library(stargazer) # pretty tables
library(statar) # for missing values
library(sandwich) # for robust standard errors
library(lmtest) # for robust standard errors
library(data.table)  # for missing valus
library(sampleSelection) # for heckman correction
library(car)
library(pscl)
source("interaction_plots.R")
library(mfx)

#######################
#### Prepare Data #####
#######################

rt.orig <- read.csv("Data/country-year/original.csv")
rt.nearest <- read.csv("Data/country-year/nearest.csv")
rt.imputed <- read.csv("Data/country-year/imputed.csv")

# set database
rt <- rt.orig

# remove items without ccode
rt <- rt[!is.na(rt$ccode),]

# set dv
rt$rights <- rt$rights.mean

# if sum, set na to 0
rt$rights[is.na(rt$rights)] <- 0

# composite women's rights
rt$women_composite <- rowMeans(cbind(rt$wopol,rt$wosoc,rt$wecon), na.rm = T)

# Summarize Data
stargazer(rt, type="text")

# make panel data
rt <- pdata.frame(rt, c("ccode","year"))

# subset with an observation
rt.1 <- rt[rt$n.binary==1,]
summary(rt.1$muslim)

#############################
#### 1.  Plots and Tests ####
#############################

plot(rt.1$muslim, rt.1$rights, main="Scatterplot Example", 
     xlab="Muslim Perc ", ylab="Rights Words", pch=1)

abline(lm(rights~muslim, data = rt.1), col="red") # regression line (y~x) 

scatterplot(rights ~ muslim | wopol, data=rt.1, 
            xlab="Muslim perc", ylab="Rights Words", 
            main="Enhanced Scatter Plot")

ggplot(rt.1, aes(x = muslim, y = rights)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# High Density Scatterplot with Binning
library(hexbin)
x <- rnorm(1000)
y <- rnorm(1000)
bin<-hexbin(rt$muslim, rt$rights, xbins=50) 
plot(bin, main="Hexagonal Binning")

# histograms
hist(rt$n.docs,  breaks = 50)
hist(rt.1$rights,  breaks = 50)

# Wooldridge test for serial correlation
pwartest(n.docs ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

pwartest(n.docs ~lag(n.docs,1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

pwartest(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

pwartest(n.binary  ~lag(n.binary,1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

########################################
#### 2. LOGIT  Models on N. Binary #####
########################################

# MUSLIM MAJORITY
logit1 <- glm(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = binomial(link="logit"))
summary(logit1)
# hubert white standard errors
coeftest(logit1, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 
coeftest(logit1, sandwich)
sand_vcov <- sandwich(logit1)


interaction_plot_binary(logit1, effect="lag(women_composite, 1)", moderator="lag(muslim.maj, 1)", interaction="lag(women_composite, 1):lag(muslim.maj, 1)", factor_labels=c("Not Muslim","Muslim"), xlabel="Muslim majority", ylabel="Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Muslim-majority status on coverage")

# MENA
logit2 <- glm(n.binary ~ lag(count, 1) + lag(women_composite,1)*mena + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial"(link="probit"))
summary(logit2)
coeftest(logit2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_binary(logit2, effect="lag(women_composite, 1)", moderator="mena", interaction="lag(women_composite, 1):mena", factor_labels=c("Not Mena","Mena"), xlabel="Mena", ylabel="Marginal Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Mena status on coverage")

# PERCENT MUSLIM
logit3 <- glm(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial"(link="probit"))
summary(logit3)
coeftest(logit3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_continuous(logit3, effect="lag(women_composite, 1)", moderator="lag(muslim, 1)", interaction="lag(women_composite, 1):lag(muslim, 1)", mean=T, title="Interaction between Women's Rights\nand Muslim Percentage on coverage",xlabel="Percentage Muslim", ylabel="Marginal Effect of Women's Rights on Coverage")


###################################
#### 3. Negbin Model on N.Docs ####
###################################

# MUSLIM MAJORITY
nb1 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(nb1)
coeftest(nb1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_binary(nb1, effect="lag(women_composite, 1)", moderator="lag(muslim.maj, 1)", interaction="lag(women_composite, 1):lag(muslim.maj, 1)", factor_labels=c("Not Muslim Majority","Muslim Majority"), xlabel="", ylabel="Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Muslim-majority status on coverage")

# MENA
nb2 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*mena + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(nb2)
coeftest(nb2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_binary(nb2, effect="lag(women_composite, 1)", moderator="mena", interaction="lag(women_composite, 1):mena", factor_labels=c("Not Mena","Mena"), xlabel="", ylabel="Marginal Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Mena status on coverage")

# MUSLIM PERCENTAGE
nb3 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(nb3)
coeftest(nb3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_continuous(nb3, effect="lag(women_composite, 1)", moderator="lag(muslim, 1)", interaction="lag(women_composite, 1):lag(muslim, 1)", mean=T, title="Interaction between Women's Rights\nand Muslim Percentage on coverage",xlabel="Percentage Muslim", ylabel="Marginal Effect of Women's Rights on Coverage")

# tobit for good measure

tobit <- tobit(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(tobit)

interaction_plot_binary(tobit, effect="lag(women_composite, 1)", moderator="lag(muslim.maj, 1)", interaction="lag(women_composite, 1):lag(muslim.maj, 1)", factor_labels=c("Not Muslim Majority","Muslim Majority"), xlabel="", ylabel="Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Muslim-majority status on coverage")



###########################
#### 4. Heckman Models ####
###########################

## 2 - step SELECTION MODELS
heckit <- heckit(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),
                 rights ~ lag(women_composite,1) + lag(muslim.maj,1) + lag(physint,1) + lag(polity2, 1),
                 rt )
summary(heckit)
coeftest(heckit$lm, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
coeftest(heckit$lm,, vcov=function(x) NeweyWest(x, lag =1))

# OLS for good measure

lm <- lm(rights ~ lag(women_composite,1) + lag(muslim.maj,1) + lag(physint,1), data = rt)
summary(lm)

# fractional logit
glm <- glm(rights.mean ~ lag(women_composite,1) + lag(muslim.maj,1) + lag(physint,1), data = rt, family=binomial(link="logit"))
summary(glm)
coeftest(glm, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
coeftest(glm, sandwich)


#############################
#### 5. Robustness Tests ####
#############################

## LOGIT ON N.BINARY

# women's political rights
logit.r1.1 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wopol,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # muslim majority
coeftest(logit.r1.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r1.2 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wopol,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # mena
coeftest(logit.r1.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r1.3 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wopol,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # muslim percentage
coeftest(logit.r1.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# women's social rights
logit.r2.1 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wosoc,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # muslim majority
coeftest(logit.r2.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r2.2 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wosoc,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # mena
coeftest(logit.r2.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r2.3 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wosoc,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # muslim percentage
coeftest(logit.r2.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# women's economic rights
logit.r3.1 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wecon,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # muslim majority
coeftest(logit.r3.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r3.2 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wecon,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # mena
coeftest(logit.r3.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r3.3 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wecon,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # muslim percentage
coeftest(logit.r3.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 4: remove Israel from MENA
rt.no.israel <- rt
rt.no.israel$mena[rt.no.israel$ccode == 666] <- 0
logit.r4 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(women_composite,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.no.israel, family = "binomial")
coeftest(logit.r4, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 5: removing lagged DV
logit.r5.1 <- glm(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # muslim majority
coeftest(logit.r5.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r5.2 <- glm(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # mena
coeftest(logit.r5.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r5.3 <- glm(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial") # muslim percentage
coeftest(logit.r5.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 6: missing data = nearest value
rt.nearest$rights <- rt.nearest$rights.mean
rt.nearest$rights[is.na(rt.nearest$rights)] <- 0
rt.nearest$women_composite <- rowMeans(cbind(rt.nearest$wopol,rt.nearest$wosoc,rt.nearest$wecon), na.rm = T)
rt.nearest <- pdata.frame(rt.nearest, c("ccode","year"))

logit.r6.1 <- glm(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.nearest, family = "binomial") # muslim majority
coeftest(logit.r6.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r6.2 <- glm(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.nearest, family = "binomial") # mena
coeftest(logit.r6.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r6.3 <- glm(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.nearest, family = "binomial") # muslim percentavge
coeftest(logit.r6.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 7: Missing Data - Multiple Imputation
rt.imputed$rights <- rt.imputed$rights.mean
rt.imputed$rights[is.na(rt.imputed$rights)] <- 0
rt.imputed$women_composite <- rowMeans(cbind(rt.imputed$wopol,rt.imputed$wosoc,rt.imputed$wecon), na.rm = T)
rt.imputed <- pdata.frame(rt.imputed, c("ccode","year"))

logit.r7.1 <- glm(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.imputed, family = "binomial") # muslim majority
coeftest(logit.r7.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r7.2 <- glm(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.imputed, family = "binomial") # mena
coeftest(logit.r7.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

logit.r7.3 <- glm(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.imputed, family = "binomial") # muslim percentavge
coeftest(logit.r7.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))


## NEGATIVE BINOMIAL on N.Docs ##

# 1: women's political rights
nb.r1.1 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wopol,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # muslim majority
coeftest(nb.r1.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r1.2 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wopol,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # mena
coeftest(nb.r1.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r1.3 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wopol,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # muslim percentage
coeftest(nb.r1.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 2: women's social rights
nb.r2.1 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wosoc,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # muslim majority
coeftest(nb.r2.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r2.2 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wosoc,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # mena
coeftest(nb.r2.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r2.3 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wosoc,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # muslim percentage
coeftest(nb.r2.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 3: women's economic rights
nb.r3.1 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wecon,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # muslim majority
coeftest(nb.r3.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r3.2 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wecon,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # mena
coeftest(nb.r3.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r3.3 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wecon,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # muslim percentage
coeftest(nb.r3.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 4: remove Israel from MENA
rt.no.israel <- rt
rt.no.israel$mena[rt.no.israel$ccode == 666] <- 0
nb.r4 <- glm.nb(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(women_composite,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.no.israel)
coeftest(nb.r4, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 5: removing lagged DV
nb.r5.1 <- glm.nb(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # muslim majority
coeftest(nb.r5.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r5.2 <- glm.nb(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # mena
coeftest(nb.r5.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r5.3 <- glm.nb(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt) # muslim percentage
coeftest(nb.r5.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

## 6: missing data = nearest value
rt.nearest$rights <- rt.nearest$rights.mean
rt.nearest$rights[is.na(rt.nearest$rights)] <- 0
rt.nearest$women_composite <- rowMeans(cbind(rt.nearest$wopol,rt.nearest$wosoc,rt.nearest$wecon), na.rm = T)
rt.nearest <- pdata.frame(rt.nearest, c("ccode","year"))

nb.r6.1 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.nearest) # muslim majority
coeftest(nb.r6.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r6.2 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.nearest) # mena
coeftest(nb.r6.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r6.3 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.nearest) # muslim percentavge
coeftest(nb.r6.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# 7: Missing Data - Multiple Imputation
rt.imputed$rights <- rt.imputed$rights.mean
rt.imputed$rights[is.na(rt.imputed$rights)] <- 0
rt.imputed$women_composite <- rowMeans(cbind(rt.imputed$wopol,rt.imputed$wosoc,rt.imputed$wecon), na.rm = T)
rt.imputed <- pdata.frame(rt.imputed, c("ccode","year"))

nb.r7.1 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.imputed) # muslim majority
coeftest(nb.r7.1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r7.2 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(mena,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.imputed) # mena
coeftest(nb.r7.2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

nb.r7.3 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt.imputed) # muslim percentavge
coeftest(nb.r7.3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

########################
#### 6. PLM Models #####
########################

# test fixed v. random, etc
pm.f <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model="within",index = c("ccode","year"))
summary(pm.f)
pm.r <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model="random",index = c("ccode","year"))
summary(pm.r)
pm.p <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model="pooling",index = c("ccode","year"))
summary(pm.p)

pcdtest(pm.f, test = c("lm"))
plmtest(pm.p, type=c("bp"))
phtest(pm.f, pm.r)
pbgtest(pm.f)

# plm - 1
pm1 <- plm(rights ~ n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt,model="random",index = c("ccode","year"))
summary(pm1)
coeftest(pm1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se1 = coeftest(pm1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

# plm - 2
pm2 <- plm(rights ~ lag(wosoc,1)+lag(mena,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model = "pooling",index = c("ccode","year"))
summary(pm2)
coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se2 = coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

# plm - 3
pm3 <- plm(rights ~ lag(wecon,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model = "pooling",index = c("ccode","year"))
summary(pm3)
coeftest(pm3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se3 = coeftest(pm3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

stargazer(pm1,pm2,pm3, type = "html", se = list(se1,se2,se3), notes="Robust standard errors clustered on country appear in parentheses.", omit.stat = c("rsq","adj.rsq","f"),  dep.var.labels = "Proportion of Coverage Devoted to Women's Rights", covariate.labels=c("Women's Political Rights","Women's Social Rights","Women's Economic Rights","Muslim","Democracy","Physical Integrity Index","GDP Per Capita (Log)","Population (Log)","Instability","MENA"), out="Results/regressions/nearest.html")


#########################################
#### 2. Linear Models + Diagnostics #####
#########################################
rt <- as.data.frame(rt)

# fit linear model
fit <- lm(n.docs ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

# NeweyWest standard errors
NeweyWest(fit, lag = 1, diagnostics = 1)
coeftest(fit, vcov=function(x) NeweyWest(x, lag =1))

# Panel corrected standard errors
psce <-pcse(fit, groupN = as.numeric(rt$ccode), groupT = as.numeric(rt$year), pairwise=TRUE)

# outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
avPlots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rt.1)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)

# Points size reflecting Cook's distance
myfortdata = fortify(lm)
ggplot(data = myfortdata, aes(x = .fitted, y = .resid, size = .cooksd)) +
  geom_hline(yintercept = 0, colour = "firebrick3") +
  geom_point() +
  scale_size_area("Cook’s distance")

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
names(rt)
vars <- rt[,c(8,11,14,15,19,21,25)]
vars$gdp.pc.un <- ln(vars$gdp.pc.un )
vars$pop.wdi <- ln(vars$pop.wdi )
names(vars)
vif(vars) # variance inflation factors 
v1 <- vifcor(vars, th=0.9)
v2 <- vifstep(vars, th=10)
v1
v2
names(rt)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)

