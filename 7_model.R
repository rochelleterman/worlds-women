rm(list=ls())
setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/worlds-women")

library(plyr)
library("MASS")
library("xtable")
library(plm)
library(ggplot2)
library(reshape2)
library(stargazer) # pretty tables
library(statar) # for missing values
library(sandwich) # for robust standard errors
library(lmtest) # for robust standard errors
library(Amelia) # for missing valus
library(data.table)  # for missing valus
library(sampleSelection) # for heckman correction

# load data

rt.orig <- read.csv("Data/regression-data/regression-rights.csv")
rt.orig$X <- NULL
rt.nearest <- read.csv("Data/regression-data/regression-rights-nearest.csv")
rt.nearest$X <- NULL
rt.imputed <- read.csv("Data/regression-data/regression-rights-imputed.csv")
rt.imputed$X <- NULL

rt <- rt.orig

# Summarize Data
stargazer(rt, type="text")

# make panel data
rt <- pdata.frame(data, c("ccode","year"))

############################
#### 1. Heckman Models #####
############################

rt$mena[rt$ccode == 666] <- 0

## 2 - step SELECTION MODELS
summary( heckit (n.binary ~ lnreportcount + lag(muslim,1) + log(lag(gdp.pc.un,1)) + log(lag(pop.wdi,1)) + lag(polity2,1) + lag(domestic9,1),
                 rights ~ (lag(wopol,1)*lag(muslim,1))+lag(polity2,1)+lag(physint,1),
                 rt) )

pma <- plm(rights ~ mena + (lag(wopol,1)*lag(muslim,1))+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model = "pooling",index = c("ccode","year"))
summary(pma)

###########################
#### 2. Normal Models #####
###########################

# plm - 1
pm1 <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1)+mena,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm1)

coeftest(pm1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se1 = coeftest(pm1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

# plm - 2
pm2 <- plm(rights ~ lag(wosoc,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1)+mena,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm2)

coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se2 = coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

# plm - 3
pm3 <- plm(rights ~ lag(wecon,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1)+mena,data = rt,model = "pooling",index = c("ccode","year"))
summary(pm3)

coeftest(pm3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se3 = coeftest(pm3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

stargazer(pm1,pm2,pm3, type = "html", se = list(se1,se2,se3), notes="Robust standard errors clustered on country appear in parentheses.", omit.stat = c("rsq","adj.rsq","f"),  dep.var.labels = "Proportion of Coverage Devoted to Women's Rights", covariate.labels=c("Women's Political Rights","Women's Social Rights","Women's Economic Rights","Muslim","Democracy","Physical Integrity Index","GDP Per Capita (Log)","Population (Log)","Instability","MENA"), out="Results/regressions/impute.html")

#######################
# 3. Robustness Tests #
#######################

lm1 <- lm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1)+mena,data = rt) 
summary(lm1)

random <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1)+mena,data = rt,model = "random",index = c("ccode","year"))

pool <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1)+mena,data = rt,model = "pooling",index = c("ccode","year"))

fixed <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1)+mena,data = rt,model = "within",index = c("ccode","year"))

phtest(fixed, pool)

coeftest(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC1"))
coeftest(pm1, vcov=function(x) vcovHC(x, cluster="time", type="HC1"))
