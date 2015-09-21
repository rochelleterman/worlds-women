# 1) Plots
# 2) Heckman
# 3) Panel Models
# 4) Linear & Diagnostics
# 5) Poisson & Negative Binomial

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

# remove Israel from MENA
rt$mena[rt$ccode == 666] <- 0

#############################
#### 1.  Plots and Tests #####
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
hist(rt.1$rights,  breaks = 50)


# testing

# overdispersion
mean(rt$n.docs, na.rm = T)
var(rt$n.docs, na.rm = T)

# serial correlation
pwartest(n.docs ~ lag(count, 1) + lag(wopol,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

# NeweyWest
lm <- lm(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(wopol,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(lm)
  
NeweyWest(lm, lag = 1, diagnostics = 1)
coeftest(lm, vcov=function(x) NeweyWest(x, lag =1))

########################################
#### 3. LOGIT  Models on N. Binary #####
########################################

# Logit on n.binary
# Muslim-maj
logit1 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = binomial(link="probit"))
summary(logit1)
coeftest(logit1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_binary(logit1, effect="lag(women_composite, 1)", moderator="lag(muslim.maj, 1)", interaction="lag(women_composite, 1):lag(muslim.maj, 1)", factor_labels=c("Not Muslim","Muslim"), xlabel="Muslim majority", ylabel="Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Muslim-majority status on coverage")

# mena
logit2 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wopol,1)*mena + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial")
summary(logit2)
coeftest(logit2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_binary(logit2, effect="lag(wopol, 1)", moderator="mena", interaction="lag(wopol, 1):mena", factor_labels=c("Not Mena","Mena"), xlabel="Mena", ylabel="Marginal Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Mena status on coverage")

# muslim - continuous
logit3 <- glm(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(wosoc,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt, family = "binomial")
summary(logit3)
coeftest(logit3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_continuous(logit3, effect="lag(wosoc, 1)", moderator="lag(muslim, 1)", interaction="lag(wosoc, 1):lag(muslim, 1)", mean=T, title="Interaction between Women's Rights\nand Muslim Percentage on coverage",xlabel="Percentage Muslim", ylabel="Marginal Effect of Women's Rights on Coverage")

###################################
#### 4. Negbin Model on N.Docs ####
###################################

# Muslim-maj
nb1 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(wopol,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(nb1)
coeftest(nb1, vcov=sandwich)
coeftest(nb1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_binary(nb1, effect="lag(wopol, 1)", moderator="lag(muslim.maj, 1)", interaction="lag(wopol, 1):lag(muslim.maj, 1)", factor_labels=c("Not Muslim","Muslim"), xlabel="Muslim majority", ylabel="Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Muslim-majority status on coverage")

# mena
nb2 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(wopol,1)*mena + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(nb2)
coeftest(nb2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_binary(nb2, effect="lag(wopol, 1)", moderator="mena", interaction="lag(wopol, 1):mena", factor_labels=c("Not Mena","Mena"), xlabel="Mena", ylabel="Marginal Effect of Women's Rights on Coverage", title="Interaction between Women's Rights\nand Mena status on coverage")

# muslim - continuous
nb3 <- glm.nb(n.docs ~ lag(n.docs, 1) + lag(count, 1) + lag(wosoc,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(nb3)
coeftest(nb3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

interaction_plot_continuous(nb3, effect="lag(wosoc, 1)", moderator="lag(muslim, 1)", interaction="lag(wosoc, 1):lag(muslim, 1)", mean=T, title="Interaction between Women's Rights\nand Muslim Percentage on coverage",xlabel="Percentage Muslim", ylabel="Marginal Effect of Women's Rights on Coverage")

###########################
#### 2. Heckman Models ####
###########################

## 2 - step SELECTION MODELS
heckit <- heckit(n.binary ~ lag(n.binary, 1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),
                 rights ~ lag(women_composite,1) + lag(muslim.maj,1) + lag(physint,1) + lag(polity2,1),
                 rt )
summary(heckit)
coeftest(heckit$lm, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

########################
#### 5. PLM Models #####
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
pm1 <- plm(rights ~ count + lag(wopol,1)+lag(mena,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1)),data = rt,model="random",index = c("ccode","year"))
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

# fit model
fit <- lm(n.docs ~ lag(count, 1) + lag(wopol,1)*lag(muslim,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
summary(fit)

crPlots(fit)

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
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)

