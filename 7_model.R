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

# load data

rt.orig <- read.csv("Data/regression-data/regression-rights.csv")
rt.orig$X <- NULL
rt.nearest <- read.csv("Data/regression-data/regression-rights-nearest.csv")
rt.nearest$X <- NULL
rt.imputed <- read.csv("Data/regression-data/regression-rights-imputed.csv")
rt.imputed$X <- NULL

rt <- rt.nearest

# Summarize Data
stargazer(rt, type="text")

# make panel data
rt <- pdata.frame(rt, c("ccode","year"))

# subset with an observation
rt.1 <- rt[rt$n.binary==1,]
rt.1$rights <- round(rt.1$rights)
summary(rt.1$rights)
rt.1$muslim[rt.1$muslim < 0] <- 0

# remove Israel
rt$mena[rt$ccode == 666] <- 0

#############################
#### 1.  Plots and Such #####
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

hist(rt.1$rights,  breaks = 100)

#########################################
#### 2. Linear Models + Diagnostics #####
#########################################

# fit model
fit <- lm(rights ~ (lag(wopol,1) + lag(muslim,1))+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt.1)
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

###########################
#### 3. Heckman Models ####
###########################

## 2 - step SELECTION MODELS
summary( heckit (n.binary ~ lnreportcount + relevel(region,5) + log(lag(gdp.pc.un,1)) + log(lag(pop.wdi,1)) + lag(polity2,1) + lag(domestic9,1),
                 rights ~ (lag(wopol,1)*lag(muslim,1))+lag(polity2,1)+lag(physint,1),
                 rt) )

################################
#### 4. Poisson + NB Models ####
################################

# Negative Binomial
m1 <- glm.nb(rights ~ (lag(wopol,1) + lag(muslim,1))+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt.1)
summary(m1)

# GOF
1 - pchisq(summary(m1)$deviance,
           summary(m1)$df.residual
)

# testing negative binomial model assumption
m2 <- glm(rights ~ (lag(wopol,1) + lag(muslim,1))+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1), family = "poisson", data = rt.1)
summary(m2)
pchisq(2 * (logLik(m1) - logLik(m2)), df = 1, lower.tail = FALSE)
plot(m2$residuals)
# GOF
1 - pchisq(summary(m2)$deviance, 
           summary(m2)$df.residual
)
# neg.binomial doesn't seem to be a good fit.

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
pm.f.time <- pm.f <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1) + factor(year),data = rt,model="within",index = c("ccode","year"))
summary(pm.f.time)

pFtest(pm.f.time, pm.f)
plmtest(pm.p, type=c("bp"))
phtest(pm.f, pm.r)
pcdtest(pm.f, test = c("lm"))

# plm - 1
pm1 <- plm(rights ~ lag(wopol,1)+lag(muslim,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model="random",index = c("ccode","year"))
summary(pm1)
coeftest(pm1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se1 = coeftest(pm1, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

# plm - 2
pm2 <- plm(rights ~ lag(wosoc,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model = "pooling",index = c("ccode","year"))
summary(pm2)
coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se2 = coeftest(pm2, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

# plm - 3
pm3 <- plm(rights ~ lag(wecon,1)+lag(muslim,1)+lag(polity2,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model = "pooling",index = c("ccode","year"))
summary(pm3)
coeftest(pm3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
se3 = coeftest(pm3, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))[,2]

stargazer(pm1,pm2,pm3, type = "html", se = list(se1,se2,se3), notes="Robust standard errors clustered on country appear in parentheses.", omit.stat = c("rsq","adj.rsq","f"),  dep.var.labels = "Proportion of Coverage Devoted to Women's Rights", covariate.labels=c("Women's Political Rights","Women's Social Rights","Women's Economic Rights","Muslim","Democracy","Physical Integrity Index","GDP Per Capita (Log)","Population (Log)","Instability","MENA"), out="Results/regressions/impute.html")