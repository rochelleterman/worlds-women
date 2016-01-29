# 1) Prepare
# 2) Plots
# 3) PLM
# 4) Linear Models Diagnostics

rm(list=ls())
setwd("~/Dropbox/berkeley/Git-Repos/worlds-women")

require(plyr)
require("MASS")
require("xtable")
require(plm)
require(ggplot2)
require(stargazer) # pretty tables
require(statar) # for missing values
require(sandwich) # for robust standard errors
require(lmtest) # for robust standard errors
require(data.table)  # for missing valus
require(sampleSelection) # for heckman correction
require(car)
require(pscl)
require(mfx)

##########################
#### 1. Prepare Data #####
##########################

rt.orig <- read.csv("Data/country-year/original.csv")
# rt.imputed <- read.csv("Data/country-year/imputed.csv")

# set database
rt <- rt.orig

# Deal with Palestine (without ccode)
rt$ccode[is.na(rt$ccode)] <- 999

# composite women's rights
rt$women_composite <- rowMeans(cbind(rt$wopol,rt$wosoc,rt$wecon), na.rm = T)

# Summarize Data
stargazer(rt, type="text")

# subset with an observation
rt.1 <- rt[rt$n.binary==1,]
summary(rt.1$muslim)

# israel
rt$mena[rt$ccode == 666] <- 0

# n.obs
nrow(rt[rt$year>1979,]) # 6292

# make panel data
rt <- pdata.frame(rt, c("ccode","year"))

###################
#### 2. Plots #####
###################

# rights
plot(rt.1$muslim, rt.1$rights, main="Scatterplot Example", 
     xlab="Muslim Perc ", ylab="Rights Words", pch=1)
abline(lm(rights~muslim, data = rt.1), col="red") # regression line (y~x) 

# GBV
#plot(rt.1$muslim, rt.1$gbv, main="Scatterplot Example", 
     #xlab="Muslim Perc ", ylab="Rights Words", pch=1)
#abline(lm(rights~muslim, data = rt.1), col="red") # regression line (y~x) 

scatterplot(rights ~ muslim | wopol, data=rt.1, 
            xlab="Muslim perc", ylab="Rights Words", 
            main="Enhanced Scatter Plot")

jpeg("Results/regressions/scatterplot.jpeg",width=700,height=400,type="quartz")
ggplot(rt.1, aes(x = muslim, y = rights)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
dev.off()

# High Density Scatterplot with Binning
require(hexbin)
x <- rnorm(1000)
y <- rnorm(1000)
bin<-hexbin(rt$muslim, rt$rights, xbins=50) 
plot(bin, main="Hexagonal Binning")

# histograms
hist(rt$n.docs[rt$n.binary>0],  breaks = 100)
hist(rt.1$rights,  breaks = 100)

########################
#### 3. PLM Models #####
########################

# test fixed v. random, etc
pm.f <- plm(rights ~ lag(women_composite,1)+lag(muslim,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model="within",index = c("ccode","year"))
summary(pm.f)
pm.r <- plm(rights ~ lag(women_composite,1)+lag(muslim,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model="random",index = c("ccode","year"))
summary(pm.r)
pm.p <- plm(rights ~ lag(women_composite,1)+lag(muslim,1)+lag(physint,1)+log(lag(gdp.pc.un,1))+log(lag(pop.wdi,1))+lag(domestic9,1),data = rt,model="pooling",index = c("ccode","year"))
summary(pm.p)

pcdtest(pm.f, test = c("lm"))
plmtest(pm.p, type=c("bp"))
phtest(pm.f, pm.r)
pbgtest(pm.f)

#########################################
#### 4. Linear Models + Diagnostics #####
#########################################

rt <- as.data.frame(rt)

# fit linear model
fit <- lm(n.docs ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

# NeweyWest standard errors
coeftest(fit, vcov=function(x) NeweyWest(x, lag =1))

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
# Breuch Pagan test
bptest(fit)
coeftest(fit,vcov=hccm(fit))

###########################################
# Wooldridge test for serial correlation #
##########################################

# On N.Docs
pwartest(n.docs ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

# Adding laggged DV Help?
pwartest(n.docs ~ lag(n.docs,1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

# on N.Binary
pwartest(n.binary ~ lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

# Adding lagged DV Help?
pwartest(n.binary  ~lag(n.binary,1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)

######################
# Collinearity tests #
######################

# Correlation matrix
require(Hmisc)
names(rt)
vars <- rt[,c(9,11,15,16,20,22,26)]
rcorr(as.matrix(vars), type="pearson")

# Evaluate Collinearity with car
fit <- lm(n.docs ~ lag(n.docs,1) + lag(count, 1) + lag(women_composite,1)*lag(muslim.maj,1) + lag(polity2,1) + lag(domestic9,1) + log(lag(pop.wdi,1)) + log(lag(gdp.pc.un,1)),data = rt)
vif(fit)
sqrt(vif(fit)) > 2

# Evluate collinearity with USDM
require(usdm)
vars$gdp.pc.un <- log(vars$gdp.pc.un )
vars$pop.wdi <- log(vars$pop.wdi )
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

