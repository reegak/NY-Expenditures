library(MASS)  # for stepAIC and robust regression
library(leaps) # for regsubsets
library(car)   # for qqPlot
#install.packages("glmnet")
#install.packages("gam")
library(glmnet) # for lasso
library(gam)    # for generalized additive models

library(corrplot)
library(mgcv)
library(vioplot)

rm(list=ls(all=TRUE)) # remove all previous objects from memory
options(warn=-1)  # forces R to ignore all warning messages

# Read in data, remove missing data
ny<-read.table("cs73.dat",header=T); dim(ny)  #  916  11
ny2<-na.omit(ny); dim(ny2) # 914  11
attach(ny2)

## review data set: expen is response
names(ny2)
head(ny2, n=5)


## Regression model on full data set (i.e., do not split data)
## From eda_ClassVersion.R
# wealth: log-transform
# lpop: cubic polynomial
# ldens: cubic polynomial
# pint: cubic polynomial, no log-transform
# income: quadratic, no log-transform
# lgrowth: linear

## Create transformations that may be needed
lexpen<-log(expen)
lwealth<-log(wealth)
lpop<-log(pop)
ldens<-log(dens)
lincome<-log(income)
lpint = log(pint)
pint2<-pint**2
pint3<-pint**3
lpop2<-lpop**2
lpop3<-lpop**3
ldens2<-ldens**2
ldens3<-ldens**3
lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))


# Consider interactions: Sections 3.3.2, 3.6.4 of ISLR
# Use the data subset on which there are linear relationships in all variables.
set2<-(lpop>8.3 & ldens>4.5)
ny2vars = data.frame(lexpen,lwealth,lpop,lpint,lincome,lgrowr)
fit2 = lm(lexpen~.*., data = ny2vars, subset=set2)
fitfull = stepAIC(fit2)
summary(fitfull)
extractAIC(fitfull)  # want to minimize AIC
# remove non-significant interaction lwealth:lpop
fitbest = lm(lexpen ~ lwealth + lpop + lpint + lincome + lgrowr + 
               lwealth:lpint + lwealth:lgrowr + lpop:lincome + 
               lpop:lgrowr, data = ny2vars, subset = set2)
summary(fitbest)
extractAIC(fitbest)
anova(fitfull, fitbest)  # p = 0.16
# perhaps remove non-significant interactions:
#  lpop:lgrowr and then lpop:lincome
fitbest = lm(lexpen ~ lwealth + lpop + lpint  + lgrowr + 
               lwealth:lpint + lwealth:lgrowr , data = ny2vars, subset = set2)
summary(fitbest)
extractAIC(fitbest)


# Model diagnostics
# Residual plot
plot(predict(fitbest), rstudent(fitbest), ylab="Studentized Residuals", xlab="Predicted")
# Normality of Residuals
sresid <- studres(fitbest)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
# Q-Q plot for studentized resid
qqPlot(fitbest, main="QQ Plot", ylab="Studentized Residuals")
# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fitbest$coefficients)-2))
plot(fitbest, which=4, cook.levels=cutoff) # influence Plot

# leaps not smart enough to leave main effects in when interaction in model
ff=regsubsets(lexpen~.*., data=ny2vars, subset=set2)
summary(ff)


# Categorical variables in regression
# For illustration purposes, we will use the whole data set (no subset split)
wealthcat = 1*(wealth<40000)+2*(wealth>40000)*(wealth<80000)+3*(wealth>80000)*(wealth<100000)+4*(wealth>100000)*(wealth<200000)+5*(wealth>200000)
# "Dummy variable" coding or "treatment" coding: 
# contrast to a reference level or baseline 
wealthcat=factor(wealthcat) # tell R this variable is categorical
# tabulate the categorical variable
table(wealthcat) 
# Parallel boxplots with box width a function of sample size
plot(wealthcat, expen, col="cyan", varwidth=T,
     ylab="Expenditures",xlab="Wealth Categories")
# Average log-expenditure by wealth category
tapply(lexpen, wealthcat, mean)
fitc = lm(formula = lexpen ~ wealthcat, data=ny2vars)
# Treatment coding: 
#  notice that the intercept is the (cell) mean log-expenditure for wealth category 1!
summary(fitc) 
# contrast matrix for 5-level categorical variable
contr.treatment(5)
# Can fit continuous variables as well
fitc = lm(formula = lexpen ~ wealthcat + lpop + lpint + lincome, data=ny2vars, subset=set2)
summary(fitc)


