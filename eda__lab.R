# Stat 696: EDA Lab
# Experimenting with EDA using the NY Expenditures data set
# Packages required: MASS, corrplot, car, vioplot, sm (for use with vioplot)
# September 2018

## Preliminaries
# Read in data, remove missing data
ny<-read.table("cs73.dat",header=T); dim(ny)  #  916  11
ny2<-na.omit(ny); dim(ny2) # 914  11
attach(ny2)

##############
# Lab Tasks: 
#   Be prepared to discuss at end of class;
#   you will not hand anything in.
#   Use eda_ClassVersion.R as a reference for code.
##############

## EDA on the response, expenditures

## Task 1: Study a descriptive numerical summary of expen
# What do you find?  (Briefly summarize)
summary(expen)
install.packages("vioplot")
library(vioplot)

## Task 2: Study graphical summaries of expen
# a) boxplot (boxplot)
# b) histogram (hist; consider using the 'breaks' option)
# c) violin plot (vioplot; see eda_ClassVersion.R for code)
# d) Q-Q plot (qqnorm; qqline)
# e) Get in the habit of LABELLING AXES and removing titles!
# f) What do you find?  (Briefly summarize)

boxplot(expen)
hist(expen, breaks = 25)
vioplot(expen, names = "People")
qqnorm(expen, xlab = "Quart away from median", ylab = "Expeditures", main = "Exp Stats")
qqline(expen, col = 2)
#it is right skewed.
## Task 3: Consider a response transformation
# a) Perform a Box-Cox transformation on an intercept only model: boxcox(expen~1); 
#    argue for a log-transformation
# b) Study a histogram with density plot overlay of log-expenditures 
#      (see eda_ClassVersion.R for code)
# c) Perform a violin plot and Q-Q plot of log-expenditures
# d) What do you find?  (Briefly summarize)
# Response Transformation: let's try log-transform
lexpen = log(expen)
boxcox(expen~1)
par(mfrow=c(2,4))
hist(lexpen)
hist(wealth)
hist(pop)
hist(pint)
hist(dens)
hist(income)
hist(growr)
vioplot(lexpen)
dev.off()
## Task 4: EDA for predictors, univariate analysis
# For this lab and EDA, let us consider only log-transformations for all variables.
# Note that in practice, you would scan through a variety of non-linear transformations.
#
# Use scatter plot smooths to study the relationship between log-expenditure and
# a) wealth and log-wealth (see eda_ClassVersion.R for code)
lwealth = log(wealth)
plot(wealth, lexpen)
plot(lwealth, lexpen)
lines(lowess(lwealth,lexpen), col="red")
# b) population and log-population with a cut-point
#   here is code to do (b)
plot(pop, lexpen)  # notice need for a transformation on pop
lpop = log(pop)
plot(lpop, lexpen)
lines(lowess(lpop,lexpen), col="blue")
lines(c(8,8),c(0,6), col="grey", lwd=3)
# c) density and log-density with a cut-point (ammend code from b)
ldens = log(dens)
plot(dens, lexpen)
plot(ldens, lexpen)
lines(lowess(ldens,lexpen), col="red")
lines(c(4.3,4.3),c(0,6), col="green", lwd=3)
## Task 5: Growth rate
# a) Study a histogram of growth rate--can we apply a log-transformation?
hist(growr)
lgro = log(growr)
hist(lgro)
#we can, there's na's though.

# b) Consider a piecewise transformation:
# Use different transformations for growth rates > 0 and for growth rates < 0.
# As long as the joint transformation is monotonic and keeps the zero at 0, we are fine.
# One approach is the transformation t(x), x>0; -t(-x), x<0. 
# Then can apply log-transformation!  
# But have to be careful with x=0 since ln(x)=ln(0) is undefined.  Easy fix: add 0.15.
# Create the piecewise function
lgrowr<-ifelse(growr>0, log(growr+0.15), -log(-growr+0.15))
# c) Study a scatter plot smooth of log-expenditure against this new lgrowr variable 
plot(lgrowr)
lines(lowess(lgrowr), col="red")

# d) In a 2x2 graphic, present the scatter plot smooths from Task 4 (a)-(c), 
#    and then this Task 5 (c) here.

# e) Interpret the four graphics in (d) in terms of the seeming success of the
#    transformations to produce linear relationships with log-expenditures.


## Task 6: Studying relationship between predictors, correlation
# Study and interpret a correlation plot.
# Set up data frame with only the inputs
#install.packages("corrplot")
library(corrplot)
ny2vars = data.frame(expen, wealth, pop, pint, dens, income, growr)
cny2 = cor(ny2vars)
corrplot(cny2)



