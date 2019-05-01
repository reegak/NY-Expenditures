library(MASS)
library(corrplot)
library(car)
#install.packages("le  aps")
library(leaps)
par(mfrow = c(1,1))
options(warn=-1)  # forces R to ignore all warning messages
ny<-read.table("C:/Users/Kelso Quan/Documents/SchoolWork/Stat696/cs73.dat",header=T); dim(ny)
ny2<-na.omit(ny); dim(ny2) # 914  11
attach(ny2)
names(ny2)

# look at the density of each variable especially response
# because response looks log normal argue that it needs a log transform
lpop=log(pop)
ldens = log(dens)
lexpen = log(expen)
plot(x = lpop, y = lexpen)
lines(lowess(lpop,lexpen), col=2)
lines(c(8,8), c(0,6), col = "blue", lwd = 3)
plot(x = ldens, y = lexpen)
lines(lowess(ldens, lexpen), col = 2)
lines(c(4.3,4.3), c(0,6), col = "blue", lwd = 3)
set2 = (lpop > 8.3 & ldens > 4.5) #specification of the analysis
hist(expen[set2], main = "Expenditures")
lexpen<-log(expen[set2])
hist(lexpen, main = "Log Expenditures") #lexpen is "normal"
hist(wealth[set2], main = "Wealth")
lwealth<-log(wealth[set2])
hist(lwealth, main = "Log Wealth") #lwealth is "normal"
hist(pop[set2], main = "Population")
lpop<-log(pop[set2])
hist(lpop, main = "Log Population") #close to "normal"
hist(dens[set2], main = "Density")
ldens<-log(dens[set2])
hist(ldens, main = "Log Density") #terrible
hist(income[set2], main = "Income") #kinda better than lincome
lincome<-log(income[set2])
hist(lincome, main = "Log Income")
hist(pint[set2], main = "Percent Intergovernment")
lpint <- log(pint[set2])
hist(lpint, main = "Log Percent Intergovernment") 
hist(growr[set2], main = "Growth Rate")
lgrowr<-ifelse(growr[set2]>0, log(growr[set2]+1), -log(-growr[set2]+1))
hist(lgrowr, main = "Log Growth Rate")

#correlation matrix
nydata <- data.frame(lexpen, lwealth, lpop, lpint, ldens, lincome, lgrowr)
cormat <-cor(nydata)
corrplot(cormat, main = "Correlation between Log Variables")

# plot expense with each covariate
plot(lwealth, lexpen, main = "Log Wealth vs Log Expenditures") 
lines(lowess(lwealth,lexpen), col=2) #lwealth is linear
plot(lincome, lexpen, main = "Log Income vs Log Expenditures")
lines(lowess(lincome,lexpen), col=2) #lincome is linear
plot(lpop, lexpen, main = "Log Population vs Log Expenditures")
lines(lowess(lpop,lexpen), col=2) #lpop is linear
plot(lpint, lexpen , main = "Log % Intergovernment vs Log Expenditures")
lines(lowess(lpint,lexpen), col=2) #lpint is linear-ish
plot(ldens, lexpen , main = "Log Density vs Log Expenditures")
lines(lowess(ldens,lexpen), col=2) #ldens is linear-ish
plot(lgrowr, lexpen, main = "Log Growth Rate vs Log Expenditures")
lines(lowess(lgrowr,lexpen), col=2) #lgrowr is linear-ish

# finding a fit
fit1<-lm(lexpen~lwealth+lpop+lpint+ldens+lincome+lgrowr) 
par(mfrow=c(2,3))
plot(lexpen~lwealth+lpop+lpint+ldens+lincome+lgrowr)
plot(fit1)
par(mfrow=c(1,1))
stepAIC(fit1, direction = "both") #this one
summary(fit1)
exp(confint(fit1)) #confident interval for coefficients
#predictions for 1992, 2005, and 2025
sdfit <- sd(fit1$resid)
war92 <- data.frame(lwealth=log(72908), lpop=log(16225),  lpint=log(24.7), 
                    ldens= log(170),  lincome=log(19044), 
                    lgrowr=log(30.3 + 1))
war05 <- data.frame(lwealth=log(85000), lpop=log(20442),  lpint=log(24.7), 
                   ldens= log(214),  lincome=log(19500), 
                   lgrowr=log(35+1))
war25 = data.frame(lwealth=log(89000), lpop=log(31033), lpint=log(26.0), 
                   ldens = log(325), lincome=log(20000), 
                   lgrowr=log(40+1))
warick92=predict.lm(fit1,war92); exp(warick92+sdfit^2/2)
exp(predict(fit1, war92, interval="prediction")+sdfit^2/2)
warick05=predict.lm(fit1,war05); exp(warick05+sdfit^2/2)
exp(predict(fit1, war05, interval="prediction")+sdfit^2/2)
warick25=predict.lm(fit1,war25); exp(warick25+sdfit^2/2)  
exp(predict(fit1, war25, interval="prediction")+sdfit^2/2)


mon92 <- data.frame(lwealth=log(55067), lpop=log(9338), lpint=log(8.8), 
                    ldens = log(599), lincome=log(17100), 
                    lgrowr=log(35+1))
mon05 <- data.frame(lwealth=log(58000), lpop=log(10496), lpint=log(8.8), 
                    ldens = log(695), lincome=log(16726), 
                    lgrowr=log(30+1))
mon25 <- data.frame(lwealth=log(60000), lpop=log(13913), lpint=log(10.1), 
                    ldens = log(959), lincome=log(18000), 
                    lgrowr=log(35+1))
monroe05 <- predict.lm(fit1,mon92); exp(monroe05+sdfit^2/2)
exp(predict(fit1, mon92, interval = "prediction")+ sdfit^2/2)
monroe05 <- predict.lm(fit1,mon05); exp(monroe05+sdfit^2/2)
exp(predict(fit1, mon05, interval = "prediction")+ sdfit^2/2)
monroe25 <- predict.lm(fit1,mon25); exp(monroe25+sdfit^2/2)
exp(predict(fit1,mon25, interval = "prediction")+ sdfit^2/2)

#outliers
plot(predict(fit1), rstudent(fit1), ylab="Studentized Residuals", xlab="Predicted")
identify(predict(fit1), rstudent(fit1), labels=row.names(ny2)) # 'escape to finish'

predict(fit1)[rstudent(fit1)==min(rstudent(fit1))]
sresid <- studres(fit1)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit, col = 2)

qqPlot(fit1, main="QQ Plot", ylab="Studentized Residuals")
cutoff <- 4/((nrow(set2)-length(fit1$coefficients)-2))
plot(fit1, which=4, cook.levels=cutoff) # influence Plot via cook's distance
influencePlot(fit1, id.method="identify", 
              main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
vif(fit1) #numbers above 8 is bad

bc = boxcox ( expen~ . , data=ny2 )
bc $x [ bc $y==max( bc $y ) ]

