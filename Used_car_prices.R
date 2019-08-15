usedcars = read.csv("new_car_data.csv")
##pairs(usedcars)

##Linear Regression is used to regress 'Price' on the predictors
fit = lm(Price~.,data=usedcars)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

library(car)
outlierTest(fit)
crPlots(fit)
vif(fit)

fit = lm(Price~.,data=usedcars[,-c(4,7,8,9)])
summary(fit)
par(mfrow=c(2,2))
plot(fit)

## outlier check using studentized residuals
r.star = rstudent(fit)
par(mfrow=c(1,1))
plot(r.star)
cutoff.r=qt(.975,df=nrow(usedcars)-length(fit$coefficients)-1)
abline(h=cutoff.r)
abline(h=-cutoff.r)

## partial residual plots
crPlots(fit)

## variance inflation factors
vif(fit)

#########################################################################################################################
#########################################################################################################################
## Removing all those observations that are outliers
index = which(abs(r.star)>cutoff.r)
index
newdata=usedcars[-index,]

##Linear Regression is used to regress 'Price' on the predictors
fit.new = lm(Price~.,data=newdata[,-c(4,7,8,9)])
summary(fit.new)
par(mfrow=c(2,2))
plot(fit.new)

r.star = rstudent(fit.new)
par(mfrow=c(1,1))
plot(r.star)
cutoff.r=qt(.975,df=nrow(newdata)-length(fit.new$coefficients)-1)
abline(h=cutoff.r)
abline(h=-cutoff.r)
#########################################################################################################################
#########################################################################################################################
## To treat non-normal data we plot a histogram of the response to identify its nature
hist(usedcars$Price)

##Transforming the response variable to log(Price) to treat the normality
fit = lm(log(Price)~.,data=usedcars[,-c(4,7,8,9)])
summary(fit)
par(mfrow=c(2,2))
plot(fit)

## outlier check using studentized residuals
r.star = rstudent(fit)
par(mfrow=c(1,1))
plot(r.star)
cutoff.r=qt(.975,df=nrow(usedcars)-length(fit$coefficients)-1)
abline(h=cutoff.r)
abline(h=-cutoff.r)

outlierTest(fit)
## partial residual plots
crPlots(fit)

## variance inflation factors
vif(fit)
