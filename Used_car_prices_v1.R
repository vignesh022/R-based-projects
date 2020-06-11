usedcars = read.csv("UsedCars.csv")
##pairs(usedcars)

##Linear Regression is used to regress 'Price' on the predictors
fit = lm(Price~.,data=usedcars)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

##Removing non-siginificant predictors
fit = lm(Price~.,data=usedcars[,-c(4,7,8,9)])
summary(fit)
par(mfrow=c(2,2))
plot(fit)

library(car)
outlierTest(fit)
crPlots(fit)
vif(fit)
#########################################################################################################################
#########################################################################################################################
## Removing all those observations that are outliers
newdata=usedcars[-c(151,152,153,154,155,156,157,158,159,160,342,341,343,344,345,347,346,348,349,350,351,352,355,354,353,357),]

##Refitting complete model
fit.new = lm(Price~.,data=newdata)
summary(fit.new)
par(mfrow=c(2,2))
plot(fit.new)

outlierTest(fit.new)
crPlots(fit.new)
vif(fit.new)
#########################################################################################################################
#########################################################################################################################
##Liter and Cylinder appear to be correlated and hence we exclude Liter and other non-significant variables
fit.new = lm(Price~.,data=newdata[,-c(5,7,8,9)])
summary(fit.new)
par(mfrow=c(2,2))
plot(fit.new)

outlierTest(fit.new)
crPlots(fit.new)
vif(fit.new)

## To treat non-normal data we plot a histogram of the response to identify its nature
par(mfrow=c(1,2))
hist(newdata$Price)
#To reduce the skewness we take its log
newdata$Price.new = log(newdata$Price)
hist(newdata$Price.new)

##Fitting a linear model between log(Price) and significant predictors
fit.new = lm(Price.new~.,data=newdata[,-c(1,5,7,8,9)])
summary(fit.new)
par(mfrow=c(2,2))
plot(fit.new)

outlierTest(fit.new)
crPlots(fit.new)
vif(fit.new)

##Check for all significant 2-way interactions in the model
fit.new.2 = lm(Price.new~(Mileage+Make+Cylinder+Doors)^2,data=newdata[,-c(1,5,7,8,9)])
summary(fit.new.2)
par(mfrow=c(2,2))
plot(fit.new.2)
vif(fit.new.2)

##Updated model without significant interactions due to presence of multicollinearity
fit.new.2 = lm(Price.new~Mileage+Make+Cylinder+Doors,data=newdata[,-c(1,5,7,8,9)])
summary(fit.new.2)
par(mfrow=c(2,2))
plot(fit.new.2)
outlierTest(fit.new.2)
vif(fit.new.2)
#########################################################################################################################
#########################################################################################################################
##Best model selection and CVMSPE comparison
newdata.2 = newdata[,-c(1,5,7,8,9)]
## set aside a test set (20% of data)
n = length(newdata.2$Price.new)
n.test = round(n * .2)
n.train = n - n.test
n
n.test
n.train

test.idx = sample(1:n,size = n.test)
train.idx = (1:n)[-test.idx]

train = newdata.2[train.idx,]
test = newdata.2[test.idx,]

#########################################################################################################################
#########################################################################################################################
## AIC based selection
fit.AIC=step(fit.new.2)
summary(fit.AIC)
vif(fit.AIC)

yhat.AIC=predict(fit.AIC,newdata=test)
mspe.AIC=mean((test$Price.new-yhat.AIC)^2)
mspe.AIC
accuracy.aic = 1-(mspe.AIC^0.5)
accuracy.aic
##
## V-fold Cross Validation
##
V=10
cvmspe=rep(NA,V)
cv.idx=as.numeric(matrix(1:V,nrow=n.train,ncol=1))
## randomly assign data points to each of V parts
cv.idx=sample(cv.idx)

## loop through, holding out one set of data at each point
for(i in 1:V){
  hold.out.idx=which(cv.idx==i)
  fit=lm(Price.new~Mileage+Make+Cylinder+Doors,data=train[-hold.out.idx,])
  y.pred=predict(fit,newdata=train[hold.out.idx,])
  cvmspe[i]=mean((train$Price.new[hold.out.idx]-y.pred)^2)
}
summary(fit)
cvmspe.AIC=mean(cvmspe)
cvmspe.AIC
accuracy.aic.2 = 1-(cvmspe.AIC^0.5)
accuracy.aic.2
#########################################################################################################################
#########################################################################################################################
##Choosing the best/cheapest vehicle for average Mileage, Cylinder and Doors amongst the 6 Makes

Mileage.mean = mean(newdata.2$Mileage)
Cylinder.mean = mean(newdata.2$Cylinder)
Doors.mean = mean(newdata.2$Doors)
predict.data = c(Mileage.mean,Cylinder.mean,Doors.mean)

##Buick
price.buick = 9.279 - (7.687e-06*predict.data[1]) + (0.1595*predict.data[2]) - (0.03487*predict.data[3])
exp(price.buick)
##Cadillac
price.cadillac = 9.6274 - (7.687e-06*predict.data[1]) + (0.1595*predict.data[2]) - (0.03487*predict.data[3])
exp(price.cadillac)
##Chevrolet
price.chevrolet = 9.0868 - (7.687e-06*predict.data[1]) + (0.1595*predict.data[2]) - (0.03487*predict.data[3])
exp(price.chevrolet)
##Pontiac
price.pontiac = 9.1733 - (7.687e-06*predict.data[1]) + (0.1595*predict.data[2]) - (0.03487*predict.data[3])
exp(price.pontiac)
##SAAB
price.saab = 9.9282 - (7.687e-06*predict.data[1]) + (0.1595*predict.data[2]) - (0.03487*predict.data[3])
exp(price.saab)
##Saturn
price.saturn = 9.1177 - (7.687e-06*predict.data[1]) + (0.1595*predict.data[2]) - (0.03487*predict.data[3])
exp(price.saturn)

## table
T=matrix(c(exp(price.buick),exp(price.cadillac),exp(price.chevrolet),exp(price.pontiac),exp(price.saab),exp(price.saturn)),ncol=1)
rownames(T)=c("Buick","Cadillac","Chevrolet","Pontiac","SAAB","Saturn")
colnames(T)=c("UsedCarPrice")
T
