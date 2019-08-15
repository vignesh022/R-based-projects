load('fire.Rdata')

##Logistic Regression is used to regress 'burnt' on the predictors since response is binary
fit = glm(burnt~.,family="binomial",data=fire)
summary(fit)

##Choosing the best model using stepwise AIC-based variable selection
fit.aic = step(fit)
summary(fit.aic)

## outlier check
library(car)
outlierTest(fit.aic)
## partial residual plots
crPlots(fit.aic)
## variance inflation factors
vif(fit.aic)

##Identifying key interaction terms between the 6 characteristics near the home
fit.6 = glm(burnt~(planted+buildings+perc.woody+perc.cleared+distance2bush+distance2tree)^2,family="binomial",data=fire)
summary(fit.6)

##Removing insignificant terms from the AIC model and Including interactions
fit.new = glm(burnt~(ffdi+perc.burntless5yrs+amt.not.NP+distance2tree+planted+perc.woody),family="binomial",data=fire)
summary(fit.new)

fit.aic.new = step(fit.new)
summary(fit.aic.new)

coef = fit.aic.new$coefficients

fire$plantedp=rep(0,nrow(fire))
fire$plantedp[fire$planted=='p']=1
fire$plantedr=rep(0,nrow(fire))
fire$plantedr[fire$planted=='r']=1
##Plotting for final model
par(mfrow=c(2,2))
X = seq(min(fire$plantedp),max(fire$plantedp), (max(fire$plantedp)-min(fire$plantedp))/1000)
Y = (exp(coef[1]*X)/(1+exp(coef[1]*X)))
plot(X,Y)

X = seq(min(fire$distance2tree),max(fire$distance2tree), (max(fire$distance2tree)-min(fire$distance2tree))/1000)
Y = (exp(coef[3]*X)/(1+exp(coef[3]*X)))
plot(X,Y)

X = seq(min(fire$plantedr),max(fire$plantedr), (max(fire$plantedr)-min(fire$plantedr))/1000)
Y = (exp(coef[4]*X)/(1+exp(coef[4]*X)))
plot(X,Y)

X = seq(min(fire$perc.woody),max(fire$perc.woody), (max(fire$perc.woody)-min(fire$perc.woody))/1000)
Y = (exp(coef[5]*X)/(1+exp(coef[5]*X)))
plot(X,Y)

############################################################################################################################################
############################################################################################################################################

##Best model selection and CVMSPE comparison between AIC, RR and LASSO

## set aside a test set (20% of data)
n = length(fire$burnt)
n.test = round(n * .2)
n.train = n - n.test
n
n.test
n.train

test.idx = sample(1:n,size = n.test)
train.idx = (1:n)[-test.idx]

train = fire[train.idx,]
test = fire[test.idx,]

#######################################################
##
## (2) AIC Stepwise Selection
##
#######################################################
fit.AIC=step(fit)
summary(fit.AIC)

yhat.AIC=predict(fit.AIC,newdata=test,type="response")
mspe.AIC=mean((test$burnt-yhat.AIC)^2)
mspe.AIC
accuracy.aic = 1-(mspe.AIC^0.5)
accuracy.aic
##
## V-fold Cross Validation for AIC (to compare with RR / Lasso)
##


## divide training data into V equal groups
n=nrow(train)
V=10
idx.group=rep(1:V,floor(n/V+1))
idx.group=idx.group[1:n]
idx.group=sample(idx.group)
idx.group

## cross-validation
cvmspe=rep(NA,V)
cv.best.models=list()
for(v in 1:V){
  idx.holdout=which(idx.group==v)
  fit=step(glm(burnt~.,data=train[-idx.holdout,]),trace=0)
  cv.best.models[[v]] <- formula(fit)
  yhat=predict(fit,newdata=train[idx.holdout,],type="response")
  cvmspe[v]=mean((train$burnt[idx.holdout]-yhat)^2)
}
cvmspe

cvmspe.AIC=mean(cvmspe)

## best subsets (via AIC) for each CV group:
cv.best.models
## note how different they are!
## AIC-based selection is "unstable" - small changes in the data
##   can lead to significant changes in the results!

#######################################################
##
## (3) ridge regression
##
#######################################################

## first check VIFs
library(car)
vif(fit)

library(glmnet)
## alpha=0 gives ridge regression
## alpha=1 gives lasso regression
fit = glm(burnt~.,family="binomial",data=train)
summary(fit)
X = model.matrix(fit)
y=train[,1]
## fit ridge (trying 100 different lambda values)
rr=glmnet(x=X,y=y,family="binomial",alpha=0,nlambda=100)
plot(rr,xvar="lambda",main="Ridge Regression Betas for Different Values of the Tuning Parameter")

## use 10-fold crossvalidation to find the best lambda
cv.rr=cv.glmnet(x=X,y=y,alpha=0,nfolds=10,family="binomial")

## getting cvmspe from best value of lambda
cvmspe.rr=min(cv.rr$cvm)

## get lambda and best rr fit
lambda.rr=cv.rr$lambda.min
lambda.rr

## some plots
par(mfrow=c(2,2))
plot(cv.rr)
abline(v=log(lambda.rr))
plot(rr,xvar="lambda",main="Ridge Regression Betas for Different Values of the Tuning Parameter")
abline(v=log(lambda.rr))

## beta estimates for best lambda
betas.rr=coef(cv.rr,s="lambda.min")
plot(betas.rr)

fit = glm(burnt~.,family="binomial",data=test)
X.new = model.matrix(fit)

yhat.rr=predict(cv.rr,s="lambda.min",newx=X.new,type="response")
mspe.rr=mean((test$burnt-yhat.rr)^2)
mspe.rr
accuracy.rr = 1-(mspe.rr^0.5)
accuracy.rr


#######################################################
##
## (4) lasso regression
##
#######################################################

## alpha=0 gives ridge regression
## alpha=1 gives lasso regression
fit = glm(burnt~.,family="binomial",data=train)
X = model.matrix(fit)
y=train[,1]
## fit lasso (trying 100 different lambda values)
lasso=glmnet(x=X,y=y,family="binomial",alpha=1,nlambda=200)
plot(lasso,xvar="lambda",main="Lasso Regression Betas for Different Values of the Tuning Parameter")
plot(rr,xvar="lambda",main="Ridge Regression Betas for Different Values of the Tuning Parameter")

## use 10-fold crossvalidation to find the best lambda
cv.lasso=cv.glmnet(x=X,y=y,alpha=1,nfolds=10,family="binomial")

## get lambda and best lasso fit
lambda.lasso=cv.lasso$lambda.min
lambda.lasso

## getting cvmspe from best value of lambda
cvmspe.lasso=min(cv.lasso$cvm)

## some plots
par(mfrow=c(1,2))
plot(cv.lasso)
abline(v=log(lambda.lasso))
plot(lasso,xvar="lambda")
abline(v=log(lambda.lasso))

## beta estimates for best lambda
betas.lasso=coef(cv.lasso,s="lambda.1se")
betas.lasso

plot(betas.rr,main="Ridge Regression")
plot(betas.lasso,main="Lasso Regression")

fit = glm(burnt~.,family="binomial",data=test)
X.new = model.matrix(fit)

yhat.lasso=predict(cv.lasso,newx=X.new,s="lambda.min",type="response")
mspe.lasso=mean((test$burnt-yhat.lasso)^2)
mspe.lasso
accuracy.lasso = 1-(mspe.lasso^0.5)
accuracy.lasso
#############################################################
##
## (5) comparison of CVMSPE on training set and MSPE on test set
##
#############################################################

##
## compare CVMSPE
##

cvmspe.AIC
cvmspe.rr
cvmspe.lasso


##
## Comparison of MSPE on test set
##

mspe.AIC
mspe.rr
mspe.lasso

##
## Comparison of accuracy on test set
##

accuracy.aic
accuracy.rr
accuracy.lasso

## table
T=matrix(c(cvmspe.AIC,cvmspe.rr,cvmspe.lasso,mspe.AIC,mspe.rr,mspe.lasso,accuracy.aic,accuracy.rr,accuracy.lasso),ncol=3)
rownames(T)=c("AIC","RR","Lasso")
colnames(T)=c("CVMSPE-Train","MSPE-Test","Accuracy-Test")
T

############################################################################################################################################
############################################################################################################################################
form.best = "burnt~(ffdi+topo+perc.cleared+perc.burntless5yrs+amt.not.NP+adj.for.type+distance2tree+planted+perc.woody)"
par(mfrow=c(3,1))

##Setting all trees with "distance2tree" < 10 as =10
fire.new = fire
fire.new$distance2tree[fire.new$distance2tree<10]=10
fit.best=glm(form.best,family="binomial",data=fire.new)
summary(fit.best)
yhat=predict(fit.best,newdata=fire.new,type="response")
mspe.1=mean((fire.new$burnt-yhat)^2)
mspe.1
accuracy.1 = 1-(mspe.1^0.5)
accuracy.1

X = seq(min(fire.new$distance2tree),max(fire.new$distance2tree), (max(fire.new$distance2tree)-min(fire.new$distance2tree))/1000)
Y = (exp(fit.best$coefficients[9]*X)/(1+exp(fit.best$coefficients[9]*X)))
plot(X,Y)

##Setting "perc.woody" to 50% for those with >50%
fire.new = fire
fire.new$perc.woody[fire.new$perc.woody>50]=50
fit.best=glm(form.best,family="binomial",data=fire.new)
summary(fit.best)
yhat=predict(fit.best,newdata=fire.new,type="response")
mspe.2=mean((fire.new$burnt-yhat)^2)
mspe.2
accuracy.2 = 1-(mspe.2^0.5)
accuracy.2

X = seq(min(fire.new$perc.woody),max(fire.new$perc.woody), (max(fire.new$perc.woody)-min(fire.new$perc.woody))/1000)
Y = (exp(fit.best$coefficients[11]*X)/(1+exp(fit.best$coefficients[11]*X)))
plot(X,Y)

##Replacing all "remnants" with "planted"
fire.new = fire
fire.new$planted[fire.new$planted=='r']='p'
fit = glm(burnt~. - factor(planted),family="binomial",data=fire.new)
summary(fit)

##Choosing the best model using stepwise AIC-based variable selection
fit.aic = step(fit)
summary(fit.aic)
form.best = "burnt~(ffdi+topo+perc.burntless5yrs+perc.woody)"
fit.best=glm(form.best,family="binomial",data=fire.new)
summary(fit.best)
yhat=predict(fit.best,newdata=fire.new,type="response")
mspe.3=mean((fire.new$burnt-yhat)^2)
mspe.3
accuracy.3 = 1-(mspe.3^0.5)
accuracy.3

fire.new$planted[fire.new$planted=='r']=0
fire.new$plantedp[fire.new$planted=='p']=1
X = seq(0,1,0.001)
Y = (exp(fit.best$coefficients[1]*X)/(1+exp(fit.best$coefficients[1]*X)))
plot(X,Y)

## table
T=matrix(c(mspe.1,mspe.2,mspe.3,accuracy.1,accuracy.2,accuracy.3),ncol=2)
rownames(T)=c("distance2tree","perc.woody","planted")
colnames(T)=c("MSPE","Accuracy")
T
