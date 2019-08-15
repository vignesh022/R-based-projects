load("isj.Rdata")
head(isj)
pairs(isj)

fit=lm(isj~.,data=isj)
summary(fit)

fit=glm(isj~y+forest+chap,family=("binomial"),data=isj)

fit=glm(isj~x+y+elev+forest+chap,data=isj,family=("binomial"))
summary(fit)

## hypothesis tests
betahat=coef(fit)
X=model.matrix(fit)

######################################################
##
## Residual analysis
##
######################################################

par(mfrow=c(2,2))
plot(fit)


## outlier check
library(car)
outlierTest(fit)

## partial residual plots
crPlots(fit)

## variance inflation factors
vif(fit)
 
#####################################################
plot(isj[,2:3],type="n",main="probability",asp=1)
na.idx=which(is.na(isj$isj))
isj.star=isj$isj[-na.idx]
norm.isj=(isj.star-min(isj.star))/(max(isj.star)-min(isj.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.isj))
points(isj[1:307,2:3],pch=4,col="red")
#####################################################
newdata = data.frame(y=isj$y[1:307],chap=isj$chap[1:307],forest=isj$forest[1:307])
mu=predict.glm(fit,newdata=newdata,type="response")
newdata2 = data.frame(mu2=mu,x=isj$x[1:307],y=isj$y[1:307],chap=isj$chap[1:307],forest=isj$forest[1:307])
plot(newdata2[,2:3],type="n",main="probability",asp=1)
na.idx2=which(is.na(newdata2$mu2))
isj2.star=newdata2$mu2[-na.idx2]
norm.isj2=(isj2.star-min(isj2.star))/(max(isj2.star)-min(isj2.star))
points(newdata2[-na.idx2,2:3],pch=20,col=grey(1-norm.isj2))
points(newdata2[1:307,2:3],pch=4,col="red")
#####################################################
coef(fit)
exp(coef(fit))
