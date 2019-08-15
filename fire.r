
load("fire.Rdata")
ls()
str(fire)
library(car)


fit=glm(burnt~.+I(distance2tree^2)+perc.woody*planted,data=fire,family=binomial)
summary(fit)
crPlots(fit)

fit.aic=step(fit)
summary(fit.aic)

## plot for perc.woody and planted
pred.data=fire[,c(2,3,5,17,18,20)]
summary(pred.data)
for(i in c(1,2,3,4)){
    pred.data[,i]=mean(pred.data[,i])
}
pred.data
pred.data$perc.woody=sort(pred.data$perc.woody)
data.p=pred.data
data.p$planted="p"
data.r=pred.data
data.r$planted="r"
pi.hat.p=predict(fit.aic,newdata=data.p,type="response")
pi.hat.r=predict(fit.aic,newdata=data.r,type="response")
plot(pred.data$perc.woody,pi.hat.p,ylim=c(0,1),type="l",lwd=3,main="Effect of perc.woody on Prob. of Burning")
points(pred.data$perc.woody,pi.hat.r,type="l",lwd=3,col="red")
legend("topleft",legend=c("planted","remnant"),lwd=3,col=c(1,2))


## plot for distance2tree
pred.data=fire[,c(1,2,3,5,17,18,20)]
summary(pred.data)
for(i in c(2,3,4,7)){
    pred.data[,i]=mean(pred.data[,i])
}
pred.data[,6]="p"
sort.idx=sort(pred.data$distance2tree,index.return=T)$ix
pred.data=pred.data[sort.idx,]
plot(pred.data$distance2tree,pred.data$burnt)
pi.hat=predict(fit.aic,newdata=pred.data,type="response")
points(pred.data$distance2tree,pi.hat,col="red",lwd=3,type="l")

outlierTest(fit.aic)
plot(cooks.distance(fit.aic))


