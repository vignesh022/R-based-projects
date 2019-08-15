data(cars)
?cars
str(cars)
?str

pairs(cars)     ## plots a matrix of pairwise scatterplots
summary(cars)

x=cars$dist
y = cars$speed

plot(x,y)
abline(fit)
var(x)
var(y)
hist(cars)
hist(x)
hist(y)

fit = lm(speed~dist,data=cars)
fit

summary(fit)
res = fit$resid
hist(res)

plot(fit)

##
## Diagnostic plots
##

## plot fitted values vs residuals (check for heteroscedasticity)
plot(yhat,res)
## plot residuals vs each covariate (check for heteroscedasticity and nonlinearity)
plot(cars$dist,res)
abline(0,0,col="red")

## partial residual plots (requires the "car" package)
library(car)
crPlots(fit)
## partial residual plots using prp.r
## (put "prp.r" in your R working directory, or copy/paste it into R)
source("prp.r")
prp(fit,cars,names=c("dist"))
## QQ-plot of residuals (check normality)
qqnorm(res)
qqline(res)

fit2=lm(log(speed)~dist,data=cars)
summary(fit2)
plot(fit2)

fit3=lm((speed^0.5)~dist,data=cars)
summary(fit3)
plot(fit3)

cars$speed.squared=cars$speed^2
str(cars)

fit3=lm(speed.squared~dist,data=cars)
summary(fit3)        
plot(fit3)

cars$speed.cubed=cars$speed^3
str(cars)
fit4=lm(speed.cubed~dist,data=cars)
summary(fit4)
plot(fit4)

cars$dist.squared=cars$dist^2
str(cars)
fit5=lm(speed~dist.squared,data=cars)
summary(fit5)
plot(fit5)

cars$dist.cubed=cars$dist^3
str(cars)
fit6=lm(speed~dist.cubed,data=cars)
summary(fit6)

plot(cars$dist,cars$speed.squared)
abline(fit3)

y3=cars$speed.squared
yhat3=fit3$fitted.values
plot(yhat3,y3)
res3=fit3$resid
hist(res3)
plot(yhat3,res3)
abline(0,0,col="red")
source("prp.r")
prp(fit3,cars,names=c("dist"))
## QQ-plot of residuals (check normality)
qqnorm(res3)
qqline(res3)
