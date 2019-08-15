fuel=read.csv("FuelEfficiency.csv")
str(fuel)
head(fuel)
pairs(fuel)

source("prp.r")

###################################################
##
## Modeling with Weight
##
###################################################


## simple lm with MPG~Weight

fit=lm(MPG~Weight,data=fuel)
summary(fit)

par(mfrow=c(2,2))
plot(fit)

prp(fit,fuel,"Weight")

## simple lm with MPH~poly(Weight,2)

fit=lm(MPG~poly(Weight,2),data=fuel)
summary(fit)

plot(fit)
prp(fit,fuel,"Weight")


## checking for outliers
r.star=rstudent(fit)
r.star

n=nrow(fuel)
p=ncol(model.matrix(fit))

max(r.star)
plot(r.star)
## this cutoff does NOT have a Bonferroni correction
cutoff=qt(.975,df=n-p-1)
abline(h=cutoff)
abline(h=-cutoff)

cutoff.bon=qt(1-.05/2/n,df=n-p-1)
cutoff.bon
## this is bigger than any of the studentized residuals



###################################################
##
## Modeling with Horsepower
##
###################################################


## simple lm with MPG~Horsepower

fit=lm(MPG~Horsepower,data=fuel)
summary(fit)

par(mfrow=c(2,2))
plot(fit)

prp(fit,fuel,"Horsepower")

## checking for outliers
r.star=rstudent(fit)
r.star

n=nrow(fuel)
p=ncol(model.matrix(fit))

max(r.star)
plot(r.star)
## this cutoff does NOT have a Bonferroni correction
cutoff=qt(.975,df=n-p-1)
abline(h=cutoff)
abline(h=-cutoff)

cutoff.bon=qt(1-.05/2/n,df=n-p-1)
cutoff.bon
## this is bigger than any of the studentized residuals



###################################################
##
## Now a joint model with multiple covariates
##
###################################################

fit=lm(MPG~poly(Weight,2)+Horsepower+Drive_Ratio+Displacement+Cylinders,data=fuel)
summary(fit)
## Q: what happened to Horsepower!!??
## A: multicollinearity

library(car)
vif(fit)

