smoking = read.csv("smoking.csv")
pairs(smoking)

##
##
## Specify model with offset = log(personyrs)
##
##
par(mfrow=c(2,2))

fit=lm(deaths~factor(agecat,smokes)+pyears,data=smoking)
summary(fit)

fit=glm(deaths~factor(agecat)+smokes,offset=log(pyears/1000),family=("poisson"),data=smoking)
summary(fit)

###############################################
##
## Residual diagnostics
##
###############################################

## residual plots
plot(fit)

## partial residual plots

library(car)
crPlots(fit)

## Test for outliers
library(car)
outlierTest(fit)

source("prp.r")
prp(fit,smoking,names=c("agecat","smokes","pyears"))


##
## simulation study to see if resids are acceptable
##

## simulate data from the fitted model
muhat=predict(fit,type="response")

ysim=rpois(length(muhat),lambda=muhat)

## fit the simulated data
fit.sim=glm(ysim~factor(agecat)+smokes,offset=log(pyears),family=poisson(link = "log"),data=smoking)

summary(fit.sim)

## residual plots
plot(fit.sim)
outlierTest(fit.sim)
crPlots(fit.sim)
## These look qualitatively very similar to resids from our model fit
## Conclusion: No particular problems with residuals.  Model is OK.
##


###############################################
##
## Predictions 
##
###############################################

## predicted mean # of resp.deaths per year for 1000 people in a particular group
##  by setting "arsenic=1:4" I am jointly calculating everything for all 4 arsenic groups
##  This allows us to compare predicted respitory death rates across arsenic exposure levels

## make a data frame with the desired predictor variables
data=data.frame(pyears=1000,agecat=3,smokes=0:1)
data

## get the mean and sd of the linear predictor eta for those predictor variables
pred.mean=predict(fit,newdata=data,type="response",se=T)
mu.hat=pred.mean$fit
pred.mean
## CI bounds on linear predictor
CI.up=mu.hat+1.96*pred.mean$se.fit
CI.down=mu.hat-1.96*pred.mean$se.fit

## CI on LINEAR PREDICTOR in table form
cbind(CI.down,CI.up)

## CI on MEAN # of resp.deaqths per year for 1000 people
## Note: for Poisson regression, response function is mu=exp(eta)
exp(cbind(CI.down,CI.up))

