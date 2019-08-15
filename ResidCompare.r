# cars data
data(cars)
head(cars)
str(cars)
## lm fit for
## (dist/speed) = b0+b1*speed+eps, eps ~ N(0,s2)


fit=lm(dist/speed~speed,data=cars)
summary(fit)

source("prp.r")
prp(fit,cars,"speed")


## get residuals
## eps.hat = (I-H)y

eps.hat=resid(fit)
plot(eps.hat)

## calculate the covariance matrix of the residuals
## cov(eps.hat) = s2*(I-H)

X=model.matrix(fit)
head(X)
str(X)

H=X%*%solve(t(X)%*%X)%*%t(X)
I=diag(50)

s2.hat=(summary(fit)$sigma)^2
s2.hat

t(eps.hat)%*%eps.hat/(50-2)

Cov.resids=s2.hat*(I-H)
Cov.resids[1:10,1:10]


##
## Get standardized residuals
##

eps.standard=rstandard(fit)

##
## Get studentized residuals
##

eps.student=rstudent(fit)

##
## compare
##

## note that the largest residuals are "pulled out" by standardizing
## but that overal the differences are pretty minor

plot(eps.hat,ylim=c(-1.5,4))
points(eps.standard,pch=3,col="red")
points(eps.student,pch=2,col="blue")
legend("topright",legend=c("Residuals","Standardized Resids","Studentized Resids"),col=c("black","red","blue"),pch=c(1,3,2))

pairs(cbind(eps.hat,eps.standard,eps.student))

