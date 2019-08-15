data(cars)
str(cars)

fit=lm(dist~speed,data=cars)
summary(fit)

##beta.hat calculation

X=model.matrix(fit) #design matrix X
Y=data.matrix(cars[,2])

beta.hat=solve(t(X)%*% X)%*%t(X)%*%Y ##beta.hat vector

##residuals = (I-H)Y

H=X%*%solve(t(X)%*%X)%*%t(X)
I=diag(50)

res = (I-H)%*%Y
res.fit=fit$residuals
plot(res,res.fit)

##sigma.squared(s2) = (e'e/n-p)

s2=t(res)%*%res/(50-2)

##yhat vaules

yhat = X%*%beta.hat
plot(yhat,fit$fitted.values)

##studentized residuals

ri.fit = rstudent(fit)

ri = rep(0,50)
for(i in 1:50){
  ri[i]=(s2^-0.5*(res[i]/sqrt(1-q[i]))) ##q = leverage or diagonal of H
  }

plot(ri,ri.fit)

##standard error (sek) for k=1,2

z=solve(t(X)%*%X)

##k=1
se1 = sqrt(s2*z[1,1]) ##standard error for intercept
##k=2
se2 = sqrt(s2*z[2,2]) ##standard error for slope

##p-values for beta.k; k=1,2

## t-value = |beta.k|/se.k
t1 = beta.hat[1]/se1
t2 = beta.hat[2]/se2

p1 = 2*pt(-abs(t1),df=48) 
p2 = 2*pt(-abs(t2),df=48)

##R-squared = 1 - (SSE/SST)

#sse
sse = t(res)%*%res

#sst
one = rep(1,50)
sst = t((Y - mean(Y)*one))%*%(Y - mean(Y)*one)

r.squared = 1 - sse/sst


