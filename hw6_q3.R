load("dispersal.Rdata")

dispersal.f=dispersal[dispersal$sex=='F',]
dispersal.m=dispersal[dispersal$sex=='M',]

nll.reg <- function(s2,y.f,y.m,t.f,t.m){
  
  ## get parameters
  s2.f = s2[1]
  s2.m = s2[2]
  
  ## calculate loglikelihood
  
  net.loglikelihood=sum(dnorm(y.f,0,sqrt(t.f*s2.f),log=T))+sum(dnorm(y.m,0,sqrt(t.m*s2.m),log=T))
  
  ## return negative loglikelihood
  -net.loglikelihood
}


## find MLE using "optim" numerical optimization
##
s2.init=c(1,1)
out=optim(s2.init,nll.reg,y.f=dispersal.f$d,y.m=dispersal.m$d,t.f=dispersal.f$t,t.m=dispersal.m$t,control=list(trace=10),hessian=T)

## get parameter estimates (order is the same as in beta.s2)
out$par


## get standard errors from optim
H=out$hessian
S=solve(H)
se=sqrt(diag(S))
se

CI.s2.m.up = out$par[2]+1.96*se[2] 
CI.s2.m.low = out$par[2]-1.96*se[2]

CI.s2.f.up = out$par[1]+1.96*se[1] 
CI.s2.f.low = out$par[1]-1.96*se[1]
