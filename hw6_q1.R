mistletoe=read.csv("mistletoe.csv")

na.idx=which(is.na(mistletoe$infected.USU))

mistletoe.USU = data.frame(mistletoe[-na.idx,1:17])

na.idx2=which(mistletoe.USU$infected.USU==1)
mistletoe.err0 = data.frame(mistletoe.USU[-na.idx2,1:17])

na.idx3=which(mistletoe.USU$infected.USU==0)
mistletoe.err1 = data.frame(mistletoe.USU[-na.idx3,1:17])

par(mfrow=c(2,2))

##linear
form="infected.mndnr~csize+cdense+usize+udense+phys+mortal+si+age+ba+dbh+height+volume+dense+x+y"
form="infected.USU~csize+cdense+usize+udense+phys+mortal+si+age+ba+dbh+height+volume+dense+x+y"
form="infected.mndnr~0+dense"

fit=glm(form,family="binomial",data=mistletoe.err1)
summary(fit)
plot(fit)
vif(fit)
outlierTest(fit)
crPlots(fit)

newfit=step(fit)
summary(newfit)

coef = c(-fit$coefficients)
coef

X = seq(min(mistletoe.err1$phys),max(mistletoe.err1$phys), (max(mistletoe.err1$phys)-min(mistletoe.err1$phys))/1000)
Y = (exp(coef[3]*X)/(1+exp(coef[3]*X)))
plot(X,Y)

X = seq(min(mistletoe.err1$si),max(mistletoe.err1$si), (max(mistletoe.err1$si)-min(mistletoe.err1$si))/1000)
Y = (exp(coef[5]*X)/(1+exp(coef[5]*X)))
plot(X,Y)

X = seq(min(mistletoe.err1$dbh),max(mistletoe.err1$dbh), (max(mistletoe.err1$dbh)-min(mistletoe.err1$dbh))/1000)
Y = (exp(coef[6]*X)/(1+exp(coef[6]*X)))
plot(X,Y)

X = seq(min(mistletoe.err1$y),max(mistletoe.err1$y), (max(mistletoe.err1$y)-min(mistletoe.err1$y))/1000)
Y = (exp(coef[8]*X)/(1+exp(coef[8]*X)))
plot(X,Y)
