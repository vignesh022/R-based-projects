Fluid=read.csv("fluid.csv")
str(Fluid)
Fluid$xsqd=Fluid$x^2
str(Fluid)
Fluid$logt=log(Fluid$t)
str(Fluid)

fit=lm(log(xsqd)~logt,data=Fluid)
summary(fit)
plot(fit)
