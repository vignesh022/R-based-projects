x=runif(200,-6,7)
y=2*x^3-7*x^2-40*x+100+rnorm(200,sd=90)
plot(x,y)

alldata=data.frame(y,x)

## set aside a test set (20% of data)
n=length(y)
n.test=round(n*.2)
n.train=n-n.test
n
n.test
n.train

test.idx=sample(1:n,size=n.test)
train.idx=(1:n)[-test.idx]

train=alldata[train.idx,]
test=alldata[test.idx,]


##specify a "formula" - try different orders of polynomial
form="y~x+I(x^2)+I(x^3)"
##

## randomly divide the data into V equal parts
V=10

cvmspe=rep(NA,V)
cv.idx=as.numeric(matrix(1:V,nrow=n.train,ncol=1))
cv.idx
## randomly assign data points to each of V parts
cv.idx=sample(cv.idx)

## loop through, holding out one set of data at each point
for(i in 1:V){
    hold.out.idx=which(cv.idx==i)
    fit=lm(form,data=train[-hold.out.idx,])
    y.pred=predict(fit,newdata=train[hold.out.idx,])
    cvmspe[i]=mean((train$y[hold.out.idx]-y.pred)^2)
}
summary(fit)
mean(cvmspe)


##
##
## Now use the best model to fit on the full training data and predict on the test set.
##
##
form.best="y~x+I(x^2)+I(x^3)"
fit=lm(form.best,data=train)
y.pred=predict(fit,newdata=test)
mspe.test=mean((test$y-y.pred)^2)

mspe.test
mean(cvmspe)

## note that out of sample MSPE is higher than CVMSPE!
