mistletoe = read.csv("mistletoe.csv")

## set aside a test set (20% of data)
n = length(fire$burnt)
n.test = round(n * .2)
n.train = n - n.test
n
n.test
n.train

test.idx = sample(1:n,size = n.test)
train.idx = (1:n)[-test.idx]

train = fire[train.idx,]
test = fire[test.idx,]

##categorical
form="infected.mndnr~factor(csize)+factor(cdense)+factor(usize)+factor(udense)+factor(phys)+factor(mortal)+si+age+ba+dbh+height+volume+dense+x+y"
##form="infected.mndnr~0+factor(phys)+mortal+si+dbh+height+dense+x"

##linear
form="infected.mndnr~csize+cdense+usize+udense+phys+mortal+si+age+ba+dbh+height+volume+dense+x+y"
##form = "infected.mndnr~csize+usize+udense+phys+mortal+si+height+dense+x"

##
## randomly divide the data into V equal parts
V = 10

cvmspe = rep(NA,V)
cv.idx = as.numeric(matrix(1:V,nrow = n.train,ncol = 1))
## randomly assign data points to each of V parts
cv.idx = sample(cv.idx)
cv.idx

## loop through, holding out one set of data at each point
for (i in 1:V) {
  hold.out.idx = which(cv.idx == i)
  fit = lm(form,data = train[-hold.out.idx,])
  y.pred = predict(fit,newdata = train[hold.out.idx,],type = "response")
  cvmspe[i] = mean((train$infected.mndnr[hold.out.idx] - y.pred) ^ 2)
}

mean(cvmspe)


##
##
## Now use the best model to fit on the full training data and predict on the test set.
##
##
form.best = "infected.mndnr~csize+cdense+usize+udense+phys+mortal+si+age+ba+dbh+height+volume+dense+x+y"
fit = lm(form.best,data = train)
summary(fit)
plot(fit)
vif(fit)
y.pred = predict(fit,newdata = test,type = "response")
mspe.test = mean((test$infected.mndnr - y.pred) ^ 2)

mspe.test
mean(cvmspe)

## MSPE was higher than CVMSPE
