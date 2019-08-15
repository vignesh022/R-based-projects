#################################################
##
## Logistic Regression
##
#################################################

##
## Read in data on grad school admissions
##
## admit = admission status (1=admitted, 0=not)
## gre = GRE score
## gpa = undergraduate GPA
## rank = "rank" of undergraduate institution
##         (four categories 1=best, 4=worst)

admissions <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(admissions)
pairs(admissions)

##
## Now try logistic regression
##

fit=glm(admit~gre+gpa+factor(rank),family=binomial(link="logit"),data=admissions)
summary(fit)

glm.fitted <- fitted(fit)

compare$predicted = predict(fit,newdata=admissions,type="response")


