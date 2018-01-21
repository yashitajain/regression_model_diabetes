diabetes=read.table("diabetes.data.txt",header=TRUE)
summary(diabetes)
diabetes$SEX=as.factor(diabetes$SEX)
pairs(diabetes-diabetes$SEX)
par(mfrow=c(2,2))
#non-norma
hist(diabetes$AGE,prob=TRUE,col="yellow")
lines(density(diabetes$AGE))
qqnorm(diabetes$AGE)
qqline(diabetes$AGE)
hist(diabetes$BMI,col="green", prob=TRUE)
lines(density(diabetes$BMI))
qqnorm(diabetes$BMI)
qqline(diabetes$BMI)
hist(diabetes$BP,col="red", prob=TRUE)
lines(density(diabetes$BP))
qqnorm(diabetes$BP)
qqline(diabetes$BP)
hist(diabetes$S4,prob=T,col="blue")
lines(density(diabetes$S4))
qqnorm(diabetes$S4)
qqline(diabetes$S4)
#normal
hist(diabetes$S1,col="yellow",prob=T)
lines(density(diabetes$S1))
qqnorm(diabetes$S1)
qqline(diabetes$S1)
hist(diabetes$S2,prob=T,col="green")
lines(density(diabetes$S2))
qqnorm(diabetes$S2)
qqline(diabetes$S2)
hist(diabetes$S3,prob=T,col="red")
lines(density(diabetes$S3))
qqnorm(diabetes$S3)
qqline(diabetes$S3)
hist(diabetes$S6,prob=T,col="blue")
lines(density(diabetes$S6))
qqnorm(diabetes$S6)
qqline(diabetes$S6)

set.seed(26)
n=nrow(diabetes)

test=sample(n,round(n/4))
data.train=diabetes[-test,]
data.test=diabetes[test,]
x <- model.matrix(Y~ ., data = diabetes)[,-1]
x.test <- x[test,]
y <- diabetes$Y
y.test <- y[test]
#bestsusbet
library(leaps)
lm=regsubsets(Y~.,data.train,nvmax = 10)
best=summary(lm)
#bic
plot(best$bic, xlab="Number of Variables", ylab="BIC", type="l",font.lab=2)
x = which.min(best$bic)
y = best$bic[6]
points(x,y, col="red", cex=1, pch=19)
round(coef(summary(lm(Y~SEX+BMI+BP+S1+S2+S5, data = data.train))),2)
coefi  = coef(lm,id=6)
pred   = coefi[1] + (x.test[, names(coefi[-1])]%*%coefi[-1])
#rss
plot(best$rss, xlab="Number of Variables", ylab="RSS", type="l",font.lab=2)
x = which.min(best$rss)
y = best$rss[x]
points(x,y, col="red", cex=1, pch=19)
#adjustedr2
plot(best$adjr2, xlab="Number of Variables", ylab="Adj R2", type="l",font.lab=2)
x = which.max(best$adjr2)
y = best$adjr2[x]
points(x,y, col="red", cex=1, pch=19)
#cp
plot(best$cp, xlab="Number of Variables", ylab="Cp", type="l",font.lab=2)
x = which.min(best$cp)
y = best$cp[x]
points(x,y, col="red", cex=1, pch=19)
cat(names(coef(lm,6)))
pred   = coefi[1] + (x.test[, names(coefi[-1])]%*%coefi[-1])
round(mean((y.test - pred)^2))

#best_subset using 10-fold cross validation
predict.regsubsets = function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}
set.seed(26)
k=10
folds = sample(1:k, nrow(data.train),replace = TRUE)
cv.errors = matrix(NA,k,10, dimnames = list(NULL, paste(1:10)))
for (j in 1:k){
  best.fit = regsubsets(Y~. ,data=data.train[folds!=j,],nvmax = 10)
  for (i in 1:10){
    pred = predict.regsubsets(best.fit, data.train[folds==j,], id=i)
    cv.errors[j,i] = mean((data.train$Y[folds==j]-pred)^2)
  }
}

mean.cv.errors = apply(cv.errors, 2, mean)
plot(mean.cv.errors, type="b", main = "Mean CV errors",xlab = "Number of Predictors",
     ylab="Mean CV Errors")


y = min(mean.cv.errors)
x = which.min(mean.cv.errors)
points(x,y, col="red", cex=1, pch=19)

regfit.cv = regsubsets(Y~. , data = data.train , nvmax = 10)
coefi  = coef(regfit.cv,id=6)
pred   = coefi[1] + (x.test[, names(coefi[-1])]%*%coefi[-1])
coefi
cat(paste0("The test MSE for the best subsets model using CV is: ",round(mean((y.test - pred)^2),2))
)
