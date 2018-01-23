Diabetes = read.table("diabetes.data.txt", header=TRUE)
Diabetes$SEX = as.factor(Diabetes$SEX) 
summary(Diabetes) 
attach(Diabetes) 
anyNA(Diabetes) 
SEX = as.factor(SEX) 

 
 
 
 
#Exploratory Data Analysis 
install.packages("corrplot") 
library(corrplot) 
cormax=cor(Diabetes[-2]) 
corrplot(cormax, method="circle",tl.col="black") 
boxplot(BMI,AGE, BP, S3, S6, col="royalblue2", names=c("BMI", "AGE", "BP", "S3", "S6")) 
boxplot(S1,S2, col="royalblue2", names=c("S1", "S2")) 
boxplot(S4,S5, col="royalblue2", names=c("S4", "S5")) 
plot(SEX, col=c("royalblue2", "violet"), xlab="SEX",font.axis=4) 
 
diabetes=read.table("diabetes.data.txt",header=TRUE) 

#2. Data Modeling 
summary(diabetes) 
diabetes$SEX=as.factor(diabetes$SEX) 
set.seed(26) 
n=nrow(diabetes) 
 
test <- sample(n, round(n/4)) 
data.train <- diabetes[-test,] 
data.test <- diabetes[test,] 
x <- model.matrix(Y~ ., data = diabetes)[,-1] 
 
x.train <- x[-test,] 
x.test <- x[test,] 
y <- diabetes$Y 
y.train <- y[-test] 
y.test <- y[test] 
 
#OLS 
ols.lm=lm(Y~.,data.train) 
round(coef(summary(lm(Y~., data = data.train))),2) 
coefi  = coef(ols.lm,id=10) 
lm.pred   = coefi[1] + (x.test[, names(coefi[-1])]%*%coefi[-1]) 
cat(paste0("The test MSE for the OLS model is: ",round(mean((y.test - lm.pred)^2),2))) 
summary(ols.lm) 
 
#stepwise regression
S12=(scale(diabetes$S1)+scale(diabetes$S2))/2 
S34=(scale(diabetes$S3)+scale(diabetes$S4))/2 
drops=c("S1","S2","S3","S4") 
DS=diabetes[ , !(names(diabetes) %in% drops)] 
DS=data.frame(DS,S12,S34) 
DS.train <- DS[-test,] 

 
 
DS.test <- DS[test,] 
x <- model.matrix(Y~ ., data = DS)[,-1] 
 
x.train <- x[-test,] 
x.test <- x[test,] 
y <- DS$Y 
y.train <- y[-test] 
regfit.full = lm(Y~., data = DS.train) 
step <- stepAIC(regfit.full, direction="both") 
regfit.full = lm(Y~.-(S12+S6+AGE), data = DS.train) 
round(coef(summary(lm(Y~.-(S12+S6+AGE), data = DS.train))),3) 
coefi  = coef(regfit.full,id=5) 
sr.pred   = coefi[1] + (x.test[, names(coefi[-1])]%*%coefi[-1]) 
cat(paste0("The test MSE for Step-wise regression model is: ",round(mean((y.test - sr.pred)^2),2))) 
 
#best_subset with 10 fold 
predict.regsubsets = function(object, newdata, id,...){  form=as.formula(object$call[[2]])  
mat = model.matrix(form, newdata)  
coefi = coef(object, id=id)  
xvars = names(coefi)  
mat[,xvars]%*%coefi } 
 
k=10 
set.seed(26) 
folds = sample(1:k, nrow(DS.train),replace = TRUE) 
cv.errors = matrix(NA,k,8, dimnames = list(NULL, paste(1:8))) 
 
for (j in 1:k){  
  best.fit = regsubsets(Y~. ,data=DS.train[folds!=j,],nvmax = 8)  
  for (i in 1:8){  
  k.pred = predict.regsubsets(best.fit, DS.train[folds==j,], id=i)  
  cv.errors[j,i] = mean((DS.train$Y[folds==j]-k.pred)^2)  
} } 
 
mean.cv.errors = apply(cv.errors, 2, mean) 
plot(mean.cv.errors, type="b", main = "Mean CV errors",xlab = "Number of Predictors",  ylab="Mean CV Errors") 
 
y = min(mean.cv.errors) 
x = which.min(mean.cv.errors) 
 

 
 
points(x,y, col="red", cex=1, pch=19) 
 
regfit.cv = regsubsets(Y~. , data = DS.train , nvmax = 8) 
coefi  = coef(regfit.cv,id=7) 
cv.pred   = coefi[1] + (x.test[, names(coefi[-1])]%*%coefi[-1]) 
coefi 
cat(paste0("The test MSE for the best subsets model using CV is: ",round(mean((y.test - cv.pred)^2),2))) 
 
#ridge regression 
library(glmnet) 
grid = 10^seq(10,-2,length=100) 
set.seed(26) 
cv.out = cv.glmnet(x.train, y.train, alpha = 0) 
plot(cv.out) 
largelam = cv.out$lambda.min 
ridge.mod = glmnet(x.train, y.train, alpha = 0, lambda = grid, thresh = 1e-12) 
ridge.pred = predict(ridge.mod, s=largelam, newx=x.test)
cat(paste0("The test MSE for the ridge regression model using CV is: ",round(mean((y.test - ridge.pred)^2),2))) 
coef = glmnet(x.train, y.train, alpha = 0, lambda = largelam, thresh = 1e-12)$beta 
matrix(coef, dimnames = list(row.names(coef), c("Coefficient"))) 
 
#lasso 
set.seed(26) 
cv.out = cv.glmnet(x.train, y.train, alpha = 1) 
plot(cv.out) 
largelam = cv.out$lambda.min 
lasso.mod = glmnet(x.train, y.train, alpha = 1, lambda = largelam) 
lasso.pred = predict(lasso.mod, s=largelam, newx = x.test) 
cat(paste0("The test MSE for the lasso model using CV is: ",round(mean((y.test - lasso.pred)^2),2))) 
 
coef= glmnet(x.train, y.train, alpha = 1, lambda = largelam)$beta 
matrix(coef, dimnames = list(row.names(coef), c("Coefficient"))) 
 
#pcr 
library(pls) 
set.seed (26) 
pcr.fit=pcr(data.train$Y~., data=data.train,scale=TRUE ,validation ="CV") 
validationplot(pcr.fit ,val.type="MSEP") 
summary(pcr.fit) 
pcr.pred=predict(pcr.fit ,x.test, ncomp =8) 
mean((pcr.pred -y.test)^2) 
 

 
 
pcr.fit=pcr(y~x,scale =TRUE ,ncomp=8) 
summary(pcr.fit) 
 
#pls 
set.seed(26) 
pls.fit=plsr(Y~., data=data.train,scale=TRUE ,validation ="CV") 
summary (pls.fit ) 
validationplot(pls.fit ,val.type="MSEP") 
pls.pred=predict (pls.fit ,x.test, ncomp =3) 
mean((pls.pred -y.test)^2) 
pls.fit=plsr(Y~., data=diabetes,scale=TRUE ,ncomp =3) 
summary(pls.fit) 
 
#comparison 
test.avg = mean(data.test[, "Y"]) 
lm.test = 1 - mean((data.test[, "Y"] - lm.pred)^2) /mean((data.test[, "Y"] - test.avg)^2) 
ridge.test = 1 - mean((data.test[, "Y"] - ridge.pred)^2) /mean((data.test[, "Y"] - test.avg)^2) 
lasso.test = 1 - mean((data.test[, "Y"]- lasso.pred)^2) /mean((data.test[, "Y"] - test.avg)^2) 
pcr.test = 1 - mean((data.test[, "Y"] - data.frame(pcr.pred))^2) /mean((data.test[, "Y"] - test.avg)^2) 
pls.test = 1 - mean((data.test[, "Y"] - data.frame(pls.pred))^2) /mean((data.test[, "Y"] - test.avg)^2) 
barplot(c(lm.test, ridge.test, lasso.test, pcr.test, pls.test), col="purple", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared") 

