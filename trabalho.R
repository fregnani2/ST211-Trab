library(MASS)
library(leaps)
library (ISLR)
df <- data.frame(Boston)

smp_size <- floor((0.19762845849802371541) * nrow(df))
set.seed(123)
test_ind <- sample(seq_len(nrow(df)), size = smp_size)

test <- Boston[test_ind, ]
train <- Boston[-test_ind, ]

regfit.full=regsubsets(medv~.,train,nvmax=13)

reg.summary = summary (regfit.full)


par(mfrow =c(2,2))

rmax = which.max(reg.summary$adjr2)
cpmin = which.min(reg.summary$cp)
bicmin = which.min(reg.summary$bic)

plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp", type="l")
points (cpmin,reg.summary$cp[cpmin],col = "red")

plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab="R²", type="l")
points (rmax,reg.summary$adjr2[rmax],col = "red")

plot(reg.summary$bic ,xlab =" Number of Variables ",ylab="BIC", type="l")
points (bicmin,reg.summary$bic[bicmin],col = "red")

coef(regfit.full,11)

#B)
regfit.fwd = regsubsets(medv~.,train ,nvmax = 13,method ="forward")
regfit.fwd.summary = summary(regfit.fwd)

regfit.bwd=regsubsets(medv~.,train,nvmax=13,method="backward")
bwd.summary = summary(regfit.bwd)

#resultados iguais a A
coef(regfit.bwd,11) 
coef(regfit.fwd,11)

#C)
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(df),rep=TRUE)
test=(!train)
regfit.best=regsubsets(medv~.,data=df[train,],nvmax=13)
test.mat=model.matrix(medv~.,data=df[test,])
val.errors=rep(NA,13)
for(i in 1:13){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((df$medv[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,11)#Melhor modelo tem 11 variáveis, sem o indus e o medv

predict.regsubsets=function(object,newdata,id,...){#método predict mais fácil
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(medv~.,data=df,nvmax=13)
coef(regfit.best,11)

k=11
set.seed(1)
folds=sample(1:k,nrow(df),replace=TRUE)
cv.errors=matrix(NA,k,13, dimnames=list(NULL, paste(1:13)))
for(j in 1:k){
  best.fit=regsubsets(medv~.,data=df[folds!=j,],nvmax=13)
  for(i in 1:13){
    pred=predict.regsubsets(best.fit,df[folds==j,],id=i)
    cv.errors[j,i]=mean( (df$medv[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
which.min(mean.cv.errors)
reg.best=regsubsets(medv~.,data=df, nvmax=13)
coef(reg.best,11)
                     


#D
test <- Boston[test_ind, ]
train <- Boston[-test_ind, ]
regfit.test = regsubsets(medv~.,test,nvmax=13)

test.summary = summary (regfit.test)

par(mfrow =c(2,2))

rmax.test = which.max(test.summary$adjr2)
cpmin.test = which.min(test.summary$cp)
bicmin.test = which.min(test.summary$bic)

plot(test.summary$cp ,xlab =" Number of Variables ",ylab="CpTest", type="l")
points (cpmin.test,test.summary$cp[cpmin.test],col = "red")

plot(test.summary$adjr2,xlab =" Number of Variables ",ylab="R² test", type="l")
points (rmax.test,test.summary$adjr2[rmax.test],col = "red")

plot(test.summary$bic,xlab =" Number of Variables ",ylab="BIC test", type="l")
points (bicmin.test,test.summary$bic[bicmin.test],col = "red")

coef(regfit.test,8)

#D.B
regfit.test.fwd = regsubsets(medv~.,test,nvmax = 13,method ="forward")
regfit.test.fwd.summary = summary(regfit.test.fwd)

regfit.test.bwd=regsubsets(medv~.,test,nvmax=13,method="backward")
bwd.test.summary = summary(regfit.test.bwd)

coef(regfit.test.bwd,8) 
coef(regfit.test.fwd,8)

#D.C
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(df),rep=TRUE)
test=(!train)
regfit.best=regsubsets(medv~.,data=df[train,],nvmax=13)
test.mat=model.matrix(medv~.,data=df[test,])
val.errors=rep(NA,13)
for(i in 1:13){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((df$medv[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,8)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(medv~.,data=df,nvmax=13)
coef(regfit.best,8)

k=8
set.seed(1)
folds=sample(1:k,nrow(df),replace=TRUE)
cv.errors=matrix(NA,k,13, dimnames=list(NULL, paste(1:13)))
for(j in 1:k){
  best.fit=regsubsets(medv~.,data=df[folds!=j,],nvmax=13)
  for(i in 1:13){
    pred=predict.regsubsets(best.fit,df[folds==j,],id=i)
    cv.errors[j,i]=mean( (df$medv[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
which.min(mean.cv.errors)
reg.best=regsubsets(medv~.,data=df, nvmax=13)
coef(reg.best,8)
