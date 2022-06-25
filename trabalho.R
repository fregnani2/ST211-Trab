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
names(reg.summary )


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
regfit.fwd=regsubsets(medv~.,train,nvmax=13,method="forward")
regfit.fwd.summary = summary(regfit.fwd)

regfit.bwd=regsubsets(medv~.,train,nvmax=13,method="backward")
summary(regfit.bwd)

rmax.fwd = which.max(regfit.fwd.summary$adjr2)
plot(regfit.fwd.summary$adjr2 ,xlab =" Number of Variables ",ylab="R²fwd", type="l")
points (rmax.fwd,regfit.fwd.summary$adjr2[rmax.fwd],col = "red")

cpmin.fwd = which.min(regfit.fwd.summary$cp)
plot(regfit.fwd.summary$cp ,xlab =" Number of Variables ",ylab="Cpfwd", type="l")
points (cpmin.fwd,regfit.fwd.summary$cp[cpmin.fwd],col = "red")

bicmin.fwd = which.min(regfit.fwd.summary$bic)
plot(regfit.fwd.summary$bic ,xlab =" Number of Variables ",ylab="BICfwd", type="l")
points (bicmin.fwd,regfit.fwd.summary$bic[bicmin.fwd],col = "red")
