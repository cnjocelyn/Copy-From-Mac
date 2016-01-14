library(ElemStatLearn)
data("prostate")
attach(prostate)
## the full model
g = lm(lpsa ~ ., data = prostate)
summary(g)
##method1: backward elimination
g1 = update(g,. ~ .-train)
summary(g)
##continue dropping
g1 = update(g1,. ~ .-gleason)
summary(g1)
g1 = update(g1,. ~ .-lcp)
summary(g1)
g1 = update(g1,. ~ .-pgg45)
summary(g1)
g1 = update(g1,. ~ .-age)
summary(g1)
g1 = update(g1,. ~ .-lbph)
summary(g1)
## other predictors can also made an optimal fit
summary(lm(lpsa ~ lcavol+ svi + lbph))

## method 2:AIC
step(g)

## method 3:Adjusted R2
library(leaps)
g3 = regsubsets(lpsa ~., data = prostate)
summary(g3)
##plot adjusted R2 against p+1
rs =  summary(g3)
plot(2:9, rs$adjr2, xlab = "No. of Parameters", ylab = "Adjusted Rsq")
##select model with largest adjusted R2
which.max(rs$adjr2)

##method 4: Mallow Cp
which.min(rs$cp)
plot(2:9, rs$cp,xlab = "No. Parameters", ylab = "Cp")
abline(0,1)

