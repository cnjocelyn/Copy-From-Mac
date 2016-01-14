library(faraway)
data(fat)
attach(fat)
index <- seq(10, 250, by=10)
## Extract data and remove ‘‘brozek’’, ‘‘density’’ and ‘‘free’’
train <- fat[-index, -c(1, 3, 8)]
test <- fat[index, -c(1, 3, 8)]
##Linear regression with all predictors
model1 <- lm(siri~ ., data = train)
summary(model1)
rmse <- function(x,y) sqrt (mean((x-y)^2))
rmse(model1$fitted.values,train$siri)
rmse(predict(model1, test),test$siri)
##variables selected using AIC
model2 <- step(model1)
summary(model2)
rmse(model2$fitted.values, train$siri)
rmse(predict(model2,test), test$siri)
##PCA
library(tools)
library(HSAUR2)
library(MVA)
fatpca <- prcomp(train[,-1],scale.=TRUE)
round(fatpca$sdev, 3)
##matplot(1:14, fatpca$rotation[,1:7],type= "l", xlab = "variables", ylab = "")
##Use cross validation for choosing number of PCs
library(pls)
set.seed(123)
modpcr1 <- pcr(siri~ .,data= train,ncomp =14,validation = "CV",segments = 5,scale.=TRUE)
rmsCV = RMSEP(modpcr1, estimate = 'CV')
which.min(rmsCV$val)
##plot the RMSE
plot(rmsCV$val, xlab = "No. of PC", ylab = "CV RMS")
##compare the RMSE on training data and test data
rmse(modpcr1$fitted.values[,,7],train$siri)
yfit <- predict(modpcr1, newdata = test, ncomp =7)
rmse(yfit,test$siri)
plot(modpcr1$fitted.values[,,7], modpcr1$fitted.values[,,7]-train$siri, xlab = "fitted value", ylab = "residuals")
plot(modpcr1$fitted.values[,,7], abs(modpcr1$fitted.values[,,7]-train$siri), xlab = "fitted value", ylab = "|residuals|")


#modpcr2 <- pcr(siri~. , data =train, ncomp = 14)
#rmsfat <- NULL
#for (k in 1:14) {
 #pv <- predict(modpcr2, newdata = test, ncomp =k)
  #rmsfat[k] = rmse(pv, test$siri)}
#plot(rmsfat, xlab = "PC number" , ylab = "TEST RMS")

##Partial least squares
set.seed(123)
modpls <- plsr(siri~., data =train, ncomp =14,validation= "CV")
pls_rmsCV = RMSEP(modpls, estimate = 'CV')
which.min(pls_rmsCV$val)
##plot the RMSE
plot(pls_rmsCV$val, xlab = "No. of PC", ylab = "CV RMS")
##compare the RMSE on training data and test data
rmse(modpls$fitted.values[,,4],train$siri)
ypred <- predict(modpls, newdata = test)
rmse(test$siri, ypred[,,4])
plot(modpls$fitted.values[,,4], modpls$fitted.values[,,4]-train$siri, xlab = "fitted value", ylab = "residuals")
plot(modpls$fitted.values[,,4], abs(modpls$fitted.values[,,4]-train$siri), xlab = "fitted value", ylab = "|residuals|")

##Redge regression
library(MASS)
##center the training data
modridge <- lm.ridge(siri~., lambda = seq(0,10,0.1), data =train)
matplot(modridge$lambda, t(modridge$coef), type = "l", lty = 1, xlab = expression(lambda), ylab = expression(hat(beta)))
##select lambda
select(modridge)
abline(v = 1.1)
which.min(modridge$GCV)
##compute the fitted the value
yfit_rid <- modridge$ym + scale(train[,-1], center = modridge$xm, scale = modridge$scales)%*% modridge$coef[,12]
rmse(yfit_rid ,train$siri)
##compare to the sample 
ypred_rid <- modridge$ym + scale(test[,-1], center = modridge$xm, scale = modridge$scales)%*% modridge$coef[,12]
rmse(ypred_rid ,test$siri)

