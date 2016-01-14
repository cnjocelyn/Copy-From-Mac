##Load the library
library (faraway)
##read in and check out the data 
data (teengamb)
attach (teengamb)
View(teengamb)

##problem 1
##plot residuals vs fitted values
result1 <- lm(gamble ~ sex+ status+ income+ verbal)
par(mfrow = c(1,2))
plot(result1$fitted, result1$residual, xlab= "Fitted", ylab= "Residuals")
abline(h = 0)
##plot absolute values of residuals vs fitted values
plot(result1$fitted, abs(result1$residual), xlab= "Fitted", ylab= "|Residuals|")
summary(lm(abs(result1$residual) ~ result1$fitted ))
##square root transformation of y
result2 <- lm(sqrt(gamble) ~ sex+ status+ income+ verbal)
plot(result2$fitted, result2$residual, xlab= "Fitted", ylab= "Residuals")
abline(h = 0)
plot(result2$fitted, abs(result2$residual), xlab= "Fitted", ylab= "|Residuals|")
summary(lm(abs(result2$residual) ~ result2$fitted ))

##problem 2
qqnorm (residuals (result2), ylab="Residuals")
qqline (residuals (result2))

##problem 3
par(mfrow = c(1,2))
halfnorm(lm.influence(result2)$hat, nlab = 2, ylab = "Leverages")
teengamb[c(35, 42), ]

##problem 4
## compute (externally studentized residuals)
ti <- rstudent(result2)
max(abs(ti))
which(ti == max(abs(ti)))
## compute p-value 
2*(1-pt(max(abs(ti)),df = 47-5-1))
## compare to alpha/n
0.05/47

##problem 5
## Compute Cook’s distance
cook <- cooks.distance(result2)
halfnorm (cook, nlab=4, ylab = "Cook's distances")
## Compute changes in coefficients
result.inf <- lm.influence(result2)
plot(result.inf$coef[,2], result.inf$coef[,3], xlab="Change in sex", ylab="Change in status", xlim=c(-0.4, 0.48), ylim=c(-0.015, 0.018))
## interactive tool to identify points by clicking
identify (result.inf$coef[, 2], result.inf$coef[, 3])

##check for #24 point
result.24 <- lm(sqrt(gamble) ~ sex+ status+ income+ verbal, data = teengamb,subset = (row.names(teengamb) !="24" ))
summary(result2)
summary(result.24)
##check for #5 point
result.5 <- lm(sqrt(gamble) ~ sex+ status+ income+ verbal, data = teengamb,subset = (row.names(teengamb) !="5" ))
summary(result.5)