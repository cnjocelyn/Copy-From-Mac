##Ordinary least squares
library(faraway)
##read in the data
data(sat)
attach(sat)
df = data.frame(sat)
g1 <- lm (total~ takers+ratio+salary+expend, df)
summary(g1, cor= T)

##check for outlier-leverage points
ti <- rstudent(g1)
which.max(ti)
2*(1-pt(m,df = 50-4-1))
0.05/50
## Compute Cook’s distance
cook <- cooks.distance(g1)
halfnorm (cook, nlab=4, ylab = "Cook's distances")
## Compute changes in coefficients
result.inf <- lm.influence(g1)
plot(result.inf$coef[,2], result.inf$coef[,3], xlab="Change in takers", ylab="Change in ratio")
## interactive tool to identify points by clicking
identify (result.inf$coef[, 2], result.inf$coef[, 3])
##check for #29 point
result.29 <- lm(total~ takers+ratio+salary+expend, data = df,subset = (row.names(df) != row.names(df)[29]))
summary(g1)
summary(result.29)
##check for #44 point
result.44 <- lm(total~ takers+ratio+salary+expend, data = df,subset = (row.names(df) != row.names(df)[44] ))
summary(result.44)
##check for #48 point
result.48 <- lm(total~ takers+ratio+salary+expend, data = df,subset = (row.names(df) != row.names(df)[48] ))
summary(result.48)
##romove #29,#44,#48 points
df1 = df[-c(29,44,48),]
result <- lm(total~ takers+ratio+salary+expend, data = df1)
summary(result,cor= T)

##Least absolute deviations
library(quantreg)
glad <- rq(total~ takers+ratio+salary+expend, data = sat)
summary(glad)

##Huber’s robust regression 
library(MASS)
gr <- rlm(total~ takers+ratio+salary+expend, data = sat)
summary(gr)

##Least trimmed squares
set.seed(123)
glts <- ltsreg(total~ expend +ratio+salary+takers, data = sat)
x <- df [, 1:4]
bcoef <- matrix(0, nrow = 10000, ncol = 5)
for (i in 1:1000){
  newy <- glts$fit + glts$resid[sample(50, rep =T)]
  bcoef[i,] <- ltsreg(x, newy, nsamp = "best")$coef
}
colnames(bcoef) <- names(coef(glts))
apply(bcoef,2, function(x) quantile(x, c(0.025,0.975)))

library(ggplot2)
bcoef <- data.frame(bcoef)
p1 <- ggplot(bcoef, aes(x= takers))+geom_density()+xlim(-5,-2)
p1 + geom_vline(xintercept = c(-3.759,-3.083),linetype= "dashed")

