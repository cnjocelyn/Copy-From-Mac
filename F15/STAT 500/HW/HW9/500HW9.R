library(faraway)
data(infmort)
attach(infmort)
##remove samples with missing data
df = na.omit(infmort)
df = df[, -4]
##transform the response
library(MASS)
g1 <- lm(mortality ~ income+ region+ income: region,df)
##find the outlier
cook <- cooks.distance(g1)
halfnorm(cook, nlab = 3,ylab = "Cook's distance")
##remove the ourliers and then use boxcox to find lambda
##df <- df[-c(25, 72,27),]
g1 <- lm(mortality ~ income+ region+ income: region,df)
boxcox(g1, data = df, plotit =T)
##transform the response
g2 <- lm(log(mortality)~ income + region+ income: region,df)
boxcox(g2, data = df,plotit =T,lambda = seq(-2, 4, by =0.05))
summary(g1)
summary(g2)
##The interaction term thorax: act ivity is not significant indicat- ing that we can fit the same slope within each group. 
##No further simplification is possible.
##transform the predictors
g4 <- lm(log(mortality)~ log(income) + region,df)
summary(g4)
##simply check
plot(df$income, df$mortality)
identify(df$income, df$mortality)
## [1] 25 27 72
##compute cook's distance to check influential points
plot(mortality ~income, df)
identify(income,mortality)
cook<-cooks.distance(g4)
halfnorm(cook,nlab=3,ylab="Cook's Distance")
##identify the influential points
df[c(72,25,27),]
##Canada libya Saudi_Arabia
ti<-rstudent(g4)
pt(ti[72],df=101-5-1)
##Compute the p-value and compare with alpha/n
2*(1-pt(ti[72],df=101-5-1))-0.05/101
##Afganistan 0.00573199  not outlier
pt(ti[25],df=101-5-1)
2*(1-pt(ti[25],df=101-5-1))-0.05/101
##Libya 0.001954266 is not an outlier
pt(ti[27],df=101-5-1)
2*(1-pt(ti[27],df=101-5-1))-0.05/101
##Saudi_Arabia -0.0004949912 not outlier
##remove 27th point and refit
df <- df[-27,]
g4 <- lm(log(mortality)~ log(income) + region,df)

##standard analysis of covariance
g_cov <- lm(log(mortality)~ log(income)*region,df)
anova(g_cov)
plot(g4$fitted.values,g4$residuals,xlab="Fitted",ylab="Residuals",main="")
abline(h=0)
qqnorm(g4$residual,ylab="Residuals")
qqline(g4$residual)
halfnorm(lm.influence(g4)$hat,nlab=2,ylab="Leverages")
plot(g4$residuals/((summary(g4)$sig)*sqrt(1-lm.influence(g4)$hat)), g4$residuals,xlab="Studentized Residuals",ylab="Raw Residuals")





