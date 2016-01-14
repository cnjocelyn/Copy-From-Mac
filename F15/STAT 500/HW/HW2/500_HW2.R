##Load the library
library (faraway)
##read in and check out the data 
data (uswages)
attach (uswages)
uswages

##remome negative values of experience variable
uswages$exper[uswages$exper < 0] <- NA

## use the lm() function for regression
reg <- lm ( wage ~ educ + exper, data = uswages)
summary(reg)

## extract other needed quantities by examining the model object and its summary
regs <- summary(reg)
names (regs)
## extract R2
regs $ r.squared

## find the No. of case which has the largest residual
## which.max (regs $ residuals)
rsd <- as.vector(regs$residuals)
which.max (rsd)
##res <- residuals(reg)
##which.max(res)

## Compute the mean and median of the residuals
mean(rsd)
median(rsd)

##the correlation of the residuals with the fitted values
cor ( rsd, reg$fitted.values)
plot(reg$fitted.values, rsd, main="correlation of the residuals with the fitted values",xlim=c(80,1200), ylim=c(-1050,2250),xlab="Fitted Values",ylab="Residuals")

## log (wages) as the response
log_wage <-log (uswages$wage)
log_reg <- lm ( log_wage ~ educ + exper, data = uswages)
summary(log_reg)









