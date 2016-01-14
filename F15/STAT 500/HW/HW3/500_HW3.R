##Load the library
library (faraway)
##read in and check out the data 
data (sat)
attach (sat)
sat

##problem 1
model1 <- lm(total~ takers+ ratio+ salary, sat)
summary(model1)

##model h0_1 under H0:βsalary = 0
h0_1 <- lm(total~ takers+ ratio, sat)
summary(h0)
##model1 is under H0 U HA
anova(h0_1, model1)

##test H0:βtakers = βratio = βsalary = 0
##anova(model1)
summary(model1)

##problem 2
conf <- confint(model1,level = 0.95)
conf
conf <- confint(model1,level = 0.99)
conf

##problem 3
library(ellipse)
plot(ellipse(model1, c(3,4)), type = "l", xlim=c(-10, 1))
points(0,0)
points(coef(model1)[3], coef(model1)[4], pch=18)
abline (v=confint(model1, level = 0.95)[3,], lty=2)
abline (h=confint(model1, level = 0.95)[4,], lty=2)

##problem 4
model2 <- lm(total~ takers+ ratio+ salary+ expend, sat)
summary(model2)

##problem 5
h0_2 <- lm(total~ takers, sat)
anova(h0_2, model2)
