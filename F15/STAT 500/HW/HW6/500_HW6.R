library(MASS)
data(trees)
attach(trees)
#part a
g1 <-  lm(Volume~ Girth +  Height)
boxcox(g1, plotit =T)
boxcox(g1, lambda = seq(0.0, 0.6, by =0.05))
g2 <- lm(I(Volume^0.3)~Girth + Height)
boxcox(g2, plotit =T,lambda = seq(0.0, 2, by =0.05))
summary(g1)
summary(g2)

#part b
g3 <- lm(Volume~ Girth+Height+I(Girth*Height)+I(Girth^2)+I(Height^2))
summary(g3)
##remove Height^2
g4 <- lm(Volume~ Girth+Height+I(Girth*Height)+I(Girth^2))
summary(g4)
##remove Girth^2
g5 <- lm(Volume~ Girth+Height+I(Girth*Height))
summary(g5)

#part c
g6 <- lm(I(Volume^0.3)~ Girth+Height+I(Girth*Height)+I(Girth^2)+I(Height^2))
summary(g6)
## remove Girth^2
g7 <- lm(I(Volume^0.3)~ Girth+Height+I(Girth*Height)+I(Height^2))
summary(g7)
##remove Height^2
g8 <- lm(I(Volume^0.3)~ Girth+Height+I(Girth*Height))
summary(g8)
##remove Girth*Height
g9 <- lm(I(Volume^0.3)~ Girth+Height)
summary(g9)