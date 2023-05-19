#  Libraries
library("tidyr")
library("ggplot2")
library("dplyr")

# Data
nonlinear <- read.csv("~/Downloads/nonlinear.csv")
head(nonlinear)

# Graph 

## X1 to Y1 Quadratic? 
quadplot <- ggplot(nonlinear, aes(x = X1, y = Y1))+
  geom_point() + stat_smooth(method="lm", formula = y ~x + I(x^2), size = 1)
quadplot
### Looks like it is quadratic to me... 
### Modeling it
X1sq <- nonlinear$X1^2

quadModel <- lm(nonlinear$Y1~nonlinear$X1+X1sq)
summary(quadModel)

#### Looking at the R-squared values (.9863), along with the F-statistic 1221 shown at the bottom 
#### and associated with the p-value, this quadratic model is significant! 
#### This means X1 is a significant quadratic predoctor of Y1

## X2 to Y2 Quadratic?
quadplot2 <- ggplot(nonlinear, aes(x - X2, y = Y2))+
  geom_point() + stat_smooth(method="lm", formula = y ~x + I(x^2), size = 1)
quadplot2
### Also looks quadratic to me, though it looks like exponential could work as well
### Modeling it

X2sq <- nonlinear$X2^2

quadModel2 <- lm(nonlinear$Y2~nonlinear$X2+X2sq)
summary(quadModel2)
#### Looking at the R-squared values(.947), along with the F-statistic 341 shown at the bottom 
#### and associated with the p-value, this quadratic model is significant! 
#### This means X2 is a significant quadratic predoctor of Y2

## But trying exponential wouldn't hurt. 
expMod <- lm(log(nonlinear$Y2)~nonlinear$X2)
summary(expMod)

#### This has a lower R-squared (.9061) BUT the F-statistic 367.6 is higher than the quadratic version
#### Therefore I say X2 and Y2 fit an exponential relationship bettern (but not by much, IMO)



