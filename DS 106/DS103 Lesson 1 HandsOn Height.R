# Install packages

install.packages("car")
install.packages("caret")
install.packages("gvlma")
install.packages("predictmeans")
install.packages("e1071")
install.packages("lmtest")

# Load packages

library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")

# Load data

heights <- read.csv("~/Downloads/heights.csv")
head(heights)

# Test for linearity
scatter.smooth(x=heights$AM_Height, y = heights$PM_Height, main="AM vs PM Heights")
## Definitely looks linear. 

# Test for Homeoscedasticity 
lmMod <- lm(AM_Height~PM_Height, data=heights)
lmMod

par(mfrow=c(2,2))
plot(lmMod)
## Resid vs Leverage looks sus. Trying the Breush-Pagan test. 
lmtest::bptest(lmMod)
## This has a p-value of 0.748, which is not a big deal so it can be declared to be homoscedastic. 
## The assumption is met!

# Non-Constant Variance Test
car::ncvTest(lmMod)
## Not significant with a p > 0.05, so the assumption is met. 

# Test for Homogeneity of Variance
## From the graphs earlier, they are all well spread so the assumption has been met

gvlma(lmMod)
##Confirmed that all my assumptions were indeed met. 

# Looking for outliers: Cooks
## Outliers in x
CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
## 3, 4, and 12 might be outliers. 

## Leverage Values
lev = hat(model.matrix(lmMod))
plot(lev)

heights[lev>.2,]
 ## Only yielded 3, so that's what we will focus on

## Outliers in y
car::outlierTest(lmMod)
### gave a significant p value, so there is an outlier

# Outliers in x and y space
summary(influence.measures(lmMod))
## Gave outliers in 2, 3 an d11

# Create new model without outliers. 
heightsNoO <- heights[c(1,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
                        29,30,31,32,33,34,35,36,37,38,39,40,41),]
lmModNoO = lm(PM_Height~AM_Height, data=heightsNoO)

summary(lmMod)
### Looks like morning height is a significant predictor of evening height and explains 99% of the variance in evening height.

summary(lmModNoO)
### Very similar results with the model with no outliers, so it's fine to keep and use the original model with all the data!
        