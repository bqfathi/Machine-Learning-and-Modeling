IQ <- read.csv("~/Downloads/IQ.csv")

head(IQ)

FitAll = lm(IQ~., data=IQ)
summary(FitAll)
## none of the tests have a good pvalue. Not a good model. Multicolinearity et al. 

# Part I Backwards elemination
step(FitAll, direction = "backward")

### Resulted in IQ ~ Test1 + Test2 + Test4 with an AIC of 71.69 
FitSomeB <- lm(IQ ~ Test1 + Test2 + Test4, data = IQ)
summary(FitSomeB)
###Looks like perhaps Test4 is a good fit according to the p-value, but barely

# Part II Forward Selection
#Backward Selection
FitAll1 = lm(Y ~ ., data = stepwiseRegression)
summary(FitAll1)

step(FitAll1, direction = 'backward', scope = formula(FitAll1))

#Forward Selection
fitstart = lm(Y ~ 1, data = stepwiseRegression)
summary(fitstart)

step(fitstart, direction = 'forward', scope = (formula(FitAll1)))

#StepWise 
step(fitstart, direction = 'both', scope = formula(FitAll1))

fitsome <- lm(formula = Y ~ X2 + X4 + X6 + X10 + X11 + X12, data = stepwiseRegression)
summary(fitsome)

#Compare the output from the different types of stepwise selections above and note the order of the elimination of features


