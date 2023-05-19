
# Libraries
library("caret")
library("IDPmisc")
library("dplyr")
library("tidyr")
library("lmtest")
library("e1071")
library("popbio")
# Data
Minerals <- read.csv("~/Downloads/minerals.csv")
head(Minerals)

# Testing and correction for assumptions
## Sample size

mylogit1 <- glm(Gold~Antimony, data=Minerals, family="binomial")

probabilities <- predict(mylogit1, type="response")
Minerals$Predicted1 <- ifelse(probabilities > .05, "pos", "neg")

Minerals$Predicted1R <- NA
Minerals$Predicted1R[Minerals$Predicted1 == 'pos'] <- 1
Minerals$Predicted1R[Minerals$Predicted1 == 'neg'] <- 0

Minerals$Predicted1R <- as.factor(Minerals$Predicted1R)
Minerals$Goldf <- as.factor(Minerals$Gold)

confmatrix <- caret::confusionMatrix(Minerals$Predicted1R, Minerals$Goldf)
confmatrix
### We are given the prediction with two of the 4 that do not least have 1. 
### Therefore, we fail to meet the assumption for sample size. Proceeding with caution.

## Logit Linearity

Minerals1 <- Minerals %>% dplyr::select_if(is.numeric)
predictors <- colnames(Minerals1)

Minerals2 <- Minerals1 %>% 
  mutate(logit=log(probabilities/(1-probabilities))) %>%
  gather(key= "predictors", value="predictor.value", -logit)

ggplot(Minerals2, aes(logit, predictor.value)) +
  geom_point(size=.5, alpha =.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors, scales="free_y")

### The graph for sure looks linear

## Multicollinearity - only test if we have multiple IV's, which we don't

## Independent Errors
plot(mylogit1$residuals)
### Looks like it follows in a nice line. 
dwtest(mylogit1, alternative = "two.sided")
### pvalue is >.05 so it meets this assumption, so it looks good also with a DW > 1

## Screen for outliers
infl <- influence.measures(mylogit1)
summary(infl)

### dfb.1 and dffit are all < 1, and hat is all <.3 

# Examine Output

summary(mylogit1)

### According to the coefficients, with a significant pvalue, 
### we can see Antimony IS a good predictor for gold. 

# Graph it
logi.hist.plot(Minerals$Antimony, Minerals$Gold, logi.mod =1, boxp=FALSE, type = "hist", col = "gray")


### This graph shows the results where most of the probabilities increase, which matches
### my previous conclusion that Antimony IS a good predictor of where Gold is. 

