# Packages
install.packages("caret")
install.packages("magrittr")
install.packages("lmtest")
install.packages("e1071")
install.packages("popbio")
# Libraries
library("caret")
library("IDPmisc")
library("dplyr")
library("tidyr")
library("lmtest")
library("e1071")
library("popbio")
# Data
minerals <- read.csv("~/Downloads/minerals.csv")
head(minerals)

# Testing and correction for assumptions
## Sample size

mylogit <- glm(Gold~Antimony, data=minerals, family="binomial")

probabilities <- predict(mylogit, type="response")
minerals$Predicted <- ifelse(probabilities > .05, "pos", "neg")

minerals$PredictedR <- NA
minerals$PredictedR[minerals$Predicted == 'pos'] <- 1
minerals$PredictedR[minerals$Predicted == 'neg'] <- 0

minerals$PredictedR <- as.factor(minerals$PredictedR)
minerals$Gold <- as.factor(minerals$Gold)

confmatrix <- caret::confusionMatrix(minerals$PredictedR, minerals$Gold)
confmatrix
### We are given the prediction with two of the 4 that do not least have 1. 
### Therefore, we fail to meet the assumption for sample size. Proceeding with caution.

## Logit Linearity

minerals1 <- minerals %>% dplyr::select_if(is.numeric)
predictors <- colnames(minerals1)

minerals2 <- minerals1 %>% 
  mutate(logit=log(probabilities/(1-probabilities))) %>%
  gather(key= "predictors", value="predictor.value", -logit)

ggplot(minerals1, aes(logit, predictor.value)) +
  geom_point(size=.5, alpha =.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors, scales="free_y")



