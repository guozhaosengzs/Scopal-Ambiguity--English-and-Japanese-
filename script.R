## Use libraries
library(tidyverse)
library(lme4)
library(lmerTest)

###### Prepare data

### Import all the data
GLenEng <- read_csv("data\\GestureLengthEnglish.csv")
GLenJap <- read_csv("data\\GestureLengthJapanese.csv")
SLenEng <- read_csv("data\\SentenceLengthEnglish.csv")
SLenJap <- read_csv("data\\SentenceLengthJapanese.csv")


### Clean column names, check each of their type and change them if needed

## 2 of Length data sets
uniformed.colnames <- c("subject", "sentence", "context", "sentence_length", "maxF0", "meanF0", "keyword")
colnames(SLenEng) <- uniformed.colnames
colnames(SLenJap) <- uniformed.colnames

sapply(SLenEng, class)
sapply(SLenJap, class)

cols  <- c("subject", "sentence", "context", "keyword")
SLenEng[cols] <- lapply(SLenEng[cols], as.factor)
SLenJap[cols] <- lapply(SLenJap[cols], as.factor)

str(SLenEng)
str(SLenJap)



###### Run Models

### Mixed Effect for Length data sets, using "lmer" function as the dependent variable is continuous

## English Sentence Length 

# Sentence Length as R.V.

sapply(SLenEng, levels)

#Random Slope and Random Intercept
model.EngSent.Len <- lmer(sentence_length ~ context  + (1|keyword)  + (1|subject), data = SLenEng)

summary(model.EngSent.Len)

model.EngSent.maxF0 <- lmer(maxF0 ~ context  + (1|keyword)  + (1|subject), data = SLenEng)

summary(model.EngSent.maxF0)

model.EngSent.meanF0 <- lmer(meanF0 ~ context  + (1|keyword)  + (1|subject), data = SLenEng)

summary(model.EngSent.meanF0)

model.EngSent.meanF0 <- lmer(meanF0 ~ (1|keyword) + (1|subject) + context, data = SLenEng)






