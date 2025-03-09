setwd("/Users/seeyanyu/Desktop/SIM - UOL (2)/ST 3189 (Machine Learning)/Coursework/Part 2")
data1 <- read.csv("student-mat.csv", sep = ";", stringsAsFactors = T)
summary(data1)
data2 <- data1[,-31]
data2 <- data2[,-31]
summary(data2)

### Factoring Variables
data2$Medu <- factor(data2$Medu)
data2$Fedu <- factor(data2$Fedu)
data2$traveltime <- factor(data2$traveltime)
data2$studytime <- factor(data2$studytime)
data2$failures <- factor(data2$failures)
data2$famrel <- factor(data2$famrel)
data2$freetime <- factor(data2$freetime)
data2$goout <- factor(data2$goout)
data2$Dalc <- factor(data2$Dalc)
data2$Walc <- factor(data2$Walc)
data2$health <- factor(data2$health)
summary(data2)

### Models (Full model)
library(caTools)
set.seed(2021)

m.full <- lm(G3 ~., data=data2)
summary(m.full)

train <- sample.split(Y=data2$G3, SplitRatio = 0.7)
trainset.full <- subset(data2, train ==T)
testset.full <- subset(data2, train ==F)

m.full.1 <- lm(G3 ~., data=trainset.full)
RMSE.mfull.train <- sqrt(mean(residuals(m.full.1)^2)) 
predict.mfull.test <- predict(m.full.1, newdata = testset.full)
testset.error <- testset.full$G3  - predict.mfull.test
RMSE.mfull.test <- sqrt(mean(testset.error^2)) 

round(RMSE.mfull.train,5) #3.41654
round(RMSE.mfull.test,5) #5.10041


### Models (Forward Selection) ------------------------------------------------
set.seed(2021)

m.null <- lm(G3 ~ 1, data = data2)
summary(m.null)
biggest <- formula(lm(G3 ~., data = data2))
m.for <- step(m.null, direction = "forward", scope = biggest)
summary(m.for)
plot(m.for)

# Train-Test Split
train <- sample.split(Y=data2$G3, SplitRatio = 0.7)
trainset.step <- subset(data2, train ==T)
testset.step <- subset(data2, train ==F)

mfor.1 <- lm(G3 ~ failures + Mjob + sex + goout + freetime + Medu + romantic + absences + famsup + higher + famsize + studytime, data = trainset.step)
plot(mfor.1)
RMSE.mfor.train <- sqrt(mean(residuals(mfor.1)^2))
predict.mfor.test <- predict(mfor.1, newdata = testset.step)
testset.error <- testset.step$G3  - predict.mfor.test
RMSE.mfor.test <- sqrt(mean(testset.error^2))
round(RMSE.mfor.train,5) #3.68985
round(RMSE.mfor.test,5) #4.54301

# 5-fold CV
set.seed(2021)
n <- nrow(data2)
folds <- 5

RMSE.fold1.train <- seq(1:folds)
RMSE.fold1.test <- seq(1:folds)

testlist <- split(sample(1:n), 1:folds)

for (i in 1:folds){
  trainset = data2[-testlist[[i]],]
  testset = data2[testlist[[i]],]
  
  mfold1.train <- lm(G3 ~ failures + Mjob + sex + goout + freetime + Medu + romantic + absences + famsup + higher + famsize + studytime, data = trainset.step)
  RMSE.fold1.train[i] <- sqrt(mean(residuals(mfold1.train)^2))
  
  predict.mfold1.test <- predict(mfold1.train, newdata = testset)
  testset.error <- testset$G3 - predict.mfold1.test
  RMSE.fold1.test[i] <- sqrt(mean(testset.error^2))
}

RMSE.fold1.train.mean <- mean(RMSE.fold1.train)
RMSE.fold1.test.mean <- mean(RMSE.fold1.test)
round(RMSE.fold1.train.mean,5) #3.68985
round(RMSE.fold1.test.mean,5) #3.95774

RMSE.fold1.train.se <- sd(RMSE.fold1.train)/sqrt(folds)
RMSE.fold1.test.se <- sd(RMSE.fold1.test)/sqrt(folds)
round(RMSE.fold1.train.se,5) #0
round(RMSE.fold1.test.se, 5) #0.11129


### models (Backward Elimination) ---------------------------------------------
library(caTools)
set.seed(2021)

m.back <- step(m.full)
summary(m.back)
plot(m.back)

# Train-Test Split
train <- sample.split(Y=data2$G3, SplitRatio = 0.7)
trainset.step <- subset(data2, train ==T)
testset.step <- subset(data2, train ==F)

mback.1 <- lm(G3 ~ sex + age + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + romantic + freetime + goout + absences, data = trainset.step)
plot(mback.1)
RMSE.mback.train <- sqrt(mean(residuals(mback.1)^2))
predict.mback.test <- predict(mback.1, newdata = testset.step)
testset.error <- testset.step$G3  - predict.mback.test
RMSE.mback.test <- sqrt(mean(testset.error^2)) 
round(RMSE.mback.train,5) #3.68133
round(RMSE.mback.test,5) #4.52708

# 5-fold CV
set.seed(2021)
n <- nrow(data2)
folds <- 5

RMSE.fold2.train <- seq(1:folds)
RMSE.fold2.test <- seq(1:folds)

testlist <- split(sample(1:n), 1:folds)

for (i in 1:folds){
  trainset = data2[-testlist[[i]],]
  testset = data2[testlist[[i]],]
  
  mfold2.train <- lm(G3 ~ sex + age + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + romantic + freetime + goout + absences, data = trainset.step)
  RMSE.fold2.train[i] <- sqrt(mean(residuals(mfold2.train)^2))
  
  predict.mfold2.test <- predict(mfold2.train, newdata = testset)
  testset.error <- testset$G3 - predict.mfold2.test
  RMSE.fold2.test[i] <- sqrt(mean(testset.error^2))
}

RMSE.fold2.train.mean <- mean(RMSE.fold2.train)
RMSE.fold2.test.mean <- mean(RMSE.fold2.test)
round(RMSE.fold2.train.mean,5) #3.68133
round(RMSE.fold2.test.mean,5) #3.94611

RMSE.fold2.train.se <- sd(RMSE.fold2.train)/sqrt(folds)
RMSE.fold2.test.se <- sd(RMSE.fold2.test)/sqrt(folds)
round(RMSE.fold2.train.se,5) #0
round(RMSE.fold2.test.se,5) #0.1165
