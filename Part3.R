library(ISLR)
library(caTools)
library(class)

setwd("/Users/seeyanyu/Desktop/SIM - UOL (2)/ST 3189 (Machine Learning)/Coursework/Part 3")
data1 <- read.csv("bank.csv", sep = ";", stringsAsFactors = T)
summary(data1)
data2 <- data1[,-6]
data2 <- data2[!(data2$pdays < 0),]
summary(data2)

### Logistic Regression + Confusion Matrix -------------------------------------
set.seed(2021)
m.full <- glm(y~., family = binomial, data = data2)
summary(m.full)

mfull.prob <- predict(m.full, type = "response")
threshold <- 0.5
y.hat <- ifelse(mfull.prob > threshold, "yes", "no")
table1 <- table(data2$y, y.hat)
table1
mean(y.hat==data2$y) #0.8443627

### Logistic Regression + Train-Test Split -------------------------------------
set.seed(2021)
train <- sample.split(Y=data2$y, SplitRatio = 0.7)
trainset <- subset(data2, train == T)
testset <- subset(data2, train ==F)
m1 <- glm(y~., family = binomial, data = trainset)
summary(m1)
testset.prob <- predict(m1, testset, type = "response")
y.hat.test <- ifelse(testset.prob > threshold, "yes", "no")
y.testset <- testset$y
table2 <- table(y.testset, y.hat.test)
table2
mean(y.hat.test == y.testset) #0.7959184


library(MASS)

### LDA (y ~ month)  ----------------------------------------------------------
set.seed(2021)
m2 <- lda(y ~ month, data = trainset)
testset.m2.pred <- predict(m2, testset)
table.m2 <- table(y.testset, testset.m2.pred$class)
table.m2
mean(y.testset == testset.m2.pred$class) #0.7632653

### LDA (y ~ duration)
set.seed(2021)
m3 <- lda(y ~ duration, data = trainset)
testset.m3.pred <- predict(m3, testset)
table.m3 <- table(y.testset, testset.m3.pred$class)
table.m3
mean(y.testset == testset.m3.pred$class) #0.7918367

### LDA (y ~ month + duration)
set.seed(2021)
m4 <- lda(y ~ month + duration, data = trainset)
testset.m4.pred <- predict(m4, testset)
table.m4 <-table(y.testset, testset.m4.pred$class)
table.m4
mean(y.testset == testset.m4.pred$class) #0.8


### QDA (y ~ month) ----------------------------------------------------------
set.seed(2021)
m5 <- qda(y ~ month, data = trainset)
testset.m5.pred <- predict(m5, testset)$class
table.m5 <- table(y.testset, testset.m5.pred)
table.m5
mean(y.testset == testset.m5.pred) #0.755102

### QDA (y ~ duration)
set.seed(2021)
m6 <- qda(y ~ duration, data = trainset)
testset.m6.pred <- predict(m6, testset)$class
table.m6 <- table(y.testset, testset.m6.pred)
table.m6
mean(y.testset == testset.m6.pred) #0.7877551

### QDA (y ~ month + duration)
set.seed(2021)
m7 <- qda(y ~ month + duration, data = trainset)
testset.m7.pred <- predict(m7, testset)$class
table.m7 <- table(y.testset, testset.m7.pred)
table.m7
mean(y.testset == testset.m7.pred) #0.7428571

### KNN -----------------------------------------------------------------

library(ISLR)
library(caTools)
library(class)

setwd("/Users/seeyanyu/Desktop/SIM - UOL (2)/ST 3189 (Machine Learning)/Coursework/Part 3")
data1 <- read.csv("bank.csv", sep = ";", stringsAsFactors = T)
summary(data1)
data2 <- data1[,-6]
data2 <- data2[!(data2$pdays < 0),]
summary(data2)
data2$month <- as.numeric(data2$month)
data2$poutcome <- as.numeric(data2$poutcome)

nrow(data2) #816
round(sqrt(816)) #29

train <- sample.split(Y=data2$y, SplitRatio = 0.7)
trainset <- subset(data2, train == T)
testset <- subset(data2, train ==F)
y.testset <- testset$y

### KNN (y ~ month)
set.seed(2021)
train.month <- as.matrix(trainset$month)
test.month <- as.matrix(testset$month)
train.y <- as.matrix(trainset$y)

knn.pred.1 <- knn(train.month, test.month, train.y, k = 29)
table.knn.1 <- table(y.testset, knn.pred.1)
table.knn.1
mean(knn.pred.1 == y.testset) #0.7510204

### knn (y ~ duration)
set.seed(2021)
train.duration <- as.matrix(trainset$duration)
test.duration <- as.matrix(testset$duration)
train.y <- as.matrix(trainset$y)

knn.pred.2 <- knn(train.duration, test.duration, train.y, k = 29)
table.knn.2 <- table(y.testset, knn.pred.2)
table.knn.2
mean(knn.pred.2 == y.testset) #0.7673469

set.seed(2021)
knn.pred.3 <- knn(train.duration, test.duration, train.y, k = 10)
table.knn.3 <- table(y.testset, knn.pred.2)
table.knn.3
mean(knn.pred.3 == y.testset) #0.7591837

set.seed(2021)
knn.pred.4 <- knn(train.duration, test.duration, train.y, k = 50)
table.knn.4 <- table(y.testset, knn.pred.2)
table.knn.4
mean(knn.pred.4 == y.testset) #0.7836735

### KNN (y ~ poutcome)
set.seed(2021)
train.pout <- as.matrix(trainset$poutcome)
test.pout <- as.matrix(testset$poutcome)
train.y <- as.matrix(trainset$y)

knn.pred.5 <- knn3(train.month, test.month, train.y, k = 29)
table.knn.5 <- table(y.testset, knn.pred.1)
table.knn.5
mean(knn.pred.5 == y.testset) #0.7510204

