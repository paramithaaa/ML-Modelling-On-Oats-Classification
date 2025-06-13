library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(tidyverse)
library(reshape2)
library(tree)
library(e1071)
library(adabag)
library(randomForest)
library(pROC)
library(tibble)
library(rpart)
library(ROCR)
library(Metrics)
library(neuralnet)
library(ROSE)

rm(list = ls())
set.seed(33410712)
W = read.csv("WinnData.csv")
W = W[sample(nrow(W), 5000, replace = FALSE),]
W = W[,c(sort(sample(1:30,20, replace = FALSE)), 31)]

#Question 1 and 2
str(W)
#finding the proportion of 1 in the data
sum(W$Class)/length(W$Class)

#balancing
W_balanced = ovun.sample(Class ~ ., data = W, method = "under", N = 2*sum(W$Class == 1))$data
sum(W_balanced$Class)/length(W_balanced$Class)

independent_vars = names(W)[numeric_vars & names(W) != "Class"]

#preprocessing
#standardize 
Wbalance_scaled = W_balanced
Wbalance_scaled[, independent_vars] = scale(W_balanced[, independent_vars])
summary(Wbalance_scaled[, independent_vars])

#Question 3
#split train and test
set.seed(33710712)
train.row = sample(1:nrow(Wbalance_scaled), 0.7*nrow(Wbalance_scaled))
Wbalance_train = Wbalance_scaled[train.row,]
Wbalance_test = Wbalance_scaled[-train.row,]

Wbalance_train$Class = as.factor(Wbalance_train$Class)
Wbalance_test$Class = as.factor(Wbalance_test$Class)

#Question 11
rffit_selected = randomForest(Class ~., data=Wbalance_train, ntree=100)
rfpred_selected = predict(rffit_selected, newdata = Wbalance_test)

cmrf_selected = confusionMatrix(Wbalance_test$Class, rfpred_selected)

#calculate metrics
accuracy_rf = cmrf_selected$overall["Accuracy"]
precision_rf = cmrf_selected$byClass["Precision"]
recall_rf = cmrf_selected$byClass["Recall"]
f1_rf = cmrf_selected$byClass["F1"]

#print results
print(accuracy_rf)
print(precision_rf)
print(recall_rf)
print(f1_rf)

#tuning
#10-fold cross-validation
ctrl = trainControl(method = "cv", number = 10)

#start tuning
set.seed(33410712)
rf_tuned = train(Class ~ ., data = Wbalance_train, method = "rf", trControl = ctrl, tuneLength = 5)

print(rf_tuned)
plot(rf_tuned)
rfpred_tuned = predict(rf_tuned, newdata = Wbalance_test)
cmrf_tuned = confusionMatrix(Wbalance_test$Class, rfpred_tuned)
print(cmrf_tuned)
cmrf_tuned

#calculate metrics
acc = rf_tuned$overall["Accuracy"]
pre = rf_tuned$byClass["Precision"]
rec = rf_tuned$byClass["Recall"]
f1 = rf_tuned$byClass["F1"]

#print results
print(acc)
print(pre)
print(rec)
print(f1)

