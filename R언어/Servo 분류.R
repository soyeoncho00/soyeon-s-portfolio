rm(list = ls())
library(mlbench)
data(Servo)
data <- Servo
summary(data)
sum(is.na(data))
#https://blog.naver.com/PostView.nhn?isHttpsRedirect=true&blogId=urimee_e&logNo=220668980136&redirect=Dlog&widgetTypeCall=true&directAccess=false
data[,"y"] = 0
data[data$Class >=12, "y"] = 1
data$y <- as.factor(data$y)
data <- data[,-5]
str(data)

set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
ind

training <- data[ind==1, ]
test <- data[ind==2, ]


##############################(1) knn
library(kknn)
library(ISLR)
library(caret)

prop.table(table(training$y)) * 100
prop.table(table(test$y)) * 100
prop.table(table(data$y)) * 100


set.seed(200)
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
knnFit <- train(y~., data = training, method = "knn",
                preProcess = c("center","scale"),
                trControl = ctrl,tuneLength = 20)
knnFit   
plot(knnFit)
knnPredict <- predict(knnFit, newdata = test)
confusionMatrix(knnPredict, test$y)
mean(knnPredict == test$y)
mean(knnPredict != test$y)

ROC(knnPredict, stat=test$y, plot="ROC", AUC=T, main="knn")



##############################(2)베이즈 분류
library(e1071)
nb.res <- naiveBayes(y~. , data =training)
nb.pred <- predict(nb.res, test)
confusionMatrix(nb.pred, test$y)
mean(nb.pred == test$y)
mean(nb.pred != test$y)
ROC(nb.pred, stat=test$y, plot="ROC", AUC=T, main="naiveBayes")


"""
confusion.mat <- table(nb.pred, test$y)
confusion.mat
sum(diag(confusion.mat))/sum(confusion.mat)
"""



