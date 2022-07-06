##################1번
rm(list = ls())
library(mlbench)
data(PimaIndiansDiabetes)
data <- PimaIndiansDiabetes
summary(data)

set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))


trainData <- data[ind==1, ]
testData <- data[ind==2, ]

#######(1) - 의사결정나무

library(party)
tree <- ctree(diabetes ~ ., data=trainData)
tree
plot(tree)


library(caret)
testPred = predict(tree, newdata=testData)
confusionMatrix(testPred, testData$diabetes)
mean(testPred == testData$diabetes)
mean(testPred != testData$diabetes)

library(Epi)
ROC(test=testPred, stat=testData$diabetes, plot="ROC", AUC=T, main="Decision Tree")


"""
confusion.mat<-table(testPred, testData$diabetes)
confusion.mat
sum(diag(confusion.mat))/sum(confusion.mat)
"""

#######(1) - AdaBoost
#https://todayisbetterthanyesterday.tistory.com/53
library(adabag)
#fit.bag = bagging(diabetes ~., data=trainData, mfinal=50)   #m은 반복의 횟수, 배깅을 의사결정 나무에 적용(50번까지 나와있음)
#fit.bag$importance
#predict.bagging(fit.bag, newdata=testData)$error    


boo.adabag <- boosting(diabetes~. , data = trainData, mfinal=50)
boo.adabag$importance

plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])
dev.off()

pred <- predict(boo.adabag, newdata = testData)
pred$class <- as.factor(pred$class)
confusionMatrix(pred$class, testData$diabetes)
mean(pred$class == testData$diabetes)
mean(pred$class != testData$diabetes)
ROC(test=pred$class, stat=testData$diabetes, plot="ROC", AUC=T, main="Adaboost")


#######(1) - 랜덤포레스트
library(randomForest)

ntree <- seq(100, 1000, by=100)
error <- rep(0,length(ntree))

count = 1
for(i in ntree){
  cat('ntree : ', i, '\n')
  RF_res <- randomForest(diabetes ~ ., data=trainData, ntree=i, Importance=TRUE)
  pred = predict(RF_res, testData[,1:8])
  confusionMatrix(pred, testData$diabetes)
  error[count] = mean(pred != testData$diabetes)
  count = count + 1
}
error #error을 보게 되면 300,500,700,800,900일때가 가장 낮게 나옴


RF_res <- randomForest(diabetes ~ ., data=trainData, ntree=300, Importance=TRUE)

pred = predict(RF_res, testData[,1:8])
confusionMatrix(pred, testData$diabetes)
mean(pred == testData$diabetes)
mean(pred != testData$diabetes)

ROC(test=pred, stat=testData$diabetes, plot="ROC", AUC=T, main="RandomForest")



"""
RF_res = randomForest(diabetes ~ ., data=trainData, ntree=1000, Importance=TRUE)
#summary(RF_res)
#RF_res$importance
#RF_res$confusion

testPred = predict(RF_res, testData[,1:8])
confusionMatrix(testPred, testData$diabetes)
mean(testPred== testData$diabetes)

#confusion.mat<-table(testPred, testData$diabetes)
#confusion.mat
#sum(diag(confusion.mat))/sum(confusion.mat)
#mean(testPred != testData[,9])
#mean(testPred == testData[,9])

"""


#######(2) - 선형커널
library(e1071)
tuned.linear <- tune.svm(diabetes~., data = trainData, gamma = 10^(-6:-1), cost = 10^(1:2), kernel = "linear")
summary(tuned.linear)
#cost가 100일때, gamma가 1e-06 최적의 모수

svmfit1 = svm(diabetes ~ ., data = trainData, kernel = "linear", cost = 100, scale = FALSE, gamma = 1e-06)
svmfit1
pred <- predict(svmfit1, newdata = testData, decision.values = TRUE)
confusionMatrix(pred, testData$diabetes)
mean(pred == testData$diabetes)   #0.7692308
mean(pred != testData$diabetes)   #0.2307692


#install.packages("Epi")
library(Epi)
ROC(test=pred, stat=testData$diabetes, plot="ROC", AUC=T, main="linear SVM")





#######(2) - 가우시안커널
tuned.gaussian <- tune.svm(diabetes~., data = trainData, gamma = 10^(-6:-1), cost = 10^(1:2), kernel = "radial")
summary(tuned.gaussian)

svmfit2 = svm(diabetes ~ ., data = trainData, scale = FALSE, kernel = "radial", cost = 100,gamma = 0.001)
svmfit2

pred <- predict(svmfit2, newdata = testData, decision.values = TRUE)
confusionMatrix(pred, testData$diabetes)
mean(pred == testData$diabetes)
mean(pred != testData$diabetes)

ROC(test=pred, stat=testData$diabetes, plot="ROC", AUC=T, main="gaussian SVM")







