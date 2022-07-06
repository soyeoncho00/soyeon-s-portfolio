rm(list = ls())
library(mlbench)
library(mice)
library(caret)

data("BreastCancer")
summary(BreastCancer) # Bare.nuclei변수에서 16개의 결측치 발견 
mean(is.na(BreastCancer)) # 전체에서 0.2% 매우 낮은 비율이지만 처리해줘야 한다고 판단

# 결측치 처리 및 ID 변수 제외  
dataset_impute <- mice(BreastCancer[,2:10],  print = FALSE)
BreastCancer <- cbind(BreastCancer[,11, drop = FALSE], mice::complete(dataset_impute, 1))
summary(BreastCancer) 
str(BreastCancer)


# 순서형 독립변수 수치화 (주관적)
BreastCancer[, c(2:10)] <- sapply(BreastCancer[, c(2:10)], as.character)
BreastCancer[, c(2:10)] <- sapply(BreastCancer[, c(2:10)], as.numeric)
str(BreastCancer)


data<-BreastCancer

set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
ind

trainData <- data[ind==1, ]
testData <- data[ind==2, ]

##############################(1)logit
fit.full <- glm(Class~., data = trainData, family = binomial(link = 'logit'))
summary(fit.full)
pred.glm = as.numeric(predict(fit.full, testData, type = "response") > 0.5)
testPred <- as.factor(ifelse(pred.glm == 0, "benign", "malignant"))
confusionMatrix(testPred, testData$Class)
mean(testPred == testData$Class)
mean(testPred != testData$Class)

library(Epi)
ROC(test=testPred, stat=testData$Class, plot="ROC", AUC=T, main="full logit")


######(1)-2
step.vs<-step(fit.full, direction = 'backward')   #AIC 적게 나온 걸 보면 fit.full이랑 비교해보았을때 변수가 다르다는 것을 알 수 있음 
pred.step = as.numeric(predict(step.vs, testData, type = "response") > 0.5)
testPred <- as.factor(ifelse(pred.step == 0, "benign", "malignant"))
confusionMatrix(testPred, testData$Class)
mean(testPred == testData$Class)
mean(testPred != testData$Class)

ROC(test=testPred, stat=testData$Class, plot="ROC", AUC=T, main="backward logit")




##################################(2)
"""
normalize <- function(x) {
  return ((x-min(x))/(max(x) - min(x)))
}
BreastCancer[,2:10] <- as.data.frame(lapply(BreastCancer[,2:10], normalize))

data<-BreastCancer

set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
ind

trainData <- data[ind==1, ]
testData <- data[ind==2, ]
"""

###backpropagation neural networks
library(neuralnet)
#net.BreastCancer <- neuralnet(Class~.,hidden=c(2,2), data=trainData, linear.output=F, stepmax=1e+10)
#net.BreastCancer
#plot(net.BreastCancer)

results <- c()
for (i in 1:10) {
  net <- neuralnet(Class~. ,hidden=c(i,i), data=trainData, linear.output=F)
  predict_net_test <- compute(net,testData[,2:10])
  predict_result<-round(predict_net_test$net.result, digits = 0)
  predTest <- as.factor(c("benign", "malignant")[apply(predict_result, 1, which.max)])
  results <- c(results, mean(predTest == testData$Class))
}
results
plot(1:10,results)
which.max(results)
max(results)


net <- neuralnet(Class~. ,hidden=c(9,9), data=trainData, linear.output=F)
net
plot(net)


predict_net_test <- compute(net,testData[,2:10])
predict_result<-round(predict_net_test$net.result, digits = 0)
predTest <- as.factor(c("benign", "malignant")[apply(predict_result, 1, which.max)])
confusionMatrix(predTest, testData$Class)

mean(predTest == testData$Class) 
mean(predTest != testData$Class)

ROC(test=predTest, stat=testData$Class, plot="ROC", AUC=T, main="nerual networks")






"""
predTest <- compute(net, covariate=testData[, c(2:10)])$net.result
#predTest <- ifelse(predTest > 0.5, malignant, benign)
predTest <- ifelse(predTest > 0.5, 1, 0)
confusionMatrix(predTest, testData$Class)
"""



head(net$generalized.weights[[1]])
par(mfrow=c(3,3))
gwplot(net, selected.covariate='Cl.thickness', min=-2.5, max=5)
gwplot(net, selected.covariate='Cell.size', min=-2.5, max=5)
gwplot(net, selected.covariate='Cell.shape', min=-2.5, max=5)
gwplot(net, selected.covariate='Marg.adhesion', min=-2.5, max=5)
gwplot(net, selected.covariate='Epith.c.size', min=-2.5, max=5)
gwplot(net, selected.covariate='Bare.nuclei', min=-2.5, max=5)
gwplot(net, selected.covariate='Bl.cromatin', min=-2.5, max=5)
gwplot(net, selected.covariate='Normal.nucleoli', min=-2.5, max=5)
gwplot(net, selected.covariate='Mitoses', min=-2.5, max=5)





###########################(3) - 배깅
library(adabag)
fit.bag = bagging(Class ~., data=trainData, mfinal=50)
#predict.bagging(fit.bag, newdata=testData)$error
#fit.bag$trees

library(rpart.plot)
par(mfrow=c(1,1))
rpart.plot(fit.bag$trees[[1]])
pred <- as.factor(predict(fit.bag, testData)$class)
confusionMatrix(pred, testData$Class)
mean(pred == testData$Class)
mean(pred != testData$Class)

ROC(test=pred, stat=testData$Class, plot="ROC", AUC=T, main="bagging")



###########################(3) - 랜덤포레스트  ----  결과가 자꾸 바뀌어서 나옴
library(randomForest)
"""
RF_res <- randomForest(Class ~ ., data=trainData, ntree=1000, Importance=TRUE)
#summary(RF_res)
RF_res
RF_res$confusion

pred = predict(RF_res, testData[,2:10])
confusionMatrix(pred, testData$Class)
mean(pred == testData$Class)
mean(pred != testData$Class)
"""


ntree <- seq(100, 1000, by=100)
error <- rep(0,length(ntree))

count = 1
for(i in ntree){
  cat('ntree : ', i, '\n')
  RF_res <- randomForest(Class ~ ., data=trainData, ntree=i, Importance=TRUE)
  pred = predict(RF_res, testData[,2:10])
  confusionMatrix(pred, testData$Class)
  error[count] = mean(pred != testData$Class)
  count = count + 1
}
error 
plot(error)
which.min(error)
min(error)


RF_res <- randomForest(Class ~ ., data=trainData, ntree=800, Importance=TRUE)
RF_res$confusion

pred = predict(RF_res, testData[,2:10])
confusionMatrix(pred, testData$Class)
mean(pred == testData$Class)
mean(pred != testData$Class)

ROC(test=pred, stat=testData$Class, plot="ROC", AUC=T, main="RandomForest")








###########################(4)SVM
#첫번째 방법
library(kernlab)
svmM <- ksvm(Class~., data = trainData, kernel='rbfdot',C=10, prob.model=T, cross=5)
pred <- predict(svmM,newdata=testData,type='response')
confusionMatrix(pred, testData$Class)
mean(pred == testData$Class)
mean(pred != testData$Class)
ROC(test=pred, stat=testData$Class, plot="ROC", AUC=T, main="svm")












