
#######class attribute값 중 2,3을 2로 합친경우!!!!!!

data <- read.table("C:/Users/ssoyeon/OneDrive - pusan.ac.kr/바탕 화면/Teaching Assistant Evaluation.txt", header=FALSE, sep=",") 
data


features_name <- c('native English speaker','Course instructor','Course',
                   'Summer or regular semester','Class size','Class attribute')
names(data) <- features_name

sum(is.na(data))  #결측값은 없음
str(data)


data$`Course instructor` <- as.factor(data$`Course instructor`)
data$Course <- as.factor(data$Course)
data$`Class attribute` <- as.factor(data$`Class attribute`)
data$`native English speaker`<-as.factor(data$`native English speaker`)
data$`Summer or regular semester`<-as.factor(data$`Summer or regular semester`)

str(data)

#levels(data$`native English speaker`)
#levels(data$`native English speaker`)
#levels(data$`Course instructor`)
#levels(data$Course)
#levels(data$`Summer or regular semester`)
#levels(data$`Class attribute`)

data$`Class attribute` <- ifelse(data$`Class attribute` == '1','1','2')
data$`Class attribute` <- as.factor(data$`Class attribute`)
table(data$`Class attribute`)


#훈련,테스트세트 구분하기 0.7과 0.3비율로
set.seed(44)
ind<-sample(2,nrow(data), replace = TRUE, prob=c(0.7,0.3))
#ind

training<-data[ind == 1,] 
test<-data[ind == 2,] 

prop.table(table(test$`Class attribute` ) )
prop.table(table(training$`Class attribute` ) )
prop.table(table(data$`Class attribute` ) )




###시각화
training %>%
  ggplot(aes(`native English speaker`, fill=`Class attribute`)) +
  geom_bar()


training %>%
  ggplot(aes(`Course instructor`, fill=`Class attribute`)) +
  geom_bar()

training %>%
  ggplot(aes(`Course`, fill=`Class attribute`)) +
  geom_bar()


training %>%
  ggplot(aes(`Summer or regular semester`, fill=`Class attribute`)) +
  geom_bar()


training %>%
  ggplot(aes(`Class size`, fill=`Class attribute`)) +
  geom_density(alpha=.5)

training %>%
  ggplot(aes(`Class size`, fill=`Class attribute`)) +
  geom_bar()


##로지스틱
all <- glm(`Class attribute`~., data = training, family=binomial)
summary(all)



predict(all, newdata = data[1:5,], type="response")
predict(all, newdata = tail(data), type="response")


fitted(all)[c(1:5, 96:100)]
predict(all, newdata = data[c(1,50,90,130),], type="response")




y_obs<-ifelse(test$`Class attribute` == '2',1,0)
yhat_lm <- predict(all, newdata=test, type='response')
#yhat_lm <- as.numeric(yhat_lm)

binomial_deviance <- function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return(2*sum(a + b))
}

binomial_deviance(y_obs,yhat_lm)


####ROC 곡선
library(ROCR)
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
performance(pred_lm, "auc")@y.values[[1]]


####혼동행렬

logistic.res <- predict(all, newdata = test, type="response")
predict_vs <- ifelse(logistic.res >= 0.9, "2", "1")
predict_vs <- as.factor(predict_vs)
confusionMatrix(predict_vs, test$`Class attribute`)
mean(predict_vs == test$`Class attribute`)




#############나무모형
library(rpart)
c <- rpart(`Class attribute`~., data = data)
c
opar <- par(mfrow = c(1,1), xpd = NA)
plot(c)
text(c, use.n = TRUE)
par(opar)

#training의 나무모형
c.training <- rpart(`Class attribute`~., data = training)
c.training
opar <- par(mfrow = c(1,1), xpd = NA)
plot(c.training)
text(c.training, use.n = TRUE)
par(opar)
dev.off()

#test의 나무모형
c.test <- rpart(`Class attribute`~., data = test)
c.test
opar <- par(mfrow = c(1,1), xpd = NA)
plot(c.test)
text(c.test, use.n = TRUE)
par(opar)
dev.off()




####그림 더 잘보게
library(rpart.plot)
prp(c,type=4, extra=2)

prp(c.training,type=4, extra=2)

prp(c.test,type=4, extra=2)



##혼동행렬

tree <- ctree(`Class attribute`~. , data = training)
#tree
#plot(tree)

testPred <- predict(tree, newdata = test)
confusionMatrix(testPred, test$`Class attribute`)
mean(testPred == test$`Class attribute`)


yhat_tr <- predict(c.training, test)
yhat_tr <-yhat_tr[,"2"]
binomial_deviance(y_obs, yhat_tr)

y_obs<-ifelse(test$Class == '2',1,0)

pred_tr <- prediction(yhat_tr, y_obs)
perf_tr <- performance(pred_tr, measure = "tpr", x.measure = "fpr")
performance(pred_tr, "auc")@y.values[[1]]
plot(perf_lm, col='black', main="ROC Curve")
plot(perf_tr, col='blue', add=TRUE)
abline(0,1, col='gray')
legend('bottomright', inset=.1,
       legend = c("GLM", "Tree"),
       col=c('black', 'blue'), lty=1, lwd=2)
dev.off()







#############단순베이즈 분류
#train.ind<- sample(1:nrow(data), ceiling(nrow(data)*2/3), replace = FALSE)
library(e1071)
nb.res <- naiveBayes(`Class attribute`~. , data =training)
nb.pred <- predict(nb.res, test)
confusionMatrix(nb.pred,test$`Class attribute`)
mean(nb.pred == test$`Class attribute`)



library(e1071)
m <- naiveBayes(`Class attribute`~., data = data)
m
table(predict(m,data), data[,5])
pred <-predict(m,data[,-1])
tab <- table(pred, data$`Class attribute`)
tab
table(data$`Class attribute`)

sum(tab[row(tab) == col(tab)])/sum(tab)






####ROC 곡선 & 이항편차
y_obs<-ifelse(test$`Class attribute` == '2',1,0)
yhat_nb <- predict(nb.res, test)
y_obs_nb <-ifelse(yhat_nb == '2',1,0)
binomial_deviance(y_obs, y_obs_nb)

library(ROCR)
pred_nb <- prediction(y_obs_nb, y_obs)
perf_nb <- performance(pred_nb, measure = "tpr", x.measure = "fpr")
plot(perf_nb, col='red', main="ROC Curve")
plot(perf_tr, col='blue',add=TRUE)
plot(perf_lm, col='black', add=TRUE)
abline(0,1, col='gray')
legend('bottomright', inset=.1,
       legend = c("GLM","Tree","NaiveBayes"),
       col=c('black','blue', 'red'), lty=1, lwd=2)

performance(pred_nb, "auc")@y.values[[1]]
dev.off()





#######################knn 인접

library(ISLR)
library(caret)


set.seed(200)
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
knnFit <- train(`Class attribute`~., data = training, method = "knn",
                preProcess = c("center","scale"),
                trControl = ctrl,tuneLength = 20)
knnFit
plot(knnFit)

#혼동행렬
knnPredict <- predict(knnFit, newdata = test)
confusionMatrix(knnPredict, test$`Class attribute`)
mean(knnPredict == test$`Class attribute`)





data.kknn <- kknn(`Class attribute`~. , training, test, distance = 1, kernel="triangular")
summary(data.kknn)

fit <- fitted(data.kknn)
pcol <-as.character(as.numeric(test$`Class attribute`))
pairs(test[1:4], pch=pcol,
      col=c("green3","red")[(test$`Class attribute` != fit)+1])


##이항편차&ROC곡선&AUC
y_obs<-ifelse(test$`Class attribute` == '2',1,0)
yhat_knn <- knnPredict
y_obs_knn <-ifelse(yhat_knn == '2',1,0)
binomial_deviance(y_obs, y_obs_knn)


pred_knn <- prediction(y_obs_knn, y_obs)
perf_knn <- performance(pred_knn, measure = "tpr", x.measure = "fpr")
plot(perf_nb, col='red', main="ROC Curve")
plot(perf_tr, col='blue',add=TRUE)
plot(perf_lm, col='black', add=TRUE)
plot(perf_knn, col='green', add=TRUE)

abline(0,1, col='gray')
legend('bottomright', inset=.1,
       legend = c("GLM","Tree","NaiveBayes",'Knn'),
       col=c('black','blue', 'red','green'), lty=1, lwd=2)

performance(pred_knn, "auc")@y.values[[1]]











