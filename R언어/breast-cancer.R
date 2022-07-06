library(dplyr)
library(ggplot2)
library(MASS)
library(rpart)
library(data.table)
library(ROCR)
library(gridExtra)

########## 1) 데이터 전처리
data <- read.table("C:/Users/ssoyeon/OneDrive - pusan.ac.kr/바탕 화면/breast-cancer.txt", header=FALSE, sep=",") 
data

#변수명 할당하기
features_name <- c('class','age','menopause','tumor-size','inv-nodes',
                   'node-caps','deg-malig','breast','breast-quad','irradiat')
names(data) <- features_name
data

sum(is.na(data))  #결측값은 없음

#str()명령으로 데이터 구조 살펴보기
str(data) 


#node-caps에 ?가 있는 행은 제외시키기
data <- data[!(data$`node-caps` == "?" ), ]
data

#범주형 데이터가 문자열 chr로 되어있기때문에 factor로 변경하기
data$class <- as.factor(data$class)
data$age <- as.factor(data$age)
data$menopause <- as.factor(data$menopause)
data$`tumor-size` <- as.factor(data$`tumor-size`)
data$`inv-nodes` <- as.factor(data$`inv-nodes`)
data$`node-caps`<-as.factor(data$`node-caps`)
data$`deg-malig`<- as.factor(data$`deg-malig`)
data$breast <- as.factor(data$breast)
data$`breast-quad` <- as.factor(data$`breast-quad`)
data$irradiat <- as.factor(data$irradiat)

#각각의 범주형 변수에 어떤 값이 있는지 확인
levels(data$age)
levels(data$menopause)


#class변수에서 no-recurrence-events일때를 0, recurrence-events 일때를 1로
#data$class <- factor(ifelse(data$class == "no-recurrence-events",0,1))
#data

#data가 제대로 변형되었는지 확인
str(data)


#훈련,테스트세트 구분하기 0.7과 0.3비율로
set.seed(1234)
ind<-sample(2,nrow(data), replace = TRUE, prob=c(0.7,0.3))
ind

training<-data[ind == 1,] 
test<-data[ind == 2,] 
head(training)
head(test)


#시각화 (age와 재발병) 
training %>%
  ggplot(aes(`age`, fill=class)) +
  geom_bar()

#시각화 (menopause와 재발병)
  training %>%
    ggplot(aes(`menopause`, fill=class)) +
    geom_bar()
  
  
#시각화 (tumor-size와 재발병)
training %>%
  ggplot(aes(`tumor-size`, fill=class)) +
  geom_bar()
  
#시각화 (inv-nodes와 재발병)
training %>%
  ggplot(aes(`inv-nodes`, fill=class)) +
  geom_bar()
  

#시각화 (node-caps와 재발병)
training %>%
  ggplot(aes(`node-caps`, fill=class)) +
  geom_bar()


#시각화 (deg-malig와 재발병)
training %>%
  ggplot(aes(`deg-malig`, fill=class)) +
  geom_bar()


#시각화 (breast와 재발병)
training %>%
  ggplot(aes(`breast`, fill=class)) +
  geom_bar()


#시각화 (breast-quad와 재발병)
training %>%
  ggplot(aes(`breast-quad`, fill=class)) +
  geom_bar()


#시각화 (irradiat와 재발병)
training %>%
  ggplot(aes(`irradiat`, fill=class)) +
  geom_bar()





#############로지스틱 회귀분석

all <- glm(class~., data = training, family=binomial)

summary(all)

#step.all <- step(all, direction = "backward")
#anova(all, test="Chisq")


#glm에서 적합된 모형의 예측값 얻기 결과값은 예측 확률값
predict(all, newdata = data[1:5,], type="response")
predict(all, newdata = tail(data), type="response")


fitted(all)[c(1:5, 96:100)]
predict(all, newdata = data[c(1,50,90,130),], type="response")




###################anova(all, test="Chisq")



##############책 내용

#예측값과 실제 간의 관계를 시각화로
y_obs<-ifelse(test$class == 'no-recurrence-events',0,1)
yhat_lm <- predict(all, newdata=test, type='response')

library(gridExtra)

p1 <- ggplot(data.frame(y_obs, yhat_lm),
             aes(y_obs, yhat_lm, group=y_obs,
                 fill=factor(y_obs))) +
  geom_boxplot()

p2 <- ggplot(data.frame(y_obs, yhat_lm),
             aes(yhat_lm, fill=factor(y_obs))) +
  geom_density(alpha=.5)
grid.arrange(p1, p2, ncol=2)

g <- arrangeGrob(p1, p2, ncol=2)

binomial_deviance <- function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return(2*sum(a + b))
}

#예측의 정확도 지표인 이항편차
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
predict_vs <- ifelse(logistic.res >= 0.9, "recurrence-events", "no-recurrence-events")
predict_vs <- as.factor(predict_vs)

confusion.mat<-table(predict_vs, test$class)
confusion.mat
sum(diag(confusion.mat))/sum(confusion.mat)




#############나무모형
library(rpart)
c <- rpart(class~., data = data)
c
opar <- par(mfrow = c(1,1), xpd = NA)
plot(c)
text(c, use.n = TRUE)
par(opar)

#training의 나무모형
c.training <- rpart(class~., data = training)
c.training
opar <- par(mfrow = c(1,1), xpd = NA)
plot(c.training)
text(c.training, use.n = TRUE)
par(opar)
dev.off()

#test의 나무모형
c.test <- rpart(class~., data = test)
c.test
opar <- par(mfrow = c(1,1), xpd = NA)
plot(c.test)
text(c.test, use.n = TRUE)
par(opar)
dev.off()

library(party)
tree <- ctree(class~. , data = training)
tree
plot(tree)



####그림 더 잘보게
library(rpart.plot)
prp(c,type=4, extra=2)

prp(c.training,type=4, extra=2)

prp(c.test,type=4, extra=2)



##혼동행렬
testPred <- predict(tree, newdata = test)
tab <- table(testPred, test$class)
tab
sum(tab[row(tab) == col(tab)])/sum(tab)



###이항편차 계산
yhat_tr <- predict(c.training, test)
yhat_tr <-yhat_tr[,"no-recurrence-events"]
binomial_deviance(y_obs, yhat_tr)

y_obs<-ifelse(test$class == 'no-recurrence-events',1,0)

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

#ROC 곡선 그렸을때 부드러운 곡선이 아닌 꺽어진 직선으로 나타남-> 모형이 단순해서











#############단순베이즈 분류
#train.ind<- sample(1:nrow(data), ceiling(nrow(data)*2/3), replace = FALSE)
library(e1071)
nb.res <- naiveBayes(class~. , data =training)
nb.pred <- predict(nb.res, test)
confusion.mat <- table(nb.pred, test$class)
confusion.mat
sum(diag(confusion.mat))/sum(confusion.mat)
#정확도가 약 0.78정도

##혼동행렬
library(e1071)
m <- naiveBayes(class~., data = data)
m
table(predict(m,data), data[,5])
pred <-predict(m,data[,-1])
tab <- table(pred, data$class)
tab
table(data$class)

sum(tab[row(tab) == col(tab)])/sum(tab)



####ROC 곡선 & 이항편차
y_obs<-ifelse(test$class == 'no-recurrence-events',1,0)
yhat_nb <- predict(nb.res, test)
y_obs_nb <-ifelse(yhat_nb == 'no-recurrence-events',1,0)
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
library(kknn)
data.kknn <- kknn(class~. , training, test, distance = 1, kernel="triangular")
summary(data.kknn)
fit <- fitted(data.kknn)
confusionMatrix(fit, test$class)
mean(fit == test$class)
pcol <-as.character(as.numeric(test$class))
pairs(test[1:4], pch=pcol,
      col=c("green3","red")[(test$class != fit)+1])




library(ISLR)
library(caret)

prop.table(table(training$class)) * 100
prop.table(table(test$class)) * 100
prop.table(table(data$class)) * 100

set.seed(200)
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
knnFit <- train(class~., data = training, method = "knn",
                preProcess = c("center","scale"),
                trControl = ctrl,tuneLength = 20)
knnFit
plot(knnFit)
knnPredict <- predict(knnFit, newdata = test)
confusionMatrix(knnPredict, test$class)
mean(knnPredict == test$class)






###roc곡선&이항편차
y_obs<-ifelse(test$class == 'no-recurrence-events',1,0)
y_obs_fit<-ifelse(fit== 'no-recurrence-events',1,0)
binomial_deviance(y_obs, y_obs_fit)


pred_knn <- prediction(y_obs_fit, y_obs)
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
dev.off()



