#######class attribute의 모든 값을 나눈 경우(low,medium,high)

library(dplyr)
library(ggplot2)
library(MASS)
library(rpart)
library(data.table)
library(ROCR)
library(gridExtra)

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

levels(data$`native English speaker`)
levels(data$`native English speaker`)
levels(data$`Course instructor`)
levels(data$Course)
levels(data$`Summer or regular semester`)
levels(data$`Class attribute`)

table(data$`Class attribute`)




#훈련,테스트세트 구분하기 0.7과 0.3비율로
set.seed(44)
ind<-sample(2,nrow(data), replace = TRUE, prob=c(0.7,0.3))
#ind

training<-data[ind == 1,] 
test<-data[ind == 2,] 
#table(training$Course)
#table(test$Course)


prop.table(table(test$`Class attribute` ) )
prop.table(table(training$`Class attribute` ) )
prop.table(table(data$`Class attribute` ) )



###########시각화


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





#################로지스틱 회귀분석
levels(training$`Class attribute`)
library(nnet)
all <- multinom(`Class attribute` ~ .,
               data = training)
logistic.res <- predict(all, newdata = test, type = "class") 
logistic.res
confusionMatrix(logistic.res,test$`Class attribute`)
mean(logistic.res == test$`Class attribute`)



y_obs <- test$`Class attribute`
yhat_lm <- logistic.res
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






##########나무모형

library(rpart)
library(party)
c <- rpart(`Class attribute`~., data = data)
c
opar <- par(mfrow = c(1,1), xpd = NA)
plot(c)
text(c, use.n = TRUE)
par(opar)

#plot (data_tr , compress = TRUE , margin = .01)
#text (data_tr , cex =1.0)

head(predict(c, newdata = data, type="class"))



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




library ( rpart.plot )
prp (c , type =4 , extra =2)
prp(c.training,type=4, extra=2)
prp(c.test,type=4, extra=2)



tree <- ctree(`Class attribute`~. , data = training)
tree
plot(tree)

testPred <- predict(tree, newdata = test)
confusionMatrix(testPred, test$`Class attribute`)
mean(testPred == test$`Class attribute`)
#tab <- table(testPred, test$`Class attribute`)
#tab
#sum(tab[row(tab) == col(tab)])/sum(tab)




y_obs <- test$`Class attribute`
yhat_tr <- testPred
library(gridExtra)

p1 <- ggplot(data.frame(y_obs, yhat_tr),
             aes(y_obs, yhat_tr, group=y_obs,
                 fill=factor(y_obs))) +
  geom_boxplot()

p2 <- ggplot(data.frame(y_obs, yhat_tr),
             aes(yhat_tr, fill=factor(y_obs))) +
  geom_density(alpha=.5)
grid.arrange(p1, p2, ncol=2)

g <- arrangeGrob(p1, p2, ncol=2)







#########################단순베이즈분류
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
table(data$class)

sum(tab[row(tab) == col(tab)])/sum(tab)




y_obs <- test$`Class attribute`
yhat_nb <- nb.pred
library(gridExtra)

p1 <- ggplot(data.frame(y_obs, yhat_nb),
             aes(y_obs, yhat_nb, group=y_obs,
                 fill=factor(y_obs))) +
  geom_boxplot()

p2 <- ggplot(data.frame(y_obs, yhat_nb),
             aes(yhat_nb, fill=factor(y_obs))) +
  geom_density(alpha=.5)
grid.arrange(p1, p2, ncol=2)

g <- arrangeGrob(p1, p2, ncol=2)









########################knn
library(ISLR)
library(caret)

set.seed(200)
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
knnFit <- train(`Class attribute`~., data = training, method = "knn",
                preProcess = c("center","scale"),
                trControl = ctrl,tuneLength = 30)
knnFit
plot(knnFit)
knnPredict <- predict(knnFit, newdata = test)
confusionMatrix(knnPredict, test$`Class attribute`)
mean(knnPredict == test$`Class attribute`)



data.kknn <- kknn(`Class attribute`~. , training, test, distance = 1, kernel="triangular")
summary(data.kknn)

fit <- fitted(data.kknn)
tab<-table(test$`Class attribute`, fit)
tab
sum(tab[row(tab) == col(tab)])/sum(tab)


y_obs <- test$`Class attribute`
yhat_knn<- fit
library(gridExtra)

p1 <- ggplot(data.frame(y_obs, yhat_knn),
             aes(y_obs, yhat_knn, group=y_obs,
                 fill=factor(y_obs))) +
  geom_boxplot()

p2 <- ggplot(data.frame(y_obs, yhat_knn),
             aes(yhat_knn, fill=factor(y_obs))) +
  geom_density(alpha=.5)
grid.arrange(p1, p2, ncol=2)

g <- arrangeGrob(p1, p2, ncol=2)









