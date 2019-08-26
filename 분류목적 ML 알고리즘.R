#데이터 분활
names(iris)
table(iris$Species)
150*0.7
iris_train = iris[1:105,]
iris_test = iris[106:150,]
idx<-sample(1:nrow(iris),  size=nrow(iris)*0.7, replace=F)

iris_train<-iris[idx, ]
iris_test<-iris[-idx, ]

dim(iris_train)
dim(iris_test)

table(iris_train$Species)
table(iris_test$Species)


install.packages('caret')
library(caret)
train.idx <-createDataPartition(iris$Species, p=0.7, list=F)#번호나옴, createDataPartition 랜덤으로 뽑는다.

iris_train<-iris[train.idx,]
iris_test<-iris[-train.idx,]

dim(iris_train)
dim(iris_test)

table(iris_train$Species)
table(iris_test$Species)

# K-nearest neighbor 
like<-read.csv('like.csv')
colnames(like)<-c('talk','book','travel','gpa','height','skin','muscle','type')
like

test<-data.frame(talk=70, book=50, travel=30, gpa=70, height=70, skin=40, muscle=50)
library(class)
train<-like[,-8]
group<-like[,8]

knnpred1<-knn(train, test, group, k=3, prob=T)
knnpred2<-knn(train, test, group, k=4, prob=T)
knnpred1
knnpred2

#나이브 베이즈 (이번 경우 로지스틱보다 정확도가 높다.)
install.packages('e1071')
library(e1071)
train_rt=naiveBayes(iris_train, iris_train$Species, laplace = 1)#lapalce=1 은 smooth 하게 만들어줌
naive_pdt=predict(train_rt, iris_test, type='class')
table(naive_pdt, iris_test$Species)
confusionMatrix(naive_pdt, iris_test$Species)


#로지스틱 회귀분석
library(nnet)
mult_rt = multinom(Species~.,iris_train)
multi_pdt<-predict(mult_rt, iris_test)
table(iris_test$Species, multi_pdt)
confusionMatrix(iris_test$Species, multi_pdt)

table(iris_train$Species)
table(iris_test$Species)

# decision tree method
library(rpart)
rpart_rt=rpart(Species~., iris_train)
rpart_rt
rpart_pdt=predict(rpart_rt, iris_test, type='class')
table(iris_test$Species, rpart_pdt)
confusionMatrix(iris_test$Species, rpart_pdt)


# ANN model
library(nnet)
iris_train_scale<-as.data.frame(sapply(iris_train[,-5], scale))
head(iris_train_scale)
iris_test_scale<-as.data.frame(sapply(iris_test[,-5],scale))
head(iris_test_scale)

iris_train_scale$Species = iris_train$Species
head(iris_train_scale)
nnet_rlt=nnet(Species~., iris_train_scale, size=3)
nnet_pdt=predict(nnet_rlt, iris_test_scale, type='class')
table(nnet_pdt, iris_test$Species)
class(nnet_pdt)
nnet_pdt=as.factor(nnet_pdt)

class(iris_test$Species)
confusionMatrix(nnet_pdt, iris_test$Species)

# SVM(support vector machine) 
# 서로 다른 분류에 속한 데이터 간에 가격이 최대가 되는 선을 찾아서 이를 기준으로 데이터를 분류하는 모델이다.
install.packages('kernlab')
library(kernlab)
svm_rt=ksvm(Species~.,iris_train, kernel='rbfdot')
svm_pdt=predict(svm_rt, iris_test, type='response') #response 범주형
table(svm_pdt, iris_test$Species)
confusionMatrix(svm_pdt, iris_test$Species)
# kk=table(svm_pdt,iris_test$Species)
# acc=c(accurecy=(kk[1,1]+kk[2,2]+kk[3,3])/sum(kk))
# acc


# Random forest method
install.packages('randomForest')
library(randomForest)
rf_rt=randomForest(Species~., iris_train, ntree=500) #ntree 기본값이 500
rf_pdt=predict(rf_rt, iris_test, type='response')
table(rf_pdt, iris_test$Species)
confusionMatrix(rf_pdt, iris_test$Species)
