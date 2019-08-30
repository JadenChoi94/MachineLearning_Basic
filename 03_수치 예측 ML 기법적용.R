<<<<<<< HEAD
#Regression analysis
install.packages('MASS')
library(MASS)
idx<-sample(1:nrow(Boston), size=nrow(Boston)*0.7, replace=F)
Boston_train<-Boston[idx,]
Boston_test<-Boston[-idx,]
dim(Boston_train); dim(Boston_test)
str(Boston_train)

lm_fit=lm(medv~., data=Boston_train)
summary(lm_fit)

lm_fit2=step(lm_fit, method='both')
summary(lm_fit2)

lm_pdt=predict(lm_fit2, newdata = Boston_test)
lm_pdt

lm_pdt95=predict(lm_fit2, newdata = Boston_test, interval='confidence')
lm_pdt95

lm_pdt99=predict(lm_fit2, newdata = Boston_test, interval='prediction')
lm_pdt99

par(mfrow=c(2,2))
plot(lm_fit2)
shapiro.test(Boston$medv)

foo=mean((lm_pdt-Boston_train$medv)^2)
sqrt(foo)

#=======================================================================
# Decision Tree, 의사결정트리 기법
install.packages('tree')
library(tree)
tree_fit=tree(medv~., data=Boston_train)
summary(tree_fit)

par(mfrow=c(1,1))
plot(tree_fit)
text(tree_fit, pretty=0)

tree_pdt=predict(tree_fit, newdata = Boston_test)
goo=mean((tree_pdt-Boston_test$medv)^2)
goo
sqrt(goo)

library(rpart)
rpart_fit=rpart(medv~., data=Boston_train)
rpart_fit
summary(rpart_fit)

#DT visualization
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(rpart_fit, digits=3, type=0, extra=1, fallen.leaves = F, cex=1)
rpart_pdt=predict(rpart_fit, newdata = Boston_test)
koo=mean((rpart_pdt-Boston_test$medv)^2)
koo
sqrt(koo)

# ANN 알고리즘

#1.정규화 함수 작성
normalize<-function(x){
    return((x-min(x))/(max(x)-min(x)))
}
Boston_train_norm=as.data.frame(sapply(Boston_train, normalize))
Boston_test_norm=as.data.frame(sapply(Boston_test, normalize))

#nnet 이용한 인공신경망 분석
library(nnet)
nnet_fit=nnet(medv~.,data=Boston_train_norm,size=5) #인공 신경망 적합하기
nnet_pdt=predict(nnet_fit, Boston_test_norm, type='raw')#예측결과생성
hoo_nnet=mean((nnet_pdt-Boston_test_norm$medv)^2)#평균제곱오차 계산
hoo_nnet
sqrt(hoo_nnet)

#neuralnet 인공신경망 분석
install.packages('neuralnet')
library(neuralnet)
neural_fit=neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat, data=Boston_train_norm, hidden=5)

neural_pdt=compute(neural_fit, Boston_test_norm[1:13])
neural_pdt
boo_neur=mean((neural_pdt$net.result-Boston_test_norm$medv)^2)
boo_neur
sqrt(boo_neur)

plot(neural_fit)


# RandomForest Algorithm
library(randomForest)
set.seed(123)
runif(2)
runif(4)
set.seed(123)
runif(2)

rf_fit=randomForest(medv~., data=Boston_train, mtry=6, importance=T)
plot(rf_fit)
importance(rf_fit)
varImpPlot(rf_fit)

rf_pdt=predict(rf_fit, newdata = Boston_test)
foo_rf=mean((rf_pdt-Boston_test$medv)^2)
sqrt(foo_rf)


=======
#Regression analysis
install.packages('MASS')
library(MASS)
idx<-sample(1:nrow(Boston), size=nrow(Boston)*0.7, replace=F)
Boston_train<-Boston[idx,]
Boston_test<-Boston[idx,]
dim(Boston_train); dim(Boston_test)
str(Boston_train)

lm_fit=lm(medv~., data=Boston_train)
summary(lm_fit)

lm_fit2=step(lm_fit, method='both')
summary(lm_fit2)

lm_pdt=predict(lm_fit2, newdata = Boston_test)
lm_pdt

lm_pdt95=predict(lm_fit2, newdata = Boston_test, interval='confidence')
lm_pdt95

lm_pdt99=predict(lm_fit2, newdata = Boston_test, interval='prediction')
lm_pdt99

par(mfrow=c(2,2))
plot(lm_fit2)
shapiro.test(Boston$medv)

foo=mean((lm_pdt-Boston_train$medv)^2)
sqrt(foo)

#=======================================================================
# Decision Tree, 의사결정트리 기법
install.packages('tree')
library(tree)
tree_fit=tree(medv~., data=Boston_train)
summary(tree_fit)

par(mfrow=c(1,1))
plot(tree_fit)
text(tree_fit, pretty=0)

tree_pdt=predict(tree_fit, newdata = Boston_test)
goo=mean((tree_pdt-Boston_test$medv)^2)
goo
sqrt(goo)

library(rpart)
rpart_fit=rpart(medv~., data=Boston_train)
rpart_fit
summary(rpart_fit)

#DT visualization
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(rpart_fit, digits=3, type=0, extra=1, fallen.leaves = F, cex=1)
rpart_pdt=predict(rpart_fit, newdata = Boston_test)
koo=mean((rpart_pdt-Boston_test$medv)^2)
koo
sqrt(koo)

# ANN 알고리즘

#1.정규화 함수 작성
normalize<-function(x){
    return((x-min(x))/(max(x)-min(x)))
}
Boston_train_norm=as.data.frame(sapply(Boston_train, normalize))
Boston_test_norm=as.data.frame(sapply(Boston_test, normalize))

#nnet 이용한 인공신경망 분석
library(nnet)
nnet_fit=nnet(medv~.,data=Boston_train_norm,size=5) #인공 신경망 적합하기
nnet_pdt=predict(nnet_fit, Boston_test_norm, type='raw')#예측결과생성
hoo_nnet=mean((nnet_pdt-Boston_test_norm$medv)^2)#평균제곱오차 계산
hoo_nnet
sqrt(hoo_nnet)

#neuralnet 인공신경망 분석
install.packages('neuralnet')
library(neuralnet)
neural_fit=neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat, data=Boston_train_norm, hidden=5)

neural_pdt=compute(neural_fit, Boston_test_norm[1:13])
neural_pdt
boo_neur=mean((neural_pdt$net.result-Boston_test_norm$medv)^2)
boo_neur
sqrt(boo_neur)

plot(neural_fit)


# RandomForest Algorithm
library(randomForest)
set.seed(123)
runif(2)
runif(4)
set.seed(123)
runif(2)

rf_fit=randomForest(medv~., data=Boston_train, mtry=6, importance=T)
plot(rf_fit)
importance(rf_fit)
varImpPlot(rf_fit)

rf_pdt=predict(rf_fit, newdata = Boston_test)
foo_rf=mean((rf_pdt-Boston_test$medv)^2)
sqrt(foo_rf)


>>>>>>> 7a1c17e97b93fe9c0346363f3b1f29c35febdb92
###이로써 지도학습(supervised algorithm)이 끝났다.