data=read.csv('./data/disease.csv')
data
model=glm(disease~., data=data, family=binomial(logit))
summary(model)

model2=glm(disease~age+sector, data=data, family=binomial)
summary(model2)
anova(model, model2, test='Chisq')
table(data$disease)
31/98
kk=(table(data$disease, model2$fitted.values>0.316))

#Error: object 'kk' not found
c(민감도=kk[2,2]/sum(kk[2,]), 특이도=kk[1,1]/sum(kk[1,]),
    에러율=(kk[1,2]+kk[2,1])/sum(kk), 정확도=(kk[1,1] + kk[2,2])/sum(kk))

install.packages('Deducer')
library(Deducer)
rocplot(model2)
rocplot(model)

library(caret)
idx<-createDataPartition(iris$Species, p=0.7, list=F)
iris_train<-iris[idx, ]
iris_test<-iris[-idx, ]
table(iris_train$Species)
table(iris_test$Species)
library(rpart)
library(e1071)
library(randomForest)

rpart_iris<-rpart(Species~., data=iris_train)
bayes_iris<-naiveBayes(Species~., data=iris_train)
rdf_iris<-randomForest(Species~., data=iris_train,importance=T)

##각 머신 러닝 기법별로 예측 범주값 벡터 생성하기
rpart_pdt<-predict(rpart_iris, newdata=iris_test, type='class')
bayes_pdt<-predict(bayes_iris, newdata=iris_test, type='class')
rdf_pdt<-predict(rdf_iris, newdata=iris_test, type='response')

table(iris_test$Species, rpart_pdt)
table(iris_test$Species, bayes_pdt)
table(iris_test$Species, rdf_pdt)

confusionMatrix(rpart_pdt, iris_test$Species, positive='versicolor')
confusionMatrix(bayes_pdt, iris_test$Species, positive='versicolor')
confusionMatrix(rdf_pdt, iris_test$Species, positive='versicolor')

install.packages('leaps')
library(leaps)
head(attitude)
cor(attitude)

out=lm(rating~., data=attitude)
summary(out)
out2=step(out, direction = 'both')
summary(out2) 

names(attitude)
leaps= regsubsets(rating~., data=attitude, nbest = 5)
summary(leaps)
plot(leaps, scale='bic') # bic은 낮을수록 좋다
out_bic=lm(rating~complaints, data=attitude)
summary(out_bic)

summary(out_bic) #Adjusted R-squared: 66%
frame()
plot(leaps, scale='Cp') #Cp, 낮을수록 좋다
out_cp=lm(rating~complaints+learning, data=attitude)
summary(out_cp)

plot(leaps, scale='adjr2') # adjr2 높을수록 좋다다
out_adjr2=lm(rating~complaints+learning+advance, data=attitude)
summary(out_adjr2)
