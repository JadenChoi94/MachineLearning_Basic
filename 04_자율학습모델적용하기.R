# 4-1 클러스터링(군집)분석
iris2<-iris[,1:4]
head(iris2)

km.out.withness<-c()
km.out.between<-c()
goo=c()

for(i in 2:7){
  set.seed(123)
  km.out<-kmeans(iris2, centers=i)
  km.out.withness[i-1]<-km.out$tot.withinss
  km.out.between[i-1]<-km.out$betweenss
  goo=c(goo, i)
}
k=data.frame(goo, km.out.withness, km.out.between)

par(mfrow=c(1,2))
plot(k$goo, k$km.out.withness, type='o')
plot(k$goo, k$km.out.between, type='o')

out=kmeans(iris2, centers =3)
out$centers
out$cluster
out$size
table(out$cluster, iris$Species)

par(mfrow=c(1,1))
plot(iris2[,1:2], col=out$cluster, pch=ifelse(out$cluster==1, 16, ifelse(out$cluster==2, 17, 18)), cex=2);
points(out$centers, col=1:3, pch=16:18, cex=5)


# 4-2 차원축소 기법
crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)
rownames(crime) = crime[, 1]
head(crime)
rownames(crime)

stars(crime[, 2:8])
stars(crime[, 2:8], flip.labels = FALSE)
stars(crime[, 2:8], flip.labels = FALSE, key.loc = c(1,20))
stars(crime[, 2:8], flip.labels = FALSE, key.loc = c(1,20),draw.segments = TRUE)

install.packages("aplpack")
library(aplpack)

faces(crime[, 2:8])
education = read.csv("http://datasets.flowingdata.com/education.csv")
head(education)
str(education)

library(lattice)
parallelplot(education[, 2:7], horizontal.axis = FALSE, col = 1)
summary(education$reading)
color = education$reading > 523
color
color+1

parallelplot(education[, 2:7], horizontal.axis = FALSE, col = color + 1)
summary(education$math)
color = education$dropout_rate > 5.3
parallelplot(education[, 2:7], horizontal.axis = FALSE, col = color + 1)
summary(education$dropout_rate)
color = education$math > 525.5
parallelplot(education[, 2:7], horizontal.axis = FALSE, col = color + 1)

# 3.차원축소 기법의 주요 종류
# 3.1 주성분 분석
data = read.csv("강의안/R_데이터분석/다변량분석/20140528_baseball.csv")
head(data)
model = prcomp(data[, 2:6], scale = T)
summary(model) #cumulative proporation: 설명력, PC2 이차원

plot(model)
biplot(model)

rownames(data)=data[,1]
data
model=prcomp(data[,2:6],scale=T)
biplot(model)


head(USArrests)
pc1=princomp(USArrests, cor=T)
summary(pc1)
plot(pc1, type='l')#엘보우 포인트
pc1$center
pc1$scale #표준편차
pc1$loadings
pc1$scores

plot(pc1$scores[,1], pc1$scores[,2], xlab='xZ1', ylab='Z2')
abline(v=0, h=0, col='gray')
biplot(pc1, cex=0.7) #행렬도
abline(v=0, h=0,col='red')