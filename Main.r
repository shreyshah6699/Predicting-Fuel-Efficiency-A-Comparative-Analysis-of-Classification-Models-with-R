rm(list = ls())

#A
filename <- file.choose()


Auto <- read.csv(filename)

mpg01 <- ifelse( Auto$mpg > median(Auto$mpg), yes = 1, no = 0)
Auto <- data.frame(Auto, mpg01)

#B
corrplot(cor(Auto[,-9]),method="number")

#C
train <- (Auto$year %% 2 == 0)
train.auto <- Auto[train,]
test.auto <- Auto[-train,]

#D
autolda.fit <- lda(mpg01~displacement+horsepower+weight+year+cylinders+origin, data=train.auto)
autolda.pred <- predict(autolda.fit, test.auto)
table(autolda.pred$class, test.auto$mpg01)

mean(autolda.pred$class != test.auto$mpg01)

#E
autoqda.fit <- qda(mpg01~displacement+horsepower+weight+year+cylinders+origin, data=train.auto)
autoqda.pred <- predict(autoqda.fit, test.auto)
table(autoqda.pred$class, test.auto$mpg01)

mean(autoqda.pred$class != test.auto$mpg01)

#F
auto.fit<-glm(mpg01~displacement+horsepower+weight+year+cylinders+origin, data=train.auto,family=binomial)
auto.probs = predict(auto.fit, test.auto, type = "response")
auto.pred = rep(0, length(auto.probs))
auto.pred[auto.probs > 0.5] = 1
table(auto.pred, test.auto$mpg01)

mean(auto.pred != test.auto$mpg01)

#G

#K=1
train.K= cbind(Auto$displacement,Auto$horsepower,Auto$weight,Auto$cylinders,Auto$year, Auto$origin)[train,]
test.K=cbind(Auto$displacement,Auto$horsepower,Auto$weight,Auto$cylinders, Auto$year, Auto$origin)[-train,]
set.seed(1)
autok.pred=knn(train.K,test.K,train.auto$mpg01,k=1)
mean(autok.pred != test.auto$mpg01)


#K=5
autok.pred=knn(train.K,test.K,train.auto$mpg01,k=5)
mean(autok.pred != test.auto$mpg01)

#K=10
autok.pred=knn(train.K,test.K,train.auto$mpg01,k=10)
mean(autok.pred != test.auto$mpg01)

#K=100
autok.pred=knn(train.K,test.K,train.auto$mpg01,k=100)
mean(autok.pred != test.auto$mpg01)