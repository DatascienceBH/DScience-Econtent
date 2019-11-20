library(randomForest)
head(iris)
str(iris)
data_set_size<- floor(nrow(iris)*.8)
index<- sample(1:nrow(iris), size=data_set_size)
training<- iris[index,]
testing<- iris[-index, ]

rf<- randomForest( Species ~ . , data=training, mtry=4, ntree=500, importance=T)
rf
# estimate of  error rate: 4.17%
# which means and accuracy  of 95.83%

plot(rf)
result<-data.frame(testing$Species, predict(rf, testing[,1:4], type="response"))
tail(result)
plot(result)
