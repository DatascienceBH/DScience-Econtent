#load package 
install.packages("FSelector")
install.packages("rpart")
install.packages("caret", dependencies=TRUE)
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("xlsx")
install.packages("data.free")
install.packages("caTools")

tapply(as.vector(iris[,4]), factor(iris[,5]), mean)
head(as.vector(iris[,4]))
iris[,4]
str(iris)
tapply(iris[,4], iris[,5], mean)

