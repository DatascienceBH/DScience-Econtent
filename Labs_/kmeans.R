head(iris)
View(iris)
names(iris)
iris.feature<- iris
iris.feature$Species<- NULL
head(iris.feature)
(results<- kmeans(iris.feature, 3))
names(iris)
results$size
#vector
results$cluster
table(iris$Species, results$cluster)
plot(iris[c("Petal.Length", "Petal.Width")], col=results$cluster)
# let's see how accurte they are,  only  classification changes
plot(iris[c("Petal.Length", "Petal.Width")], col=iris$Species)
# Le's chck sepals  
plot(iris[c("Sepal.Length", "Sepal.Width")], col=results$cluster)

# let's compare it to the original one.   
plot(iris[c("Sepal.Length", "Sepal.Width")], col=iris$Species)





pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)


if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
df <- USArrests
head(df)
table(is.na(df))
nrow(df)
df <- na.omit(df)
df <- scale(df)
head(df)
(distance <- get_dist(df))
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = df)


(i.am.array<-array( c(LETTERS[1:12]),c(2,3,2) ))


x<-rnorm(10)
y<- rnorm(10)
t.test(x, y)
t.test(x, y)$statistic
pts = seq(-4.5,4.5,length=100)
plot(pts,dt(pts,df=18),col='red',type='l')






