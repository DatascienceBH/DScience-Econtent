
getwd()



rm(list=ls())

d <- mtcars
fit <- lm(mpg ~ wt, data = d) # fit the model
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values
ggplot(d, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = wt, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
theme_bw()

plot(fit, which=1)



library(help="datasets")

unique(iris$Species)

head(iris)

glimpse(iris)

str(iris)

levels(iris$Species)

is.factor(iris$Species)

names(iris)

library(ggplot2)
library(GGally)

ggpairs(iris)

(r<-cor(iris$Petal.Length, iris$Petal.Width))
#btw thsiis pearson correlation equation

# try pearson, spearman and kendall 
cor.test(iris$Petal.Length, iris$Petal.Width, method = "spearman")

(r<-cor.test(iris$Petal.Length, iris$Petal.Width))
# confidence interval determin the correlation figures within the intervall [0.9490525 , 0.9729853]

names(r)

r$estimate

(t_value<-r$estimate*sqrt(148)/sqrt(1-r$estimate^2))
#same as r$statistic

r$conf.int

r$alternative

r$p.value



chisq.test(iris$Petal.Length, iris$Petal.Width)
# We have a high Chi-squared value and a P-value of less than. 5% significance level.
# So, we reject the null Hyothesis and conclude that. Petal.Length and Petal.Width have
# a SIGNIFICANT relationship

# we can even have a btter plot for our iris data
install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)
chart.Correlation(iris[,-5], histogram=TRUE, pch=19)

# حمّل قاعد بيانات زهرة اوركيد

data(iris)

# تحميل مكتبة التخطييات البيناية
library(ggplot2)





#  Petal.Width, Petal.Length     انجاز تخطيط بياني حول المتغيرين
 
ggplot(iris , aes(Petal.Width, Petal.Length))+
geom_point()


attach(iris)

lmModel<- lm(Petal.Length ~  Petal.Width, iris)

# Get a summary report of the model
summary(lmModel)

library(dplyr)
library(broom)

names(lmModel)

names(lmModel$coefficients)

lmModel %>% 
    augment() %>%
head()

lmModel_Augmented<- lmModel %>% 
    augment() 

glance(lmModel)

tidy(lmModel)

var(lmModel_Augmented$`.fitted`)


lmModel %>%
    augment() %>%
    summarise(var_e=var(.resid), var_y=var(Petal.Length)) %>%
    mutate(R_squared=1-var_e/var_y)

r$estimate

getwd()

getwd()

qchisq(.95,148)

(t_value<- 2.22994 /0.05140)  

0.9271/(1-0.9271)*148


43.39^2





anova(lmModel)

(F_Value<-430.4806468/0.2286808)

anova(lmModel)




iris$predicted <- predict(lmModel)   # Save the predicted values 
iris$residuals <- residuals(lmModel) # Save the residual values
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Petal.Width, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = F, size = F) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

names(lmModel)

par=(mfrow=c(2,2))
plot(lmModel)

names(lmModel)



df <- lmModel %>% augment()
names(df)
head(df)

ggplot(df, aes(.fitted, .resid)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(yend = .hat, xend = .fitted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(.resid), size = abs(.resid))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = F, size = F) +                             # Size legend removed
  geom_point(aes(y = .resid), shape = 1) +
  theme_bw()

anova(lmModel)

summary(lmModel)

# mean Square = Sum_Square/Df
# Residual Standard Error = Sqrt(Mean_Sq)
round(sqrt(0.2286808),4)
# which is exactly What was displayed above at Redisual Standard Error: 0.4782 on 148 degrees of freedom
# Petal length deviate from ht eregression line by approx. 0.4782
(devationRatio<-0.4782/mean(Petal.Length))
# is 12.72% acceptable for our regression line model 

0.4782/mean(Petal.Length)

#. R_squared =. cor(x, y ) =r  for our simple linear regtession
cor(Petal.Length, Petal.Width)

# remember the correlation test we have done
cor.test(iris$Petal.Length, iris$Petal.Width)



t.test(iris$Petal.Length)
#conf.int. for  the Petal.Length  mean 





mean(Petal.Length)

qt(.95, df=148)*sd(Petal.Length)/sqrt(150)


mean(Petal.Length) - qt(.95, df=148)*sd(Petal.Length)/sqrt(150)



step(lmModel)

attach(iris)
mean(Petal.Width) + qt(0.95, df=148)*sd(Petal.Width)/sqrt(150)

attach(iris)

qt(.95, df=148)









anova(lmModel)



t.value=seq(-43.39, 43.39, length=nrow(iris))
plot(x=t.value, y=dt(t.value, 148), type="l", xlim = c(-5, 5), xlab = "t", ylab="f(t)")

confint(lmModel)

# R^2 = 1- (Residual sum of squares)/(Total sum of squares)
1- 33.84475/430.48065
summary(lmModel)

anova(lmModel)

t.test(iris$Sepal.Length, iris$Petal.Length)

2.22994   + 0.05140





ggplot(data.frame(x = c(14.85, 43.39)), aes(x)) +
  stat_function(fun = dt, args =list(df =148)) +
  stat_function(fun = dt,   args =list(df =148),xlim = c(1.771500 ,5), geom = "area") +
xlim(-5,5)

par(mfrow=c(2,2)) # Plot in a layout with 2 rows and 2 columns
plot(lmModel)





length(Residual)

# residual =  Petal.Length - Predicted_Petal.Length
iris$Residual <- Petal.Length- predict(model)

head(iris)

(sem<- sd(iris$Residual)/sqrt(length(iris$Residual)))

c(mean(iris$Residual)-2*sem, mean(iris$Residual)+2*sem)



ggplot(iris)+
geom_boxplot( aes(as.factor(0), Residual))

ggplot(iris)+
geom_boxplot(aes(Species, Residual))

boxplot(iris$Residual)

summary(iris$Residual)
# same as the one we had above 
# Since the median deviance residual is close to zero, this means that our model is not biased in one direction 
#(i.e. the out come is neither over- nor underestimated).

# Lets plot these predicted values vs the residuals.
ggplot(iris)+
geom_point( aes( predict(model) , Residual), col="red", pch=2) +
geom_hline(yintercept=0)

ggplot() +
geom_histogram(aes(lmModel$residuals))



# exemple de multiple plot on the same graph
data <- data.frame(c(runif(30,1,50)))
ggplot(data, aes(data[,1])) +
    geom_histogram(aes(y = ..density..), binwidth = 2, fill = 'pink') +
    labs(x = 'Data', y = 'Density') +
    stat_function(fun = dnorm, 
        args = list(mean = mean(data[,1], na.rm = TRUE), 
                    sd = sd(data[,1], na.rm = TRUE)), 
        colour = 'black', size = 1) 

names(data)<- "v"

sd(data$v)

mean(data$v)

data <- data.frame(c(runif(30,1,50)))
ggplot(data, aes(data[,1])) +
    geom_histogram(aes(y = ..density..), binwidth = 3, fill = 'pink') +
    labs(x = 'Data', y = 'Density') +
    stat_function(fun = dnorm, 
        args = list(mean = mean(data[,1], na.rm = TRUE), 
                    sd = sd(data[,1], na.rm = TRUE)), 
        colour = 'black', size = 1) 

ggplot(lmModel, aes(lmModel$residuals)) +
geom_histogram(aes(y=..density..), fill="pink", binwidth = .1)+
stat_function(fun = dnorm, 
        args = list(mean = mean(lmModel$residuals), 
                    sd = sd(lmModel$residuals)), 
        colour = 'black', size = 1) +
 labs(x = 'Residuals', y = 'Density') 


x <- iris$Residual
h<-hist(x, breaks=10, col="red", xlab="Residual", main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=150)
#dnorm->density of normal dist
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
#mids ->the n cell midpoints.
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

names(iris)

#  Simplest way to do previous. Process


plot(lmModel$residuals, col="red", pch=2)


















coef(model)



ggplot( iris, aes( Petal.Width, Petal.Length))+
geom_point()+
geom_abline(intercept = model$coefficients[1], slope=model$coefficients[2], color="red", lwd=2)



eq = paste("y = ", round(model$coefficients[2], 2), "*x ", round(model$coefficients[1], 2))
           
ggplot( iris, aes( Petal.Width, Petal.Length))+
geom_point()+
geom_abline(intercept = model$coefficients[1], slope=model$coefficients[2], color="red", lwd=2)+
geom_hline(yintercept = mean(Petal.Length)) +
ggtitle( " LInear Regression ", subtitle = eq)

ggplot(iris, aes(Petal.Width,Petal.Length)) +
geom_point() +
stat_smooth(method = "lm", col = "dodgerblue3") +
theme(panel.background = element_rect(fill = "white"),
axis.line.x=element_line(),
axis.line.y=element_line()) +
ggtitle("Linear Model Fitted to Data")


ggplot(iris, aes(Sepal.Length, Petal.Width))+
  geom_point(size = 4,
             alpha = 0.5)+
  geom_smooth(method = "lm")


newDf<- c(1.5, 3.4, 5)
lmModel$coef[1] +lmModel$coef[2]*newDf
predict(lmModel, data.frame(Petal.Width=newDf))

library(broom)

lmModel %>%
    augment() %>% head()

head(predict(lmModel))

sum(lmModel$residuals)

lmModel %>%
    augment() %>% 
    summarise(Sum=sum(.resid))



cars <- mtcars
lmCarModel <- lm(mpg ~ wt, data = cars) # fit the model
cars$predicted <- predict(lmCarModel)   # Save the predicted values
cars$residuals <- residuals(lmCarModel) # Save the residual values
ggplot(cars, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = wt, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

summary(lmCarModel)

names(lmCarModel)



library(dplyr); library(broom); library(ggplot2)


    ggplot(lmCarModel) +
    geom_point(aes( predict(lmCarModel), residuals))

library(dplyr); library(broom)
lmCarModel %>% augment() %>% select(mpg, .fitted, .resid) %>% head()

head(residuals(lmCarModel));  head(predict(lmCarModel))

df<- lmCarModel %>% augment()
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_segment(aes(xend = hp, yend = df$.fitted)) +
  geom_point() +
  geom_point(aes(y = df$.fitted), shape = 1)

df<- lmCarModel %>% augment()
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = hp, yend = df$.fitted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = df$.fitted), shape = 1) +
  theme_bw()

library(tidyr)

mtcars %>% 
  gather(key = "iv", value = "x", -Sepal.Width) %>%
  ggplot(aes(x = x, y = Sepal.Width)) +
  geom_segment(aes(xend = x, yend = df$.fitted), alpha = .2) +
  geom_point(aes(color = df$.resid)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = df$.fitted), shape = 1) +
  facet_grid(~ iv, scales = "free_x") +
  theme_bw()



names(lmCarModel)

head(lmCarModel)

lmCarModel















library(ggplot2)
library(dplyr)

#Another alternative for lm graph
p <- ggplot(mtcars, aes(hp, qsec)) + geom_point()
 p + geom_smooth(method = "lm", se = FALSE) + facet_wrap(~cyl)

x< as.matrix(cbind(1, Petal.Length, Petal.Width))
y<- as.matrix(Petal.Length)
 solve(t(x)%*%x)%*%t(x)%*%y









#Generalized Linear Models
modelLM<- glm(vs ~ wt +disp, mtcars, family="binomial")

summary(modelLM)



dataPred<- data.frame(wt=2.32, disp=108)

predict(modelLM, dataPred, type="response")

install.packages("ResourceSelection")

library(ResourceSelection)

hoslem.test(mtcars$vs, fitted(modelLM))

head(mtcars)





A <- structure(list(numeracy = c(6.6, 7.1, 7.3, 7.5, 7.9, 7.9, 8, 
8.2, 8.3, 8.3, 8.4, 8.4, 8.6, 8.7, 8.8, 8.8, 9.1, 9.1, 9.1, 9.3, 
9.5, 9.8, 10.1, 10.5, 10.6, 10.6, 10.6, 10.7, 10.8, 11, 11.1, 
11.2, 11.3, 12, 12.3, 12.4, 12.8, 12.8, 12.9, 13.4, 13.5, 13.6, 
13.8, 14.2, 14.3, 14.5, 14.6, 15, 15.1, 15.7), anxiety = c(13.8, 
14.6, 17.4, 14.9, 13.4, 13.5, 13.8, 16.6, 13.5, 15.7, 13.6, 14, 
16.1, 10.5, 16.9, 17.4, 13.9, 15.8, 16.4, 14.7, 15, 13.3, 10.9, 
12.4, 12.9, 16.6, 16.9, 15.4, 13.1, 17.3, 13.1, 14, 17.7, 10.6, 
14.7, 10.1, 11.6, 14.2, 12.1, 13.9, 11.4, 15.1, 13, 11.3, 11.4, 
10.4, 14.4, 11, 14, 13.4), success = c(0L, 0L, 0L, 1L, 0L, 1L, 
0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 
1L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)), .Names = c("numeracy", 
"anxiety", "success"), row.names = c(NA, -50L), class = "data.frame")

attach(A)
names(A)

head(A)

mean(A$numeracy)

model1 <- glm(success ~ numeracy * anxiety, binomial)

summary(model1)

df.residual(model1)

anova(model1)

model_numeracy<- glm( success ~ numeracy, binomial)

model_anxiety<- glm( success ~ anxiety , binomial)

range(numeracy)

range(anxiety)



xnumeracy<- seq(0, 15, .01)
ynumeracy<- predict(model_numeracy, list(numeracy=xnumeracy), type="response")



plot(numeracy, success, pch = 16, xlab = "NUMERACY SCORE", ylab = "ADMISSION")
lines(xnumeracy, ynumeracy, col = "red", lwd = 2)

xanxiety <- seq(10, 20, 0.1)

yanxiety <- predict(model_anxiety, list(anxiety=xanxiety),type="response")

plot(anxiety, success, pch = 16, xlab = "ANXIETY SCORE", ylab = "SUCCESS")

lines(xanxiety, yanxiety, col= "blue", lwd = 2)

ggplot(cars,aes(x=as.factor(0),y=speed))+geom_boxplot()

curve(dnorm(x), -4, 4, col = "red")

ggplot(data.frame(x = c(-2, 4)), aes(x)) +
  stat_function(fun = dt, args =list(df =23)) +
  stat_function(fun = dt,   args =list(df =23),
                xlim = c(1.78,4),
                geom = "area") 

#  santosa و Iris  هذا جدول لعدد اصناف 
table( iris$Species)

(i.am.number<- 12)

data()

head(iris)

tail(iris)

str(iris)

levels(iris$Species)

?iris

nrow(iris)

attach(iris)
table(Species)

library(corrplot)

cor<- cor(Petal.Length, Petal.Width, Sepal.Length, Sepal.Width)

corr <- cor(iris)

x<-1:3; y<-101:103
mean(x); mean(y)
sd(x); sd(y)

cor(Petal.Length, Sepal.Width)





set.seed(1); n = 50; x1 = rnorm(n, 10, 3); x2 = rnorm(n, 15, 3); x3 = rnorm(n, 20, 3)
    dataframe = data.frame(x1,x2,x3)   # Three columns


    (datalong = stack(dataframe) )       # Two columns (long format)
    boxplot(dataframe, col=c("wheat4", "tan", "tan3"))

names(dataframe)

(anov = aov(values ~ ind, datalong))







str(mtcars)

attach(mtcars)

 plot(jitter(cyl, .2),  mpg, pch=19, col="darkgrey")

(cyls<- unique(cyl))

(n_groups<- length(cyl))

sample_mean<- rep(NA, n_groups)
cis<- matrix(nrow=n_groups, ncol=2)



for(i in 1:n_groups)  {
    #extract relevant Data
    rows<- which(cyl==cyls[i])
    observation<- mpg[rows]
    sample_mean[i]<-mean(observation) 
    stdev<- sd(observation)
    n<- length(observation)
    se_mean<- stdev/sqrt(n)
    cis[i, 1]<- sample_mean[i]- 2*se_mean
    cis[i, 2]<- sample_mean[i]+ 2*se_mean
}

sample_mean







 ts = replicate(1000,t.test(rnorm(10),rnorm(10))$statistic)

range(ts)

pts<-seq(-4.5, 4.5,length=100)
plot(pts,dt(pts,df=18),col='red',type='l')
lines(density(ts))

cubeFun <- function(x) {
    x^3 * 0.5
}

 ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
        stat_function(fun = cubeFun)


<video controls src="imgs/Introduction.mp4" />

ggplot(data.frame(x = c(-4, 4)), aes(x )) +
        stat_function(fun = pnorm)

ggplot(data.frame(x = c(-4, 4)), aes(x )) +
        stat_function(fun = dnorm)

 ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
        stat_function(fun = dt, args = list(df = 8))

head(iris)

require(maps)
france = map_data('world', region = 'France')
ggplot(france, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = 'white', colour = 'black')

curve(dnorm(x), xlim=c(-6,6))
curve(dnorm(x, mean=1, sd=2), col="red", add=TRUE)
pnorm(1.65)

set.seed(90546723)
Q <- rchisq(1e5,10)
hist(Q, freq=FALSE, breaks=100)
curve(dchisq(x,10),col="red",add=TRUE)

pchisq(21.78,10,lower.tail=FALSE)

curve(dchisq(x, 24), 0, 75, lwd=2, 
    ylab="PDF", xlab="Chi-sq", main="Density of CHISQ(24)")
 abline(h=0, col="green2"); abline(v=0, col="green2")
 abline(v=13.85, col="red", lwd=2, lty="dotted")
 abline(v = 5.67, lwd=2)


