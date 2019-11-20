college<- read.csv("https://raw.githubusercontent.com/datasciencebh/labs/master/colleges.csv")
install.packages("GGally")
library(ggplot2)
library(GGally)
ggpairs(college[,c( "admission_rate", "sat_avg", "tuition", "faculty_salary_avg", "median_debt", "undergrads") ])
attach(college)
cor(faculty_salary_avg, sat_avg)
install.packages("tidyverse")
install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
library(dplyr)
library(tidyverse)
theme_set(theme_bw())
library(ggplot2)
library(caret)
# بيانات تدريب واختبار
set.seed(123)
training.samples <- college$tuition %>%
  # مجموعة تدريب %80 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- college[training.samples, ]
# مجموعة اختبار
test.data <- college[-training.samples, ]