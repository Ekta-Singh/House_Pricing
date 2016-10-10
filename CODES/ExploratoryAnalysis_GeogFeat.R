library(data.table)
library(ggplot2)

###############################
setwd("C:\\Users\\system5\\Documents\\StudyWork\\Kaggle\\House_Pricing\\CODES/")
train <- read.csv("../RAW/train.csv")

head(train)
sapply(train,class)
summary(train)

train$MSSubClass <- as.factor(as.character(train$MSSubClass))
