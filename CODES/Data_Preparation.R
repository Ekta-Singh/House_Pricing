library(dplyr)
library(data.table)

setwd("C:/Users/system5/Documents/StudyWork/Kaggle/House_Pricing/CODES/")

train = read.csv("../RAW/train.csv", header = TRUE)
test = read.csv("../RAW/test.csv", header = TRUE)

source("./lib.R")

# overall summary
summary(train)
summary(test)

table(sapply(train,class))
table(sapply(test,class))

# Missing value imputation
train <- treat.missing.values(dat=train)
test <- treat.missing.values(dat=test)

# Dropping columns and final modifications
train$MSSubClass <- as.factor(train$MSSubClass)
test$MSSubClass <- as.factor(test$MSSubClass)
train <- data.table(train)
test <- data.table(test)
rmv.cols <- c("Id", "Street","Condition2", "YearBuilt", "MiscFeature", "GarageYrBlt", 
              "YearRemodAdd", "Utilities")
dim(train);dim(test)
train[,(rmv.cols):=NULL]
test[,(rmv.cols):=NULL]

any(is.na(train))
any(is.na(test))
