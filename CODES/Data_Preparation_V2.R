library(dplyr)
library(data.table)
library(mice)

setwd("C:/Users/system5/Documents/StudyWork/Kaggle/House_Pricing/CODES/")

train = read.csv("../RAW/train.csv", header = TRUE)
test = read.csv("../RAW/test.csv", header = TRUE)

source("./lib.R")

train=subset(train,train$GrLivArea<=4000)

# overall summary
summary(train)
summary(test)

table(sapply(train,class))
table(sapply(test,class))

# Missing value imputation
train <- treat.missing.values(dat=train)
test <- treat.missing.values(dat=test)

# Binning some numeric variables and dropping the original cols
train$MSSubClass <- as.factor(train$MSSubClass)
test$MSSubClass <- as.factor(test$MSSubClass)
train <- data.table(train)
test <- data.table(test)

train <- bin.variables(dat=train)
test <- bin.variables(dat=test)

train <- misc.features(dat=train)
test <- misc.features(dat=test)

rmv.cols <- c("Id", "Street","Condition2", "YearBuilt", "MiscFeature",
              "GarageYrBlt", 'PoolQC','PoolArea',"YearRemodAdd", 
              "Utilities", "YrSold", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF",
              "MSSubClass", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "MoSold", 
              "WoodDeckSF", "OpenPorchSF", "X3SsnPorch")
rmv.cols <- c("Street","Condition2", "MiscFeature",
              "GarageYrBlt", 'PoolQC','PoolArea', "X1stFlrSF", "YearRemodAdd", 
              "YearBuilt", "Utilities", 'MoSold','HouseStyle')
              
dim(train);dim(test)
train[,(rmv.cols):=NULL]
test[,(rmv.cols):=NULL]

any(is.na(train))
any(is.na(test))

colnames(test)[unlist(lapply(colnames(test), function(x){any(is.na(test[,get(x)]))}))]

test_impute <- mice(test[,c("MSZoning","Exterior1st","Exterior2nd","Functional",
                            "SaleType"), with=F], m=1,  method = "cart", 
                    seed=123, maxit = 100, printFlag = F )
for(n in names(test_impute$imp)){
  x = test_impute$imp[[n]][,1]
  test[is.na(get(n)),(n):=x]
}
test$num_ext_materials <- ifelse(as.character(test$Exterior1st)==as.character(test$Exterior2nd),1,2)

# Merging levels
train <- combine.levels(dat=train)
test <- combine.levels(dat=test)




