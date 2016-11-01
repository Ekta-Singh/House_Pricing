library(dplyr)
library(data.table)
library(mice)

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

# Binning some numeric variables and dropping the original cols
train$HouseStyle <- as.factor(train$HouseStyle)
test$HouseStyle <- as.factor(test$HouseStyle)
train <- data.table(train)
test <- data.table(test)

train <- bin.variables(dat=train)
test <- bin.variables(dat=test)

train <- misc.features(dat=train)
test <- misc.features(dat=test)

# removing levels not present in test
train <- subset(train, RoofMatl %in% c("CompShg", "Tar&Grv", "WdShake", "WdShngl"))
train$RoofMatl <- as.factor(as.character(train$RoofMatl))

train <- subset(train, !(HouseStyle %in% c("2.5Fin")))
train$HouseStyle <- as.factor(as.character(train$HouseStyle))

train <- subset(train, !(Exterior1st %in% c("ImStucc","Stone")))
train$Exterior1st <- as.factor(as.character(train$Exterior1st))

train <- subset(train, !(Exterior2nd %in% c("Other")))
train$Exterior2nd <- as.factor(as.character(train$Exterior2nd))

train <- subset(train, !(Electrical %in% c("Mix")))
train$Electrical <- as.factor(as.character(train$Electrical))

train <- subset(train, !(GarageQual %in% c("Ex")))
train$GarageQual <- as.factor(as.character(train$GarageQual))


# Removing Columns
rmv.cols <- c("Street","Condition2", "MiscFeature", "BsmtFinSF2", "BsmtUnfSF",
              "GarageYrBlt", 'PoolQC','PoolArea', "X2ndFlrSF", "YearRemodAdd", 
              "Utilities", 'MoSold','MSSubClass',"Heating","OverallCond")

dim(train);dim(test)
train[,(rmv.cols):=NULL]
test[,(rmv.cols):=NULL]

any(is.na(train))
any(is.na(test))

# Imputing Missing values in test
colnames(test)[unlist(lapply(colnames(test), function(x){any(is.na(test[,get(x)]))}))]

test_impute <- mice(test[,c("MSZoning","Exterior1st","Exterior2nd","Functional",
                            "SaleType"), with=F], m=1,  method = "cart", 
                    seed=123, maxit = 100, printFlag = F )
for(n in names(test_impute$imp)){
  x = test_impute$imp[[n]][,1]
  test[is.na(get(n)),(n):=x]
}
test$num_ext_materials <- ifelse(as.character(test$Exterior1st)==as.character(test$Exterior2nd),1,2)
test$Partial_Old <- ifelse(((test$SaleCondition=="Partial" | test$SaleCondition=="Abnorml") &
                             test$YearBuilt<=1990),1,0)

train[Time_Since_Remodel<0,AgeofHouse:=50]
train[Time_Since_Remodel<0,Time_Since_Remodel:=25]
test[Time_Since_Remodel<0,AgeofHouse:=50]
test[Time_Since_Remodel<0,Time_Since_Remodel:=25]

test[is.na(AgeofHouse), AgeofHouse:=25]

train[,YearBuilt:=NULL]
test[,YearBuilt:=NULL]

# Merging levels
train <- combine.levels(dat=train)
test <- combine.levels(dat=test)




