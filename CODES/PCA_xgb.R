library(MASS)
require(MASS)
library(caret)

library(randomForest)
library(mlbench)
library(dummies)

library(xgboost)


dat = read.csv("/Users/ekta/Desktop/Kaggle/House Prices/train.csv", header = TRUE)
test = read.csv("/Users/ekta/Desktop/Kaggle/House Prices/test.csv", header = TRUE)
 
#Removing properties with area greater than 400 sq feet
dat=subset(dat,dat$GrLivArea<=4000)
# Remove utilities and Street variable
drops=c('Street','Utilities','Id')
dat1=dat[, !(names(dat) %in% drops) ]

dat1[is.na(dat1[,'LotFrontage']),'LotFrontage']=median(dat1[,'LotFrontage'],na.rm=TRUE)
dat1$LotFrontage[dat1$LotFrontage==313]=median(dat1[,'LotFrontage'],na.rm=TRUE)

plot(dat1['LotFrontage'])
dat1$Alley=as.character(dat1$Alley)
dat1[is.na(dat1[,'Alley']),'Alley']='Missing'
dat1$Alley=as.factor(dat1$Alley)

dat1[is.na(dat1[,'MasVnrType']),'MasVnrType']='None'
dat1[is.na(dat1[,'MasVnrArea']),'MasVnrArea']=0

dat1$BsmtQual=as.character(dat1$BsmtQual)
dat1[is.na(dat1[,'BsmtQual']),'BsmtQual']='No Basement'
dat1$BsmtQual=as.factor(dat1$BsmtQual)

dat1$BsmtCond=as.character(dat1$BsmtCond)
dat1[is.na(dat1[,'BsmtCond']),'BsmtCond']='No Basement'
dat1$BsmtCond=as.factor(dat1$BsmtCond)

dat1$BsmtExposure=as.character(dat1$BsmtExposure)
dat1[is.na(dat1[,'BsmtExposure']),'BsmtExposure']='No Basement'
dat1$BsmtExposure=as.factor(dat1$BsmtExposure)

dat1$BsmtFinType1=as.character(dat1$BsmtFinType1)
dat1[is.na(dat1[,'BsmtFinType1']),'BsmtFinType1']='No Basement'
dat1$BsmtFinType1=as.factor(dat1$BsmtFinType1)

dat1$BsmtFinType2=as.character(dat1$BsmtFinType2)
dat1[is.na(dat1[,'BsmtFinType2']),'BsmtFinType2']='No Basement'
dat1$BsmtFinType2=as.factor(dat1$BsmtFinType2)

dat1[is.na(dat1[,'Electrical']),'Electrical']='SBrkr'

dat1$FireplaceQu=as.character(dat1$FireplaceQu)
dat1[is.na(dat1[,'FireplaceQu']),'FireplaceQu']='No Fireplace'
dat1$FireplaceQu=as.factor(dat1$FireplaceQu)

dat1$GarageType=as.character(dat1$GarageType)
dat1[is.na(dat1[,'GarageType']),'GarageType']='No Garage'
dat1$GarageType=as.factor(dat1$GarageType)

dat1$GarageCond=as.character(dat1$GarageCond)
dat1[is.na(dat1[,'GarageCond']),'GarageCond']='No Garage'
dat1$GarageCond=as.factor(dat1$GarageCond)

dat1$GarageFinish=as.character(dat1$GarageFinish)
dat1[is.na(dat1[,'GarageFinish']),'GarageFinish']='No Garage'
dat1$GarageFinish=as.factor(dat1$GarageFinish)

dat1$GarageQual=as.character(dat1$GarageQual)
dat1[is.na(dat1[,'GarageQual']),'GarageQual']='No Garage'
dat1$GarageQual=as.factor(dat1$GarageQual)

dat1$Fence=as.character(dat1$Fence)
dat1[is.na(dat1[,'Fence']),'Fence']='No Fence'
dat1$Fence=as.factor(dat1$Fence)


dat1$PoolQC_f=ifelse(is.na(dat1$PoolQC)==TRUE,0,1)
dat1$MiscFeature_f=ifelse(is.na(dat1$MiscFeature)==TRUE,0,1)

dat1$Condition2_f=ifelse(dat1$Condition2=='Norm',1,0)

dat1$YearBuiltCombined= ifelse(dat1$YearBuilt<=1900,'Older than 1900',
                               ifelse(dat1$YearBuilt<=1910,'1900-1910',
                               ifelse(dat1$YearBuilt<=1920,'1910-1920',
                               ifelse(dat1$YearBuilt<=1930,'1920-1930',
                               ifelse(dat1$YearBuilt<=1940,'1930-1940',
                               ifelse(dat1$YearBuilt<=1950,'1940-1950',
                               ifelse(dat1$YearBuilt<=1960,'1950-1960',
                               ifelse(dat1$YearBuilt<=1970,'1960-1970',
                               ifelse(dat1$YearBuilt<=1980,'1970-1980',
                               ifelse(dat1$YearBuilt<=1990,'1980-1990',
                               ifelse(dat1$YearBuilt<=1995,'1990-1995',
                               ifelse(dat1$YearBuilt<=2000,'1995-2000',
                               ifelse(dat1$YearBuilt<=2003,'2000-2003',
                               ifelse(dat1$YearBuilt<=2006,'2003-2006',
                               ifelse(dat1$YearBuilt<=2008,'2006-2008',
                               ifelse(dat1$YearBuilt<=2010,'2008-2010'))))))))))))))))
dat1$YearBuiltCombined=as.factor(dat1$YearBuiltCombined)

dat1$YearRemodeDecile = cut( dat1$YearRemodAdd, breaks=20)

dat1$MoSoldQtr=ifelse(dat1$MoSold<=3,'Q1',
               ifelse(dat1$MoSold<=6,'Q2',
               ifelse(dat1$MoSold<=9,'Q3',
               ifelse(dat1$MoSold<=12,'Q4'))))
dat1$MoSoldQtr=as.factor(dat1$MoSoldQtr)

drops=c('YearBuilt','MiscFeature','PoolQC','PoolArea','YearRemodAdd','GarageYrBlt','Condition2','MoSold')
dat1=dat1[, !(names(dat1) %in% drops) ]

## Merging levels
levels(dat1$LotShape)=c("IR1", "IR2_3" ,"IR2_3" ,"Reg")
levels(dat1$LotConfig)=c("Corner",  "CulDSac", "FR2","CulDSac","Inside")
levels(dat1$RoofStyle)=c("Flat","Gable" ,"Gambrel", "Mansard_Hip","Mansard_Hip", "Gable")
levels(dat1$RoofMatl)=c("CompShg", "CompShg", "CompShg" , "CompShg" ,"CompShg", "Tar&Grv","WdShake" ,"WdShngl")
levels(dat1$Heating)=c("Grav" ,"GasA",  "GasW" , "Grav" , "GasW" , "Wall")
levels(dat1$Electrical)=c("FuseA" ,"FuseF" ,"FuseP", "SBrkr" ,"SBrkr")
levels(dat1$Exterior1st)=c("AsbShng", "AsbShng", "BrkFace", "BrkFace", "AsbShng","CemntBd","HdBoard","Stucco","MetalSd", "Plywood",
                          "MetalSd","Stucco", "VinylSd","Wd Sdng","WdShing")
levels(dat1$Exterior2nd)=c("AsbShng", "AsbShng", "BrkFace", "BrkFace", "AsbShng",  "CmentBd", "HdBoard", "Stucco", "MetalSd", "VinylSd",  
                           "Plywood", "MetalSd",   "Stucco",  "VinylSd", "Wd Sdng", "Wd Shng")

levels(dat1$ExterCond)=c("Gd", "Fa", "Gd", "Fa", "TA")
levels(dat1$HeatingQC)=c("Ex", "Fa", "Gd", "Fa", "TA")
levels(dat1$Foundation)=c("BrkTil","CBlock","PConc","Slab","CBlock","CBlock")
levels(dat1$Functional)=c("Maj","Maj","Min1","Min2","Mod","Maj","Typ")
levels(dat1$SaleType)=c("COD","Con","Con","Con","Con","WD" ,"New","WD","WD" )
levels(dat1$Neighborhood)=c("Blmngtn", "Mitchel", "BrDale",  "BrkSide", "ClearCr", "CollgCr", "Crawfor", "Edwards", "Gilbert", "IDOTRR" 
                            ,"MeadowV" ,"Mitchel", "NAmes",   "NoRidge", "NPkVill", "NridgHt", "NWAmes",  "OldTown", "Sawyer",  "SawyerW"
                            ,"Somerst", "StoneBr", "SWISU",   "Timber",  "Veenker")
levels(dat1$Condition1)=c("Artery","Feedr","Norm","Pos","Pos","RR","RR","RR","RR")
i=0
for (col in names(train)){
  print(paste(col,sum(is.na(train$col))))
}
## Split into val and train 
set.seed(123)
indx = sample(1:nrow(dat1),0.2*nrow(dat1),replace = F)
val <- dat1[indx,]
train <- dat1[-indx,]

## Cover factor to dummy variables
train2=dummy.data.frame(train,names=names(which(sapply(train,class)=='factor')))
val2=dummy.data.frame(val,names=names(which(sapply(val,class)=='factor')))

## Apply PCA
train2=train2[, !(names(train2) %in% 'SalePrice')]
val2=val2[, !(names(val2) %in% 'SalePrice')]
prin_comp <- prcomp(train2, scale. = T)

#Variance of each component
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

## Using 250 var's out of 324
train.data <- data.frame(SalePrice = train$SalePrice, prin_comp$x)
train.data=train.data[,1:150]

#transform val into PCA
test.data <- predict(prin_comp, newdata = val2)
test.data <- data.frame(SalePrice = val$SalePrice,test.data)
test.data <-  test.data[,1:150]

xgb_grid = expand.grid(nrounds = c(300,500,600,800,1000,1200,1500),
                       eta = c(0.005, 0.01, 0.02, 0.03, 0.05),
                       max_depth = c(5,6,7,8),
                       colsample_bytree=c(0.65,0.7,0.75,0.8))


y_train <- train.data[["SalePrice"]]
x_train=train.data[,!(names(train.data) %in% 'SalePrice')]

y_val <- test.data[["SalePrice"]]
x_val=test.data[,!(names(test.data) %in% 'SalePrice')]


rmse_grid_search <- apply(xgb_grid, 1, function(parameterList){
  
  param <- list(max_depth = parameterList[3], 
                eta = parameterList[2], 
                silent = 1,
                objective="reg:linear",
                eval_metric="rmse",
                min_child_weight = 10,
                colsample_bytree = parameterList[4],
                base_score =0)
  
  
  model_xgb <- xgboost(data=as.matrix(x_train),label = y_train, nrounds = parameterList[1],
                       params = param, verbose = 0)
  preds_xgb_test = predict(model_xgb,newdata = as.matrix(x_val))
  rmse_xgb_test = RMSE(y_val,preds_xgb_test)
  
  preds_xgb_train = predict(model_xgb,newdata = as.matrix(x_train))
  rmse_xgb_train = RMSE(y_train,preds_xgb_train)
  
  
  return(data.frame(rmse_xgb_train=rmse_xgb_train,
                    rmse_xgb_test=rmse_xgb_test))
  
})
x_train=x_train[, c('PC1','PC3','PC4','PC5','PC6','PC10','PC2','PC9','PC12','PC7','PC21','PC18',
                    'PC131','PC32','PC11','PC8','PC31','PC14','PC143','PC25')]
model_xgb <- xgboost(data=as.matrix(x_train),label = y_train, nrounds = 500,
                     params = list(max_depth = 6, 
                                   eta = 0.05, 
                                   silent = 1,
                                   objective="reg:linear",
                                   eval_metric="rmse",
                                   min_child_weight = 10,
                                   colsample_bytree = 0.7,
                                   base_score =0)
                     , verbose = 1)
preds_xgb_test = predict(model_xgb,newdata = as.matrix(x_val))
rmse_xgb_test = RMSE(y_val,preds_xgb_test)

preds_xgb_train = predict(model_xgb,newdata = as.matrix(x_train))
rmse_xgb_train = RMSE(y_train,preds_xgb_train)

print(rmse_xgb_train)
print(rmse_xgb_test)
