library(MASS)
require(MASS)
require(xgboost)
library(car)

dat = read.csv("/Users/ekta/Desktop/Kaggle/House Prices/train.csv", header = TRUE)

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

dat1$YearBuiltDecile = cut( dat1$YearBuilt, breaks=25)
dat1$YearRemodeDecile = cut( dat1$YearRemodAdd, breaks=20)

drops=c('YearBuilt','MiscFeature','PoolQC','PoolArea','YearRemodAdd','GarageYrBlt')
dat1=dat1[, !(names(dat1) %in% drops) ]

drops=c('GarageFinish','GarageQual','Exterior2nd','BsmtFinType1','GarageCond','BsmtCond','TotalBsmtSF','GrLivArea')
dat2=dat1[,!(names(dat1) %in% drops)]

  
i=0
for (col in names(dat1)){
  print(paste(col,sum(is.na(dat1$col))))
}
fit <- lm(SalePrice~., data=dat2)

#lam = seq(0,1000,len=5)
#ridgefits=lm.ridge(SalePrice~.,data=dat2,lambda =lam)

xgb_cv <- xgb.cv(data = data.matrix(dat1[,-72]), 
               label = dat1$SalePrice, 
               eta = 0.05,
               nfold=5,
               max_depth = 6, 
               nround=300, 
               subsample = 0.6,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "rmse",
               objective = "reg:linear",
               nthread = 3,
               verbose=TRUE)

