library(MASS)
require(MASS)
library(caret)
library(car)
library(hydroGOF)
library(e1071)

library(mlbench)
library(dummies)
library(MVN)
library(glmnet)
library(pls)


dat = read.csv("/Users/ekta/Desktop/Kaggle/House Prices/train.csv", header = TRUE)
test = read.csv("/Users/ekta/Desktop/Kaggle/House Prices/test.csv", header = TRUE)

#Removing properties with area greater than 400 sq feet
dat=subset(dat,dat$GrLivArea<=4000)
# Remove utilities and Street variable
drops=c('Street','Utilities','Id')
dat1=dat[, !(names(dat) %in% drops) ]

dat1$SalePrice=log(dat1$SalePrice+1)

dat1$MSSubClass=as.factor(dat1$MSSubClass)

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

"dat1$YearBuiltCombined= ifelse(dat1$YearBuilt<=1900,'Older than 1900',
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
                        ifelse(dat1$YearBuilt<=2005,'2003-2005',
                        ifelse(dat1$YearBuilt<=2007,'2005-2007',
                        ifelse(dat1$YearBuilt<=2010,'2007-2010',
                        ifelse(dat1$YearBuilt==2010,'2010')))))))))))))))))"

#dat1$YearBuiltCombined=as.factor(dat1$YearBuiltCombined)

dat1$AgeofHouse= dat1$YrSold-dat1$YearBuilt
drops=c('YearBuilt')
dat1=dat1[, !(names(dat1) %in% drops) ]

dat1$YearRemodeDecile = cut( dat1$YearRemodAdd, breaks=20)

dat1$MoSoldQtr=ifelse(dat1$MoSold<=3,'Q1',
               ifelse(dat1$MoSold<=6,'Q2',
               ifelse(dat1$MoSold<=9,'Q3',
               ifelse(dat1$MoSold<=12,'Q4'))))
dat1$MoSoldQtr=as.factor(dat1$MoSoldQtr)

drops=c('MiscFeature','PoolQC','PoolArea','YearRemodAdd','GarageYrBlt','Condition2','MoSold','HouseStyle')
dat1=dat1[, !(names(dat1) %in% drops) ]

#dat1$LotArea_f=ifelse(dat1$LotArea>=35000,35000,dat1$LotArea)

# transform excessively skewed features with log(x + 1)

logTransformCol=c('LotFrontage','MasVnrArea','BsmtFinSF2','BsmtUnfSF','OpenPorchSF','EnclosedPorch','X3SsnPorch','ScreenPorch','GrLivArea')

for(x in logTransformCol) {
  dat1[[x]] <- log(dat1[[x]] + 1)
  }


## Dropping perfectly collinear variables
dat2=dummy.data.frame(dat1,names=names(which(sapply(dat1,class)=='factor')))

drops=c('MoSoldQtrQ4','YearRemodeDecile(2007,2010]','YearBuiltCombinedOlder than 1900','SaleConditionPartial',
        'SaleTypeWD','FenceNo Fence','PavedDriveY','GarageCondNo Garage','GarageCondTA','GarageQualTA','GarageQualNo Garage',
        'GarageFinishNo Garage','FireplaceQuTA','FunctionalTyp','KitchenQualTA','X1stFlrSF','ElectricalSBrkr',
        'CentralAirY','HeatingQCTA','HeatingWall','BsmtFinType2No Basement','BsmtFinType1No Basement','BsmtExposureNo Basement',
        'BsmtCondNo Basement','FoundationWood','ExterCondPo','ExterQualTA','MasVnrTypeStone','Exterior2ndWd Shng',
        'Exterior2ndCBlock','Exterior1stWdShing','RoofMatlWdShngl','RoofStyleShed','Condition1RRNn',
        'NeighborhoodVeenker','LandSlopeSev','LotConfigInside','LandContourLvl','LotShapeReg','AlleyPave',
        'MSZoningRM','BsmtFinType1Unf','BsmtQualTA','GarageFinishUnf','GarageTypeNo Garage','TotalBsmtSF','BsmtCondTA',
        'Exterior1stAsphShn','BsmtExposureNo','HeatingFloor','HeatingQCPo','FunctionalSev','MSSubClass190',
        'BldgTypeTwnhsE','BldgType2fmCon','BldgTypeDuplex')

dat2=dat2[, !(names(dat2) %in% drops) ]

## Regression

#Split train, test
smp_size <- floor(0.70 * nrow(dat2))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dat2)), size = smp_size)
train <- dat2[train_ind, ]
test <- dat2[-train_ind, ]
test <- test[,!(names(test) %in% 'SalePrice')]

## Linear regression
model=lm(SalePrice~.,train)
pred_test=predict.lm(model,test)
pred_train=predict.lm(model,train[,!(names(train) %in% 'SalePrice')])

test_rmse=RMSE(dat2[-train_ind, ]$SalePrice,pred_test,wt=1) #21099
train_rmse=RMSE(dat2[train_ind, ]$SalePrice,pred_train,wt = 1) #17766

test_rmse=rmse(dat2[-train_ind, ]$SalePrice,pred_test) #21099
train_rmse=rmse(dat2[train_ind, ]$SalePrice,pred_train) #17766

residual=resid(model)
test_rmse=rmse(exp(dat2[-train_ind, ]$SalePrice),exp(pred_test))
train_rmse=rmse(exp(train$SalePrice),exp(pred_train))

## Ridge regression
grid=seq(1,0,-0.001)
set.seed(1)
ridge.mod=glmnet(as.matrix(train[,!(names(train) %in% 'SalePrice')]),train$SalePrice,alpha=0, lambda =grid ,
                 thresh =1e-12)
cv.out=cv.glmnet(as.matrix(train[,!(names(train) %in% 'SalePrice')]),train$SalePrice,alpha=0)
plot(cv.out)
bestlam =cv.out$lambda.min #0.09013617

ridge.pred_test=predict (ridge.mod ,s=bestlam ,newx=as.matrix(test))
ridge.pred_train=predict (ridge.mod ,s=bestlam ,newx=as.matrix(train[,!(names(train) %in% 'SalePrice')]))

test_rmse_ridge=RMSE(dat2[-train_ind, ]$SalePrice,ridge.pred_test[,1],wt=1) #21099
train_rmse_ridge=RMSE(dat2[train_ind, ]$SalePrice,ridge.pred_train[,1],wt = 1) #17766

test_rmse_ridge=rmse(exp(dat2[-train_ind, ]$SalePrice),exp(ridge.pred_test[,1])) #21099
train_rmse_ridge=rmse(exp(dat2[train_ind, ]$SalePrice),exp(ridge.pred_train[,1])) #17766

test$pred=exp(ridge.pred_test[,1])
test$SalePrice=exp(dat2[-train_ind, ]$SalePrice)
test$resid=test$SalePrice-test$pred

write.csv(test,'testError.csv')

## Lasso Regression
set.seed(1)
lasso.mod=glmnet(as.matrix(train[,!(names(train) %in% 'SalePrice')]),train$SalePrice,alpha=1, lambda =grid ,
                 thresh =1e-12)
cv.out_lasso=cv.glmnet(as.matrix(train[,!(names(train) %in% 'SalePrice')]),train$SalePrice,alpha=1)
plot(cv.out_lasso)
bestlam_lasso=cv.out_lasso$lambda.min #0.004

lasso.pred_test=predict (lasso.mod ,s=0.004 ,newx=as.matrix(test))
lasso.pred_train=predict (lasso.mod ,s=0.004 ,newx=as.matrix(train[,!(names(train) %in% 'SalePrice')]))

test_rmse_lasso=rmse(exp(dat2[-train_ind, ]$SalePrice),exp(lasso.pred_test[,1])) #40133
train_rmse_lasso=rmse(exp(dat2[train_ind, ]$SalePrice),exp(lasso.pred_train[,1])) #44319

'Clearly ridge is better than lasso'

## PCR (Principal component regression)
set.seed(1)
noVariance=names(train[,colSums(train) %in% c(1,2)]) #columns withno variance
pcr.fit <- pcr(SalePrice~.,data=train[,!(names(train) %in% noVariance)],scale=TRUE,validation='CV')
validationplot(pcr.fit,val.type='MSEP')

pcr.pred_test=predict(pcr.fit,test,ncomp=162)
pcr.pred_train=predict(pcr.fit,train[,!(names(train) %in% 'SalePrice')],ncomp=162)

test_rmse_pcr=rmse(exp(dat2[-train_ind, ]$SalePrice),exp(as.matrix(pcr.pred_test)[,1])) #21485
train_rmse_pcr=rmse(exp(dat2[train_ind, ]$SalePrice),exp(as.matrix(pcr.pred_train)[,1])) #19192

## Outlier Detection 
result <- mvOutlier(as.matrix(dat2), qqplot = TRUE, method = "quan")
labelsO<-rownames(result$outlier)[result$outlier[,2]==TRUE]
xcoord<-result$outlier[result$outlier[,2]==TRUE,1]

#recalculate chi-squared values for ranks 50 and 49 (i.e., p=(size:(size-n.outliers + 1))-0.5)/size and df = n.variables = 3
chis = qchisq(((50:49)-0.5)/50,3)
text(xcoord,chis,label=labelsO)



