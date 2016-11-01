treat.missing.values <- function(dat){

  dat1=dat
  
  dat1[is.na(dat1[,'LotFrontage']),'LotFrontage']=median(dat1[,'LotFrontage'],na.rm=TRUE)
  dat1$LotFrontage[dat1$LotFrontage==313]=median(dat1[,'LotFrontage'],na.rm=TRUE)
  
  #dat1[is.na(dat1$MSZoning), "MSZoning"]="RL"
  #plot(dat1['LotFrontage'])
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
  
  dat1[is.na(dat1[,'TotalBsmtSF']),'TotalBsmtSF']=0
  dat1[is.na(dat1[,'BsmtFinSF1']),'BsmtFinSF1']=0
  dat1[is.na(dat1[,'BsmtFinSF2']),'BsmtFinSF2']=0
  dat1[is.na(dat1[,'BsmtUnfSF']),'BsmtUnfSF']=0
  
  dat1[is.na(dat1[,'BsmtFullBath']),'BsmtFullBath']=0
  dat1[is.na(dat1[,'BsmtHalfBath']),'BsmtHalfBath']=0
  
  dat1[is.na(dat1[,'Electrical']),'Electrical']='SBrkr'
  
  #dat1[is.na(dat1$Exterior1st), "Exterior1st"]="Wd Sdng"
  #dat1[is.na(dat1$Exterior2nd), "Exterior2nd"]="Wd Sdng"
  
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
  
  dat1[is.na(dat1[,'GarageCars']),'GarageCars']=0
  dat1[is.na(dat1[,'GarageArea']),'GarageArea']=0
  
  dat1$Fence=as.character(dat1$Fence)
  dat1[is.na(dat1[,'Fence']),'Fence']='No Fence'
  dat1$Fence=as.factor(dat1$Fence)
  
  dat1[is.na(dat1[,'KitchenQual']),'KitchenQual']= "TA"
  
  dat1$PoolQC_f=ifelse(is.na(dat1$PoolQC)==TRUE,0,1)
  dat1$MiscFeature_f= as.factor(ifelse(is.na(dat1$MiscFeature)==TRUE,0,1))
  #dat1$Condition2_f=ifelse(dat1$Condition2=='Norm',1,0)
  
  return (dat1)
}

RMSE <- function(act, pred,wt){
  sqrt(mean(((act - pred)*wt)^2, na.rm=T))
}

combine.levels <- function(dat){
  levels(dat$LotShape)=c("IR1", "IR2_3" ,"IR2_3" ,"Reg")
  levels(dat$RoofStyle)=c("Flat","Gable" ,"Gambrel", "Mansard_Hip","Mansard_Hip", "Shed")
  levels(dat$RoofMatl)=c("CompShg", "CompShg", "CompShg" , "CompShg" ,"CompShg", "Tar&Grv","WdShake" ,"WdShngl")
  levels(dat$Heating)=c("Others" ,"Gas",  "Gas" , "Others" , "Others" , "Others")
  levels(dat$Electrical)=c("FuseA" ,"FuseF" ,"FuseP", "SBrkr" ,"SBrkr")
  #levels(dat)
  # levels(dat$GarageQual)
  # levels(dat$GarageCond)
  return (dat)
}

bin.variables <- function(dat){
  # dat$YearBuilt_f=ifelse(dat$YearBuilt<=1900,'Less than 1900',
  #                        ifelse(dat$YearBuilt<=1910, '1900-1910',
  #                               ifelse(dat$YearBuilt<=1920, '1910-1920',
  #                                      ifelse(dat$YearBuilt<=1930, '1920-1930',
  #                                             ifelse(dat$YearBuilt<=1940, '1930-1940',
  #                                                    ifelse(dat$YearBuilt<=1950, '1940-1950',
  #                                                           ifelse(dat$YearBuilt<=1960, '1950-1960',
  #                                                                  ifelse(dat$YearBuilt<=1970, '1960-1970',
  #                                                                         ifelse(dat$YearBuilt<=1980, '1970-1980',
  #                                                                                ifelse(dat$YearBuilt<=1990, '1980-1990',
  #                                                                                       ifelse(dat$YearBuilt<=2000, '1990-2000',
  #                                                                                              ifelse(dat$YearBuilt<=2002, '2000-2002',
  #                                                                                                     ifelse(dat$YearBuilt<=2004, '2002-2004',
  #                                                                                                            ifelse(dat$YearBuilt<=2006, '2004-2006',
  #                                                                                                                   ifelse(dat$YearBuilt<=2008, '2006-2008', '2008-2010')))))))))))))))  # 
  # dat$YearBuilt_f = as.factor(dat$YearBuilt_f)
  dat$YearRemodeDecile = as.factor(cut(dat$YearRemodAdd, breaks=c(1925,seq(1950,2020,by = 20)), labels=F))
  dat$MoSoldQtr=ifelse(dat$MoSold<=3,'Q1',
                        ifelse(dat$MoSold<=6,'Q2',
                               ifelse(dat$MoSold<=9,'Q3',
                                      ifelse(dat$MoSold<=12,'Q4'))))
  dat$MoSoldQtr=as.factor(dat$MoSoldQtr)
  return (dat)
}

misc.features <- function(dat){
  
  dat$AgeofHouse= dat$YrSold-dat$YearBuilt
  # dat$AgeofHouse <- as.factor(cut(dat$AgeofHouse,breaks = c(0,5,10,20,50,80,150),
  #                                 include.lowest = T))
  dat$Time_Since_Remodel= dat$YrSold-dat$YearRemodAdd
  dat$Overall_Quality_Cond <- dat$OverallQual*dat$OverallCond
  
  dat$num_ext_materials <- ifelse(as.character(dat$Exterior1st)==as.character(dat$Exterior2nd),1,2)
  dat$num_basmt_materials <- ifelse(dat$BsmtFinType1=="No Basement",-1,
                                    ifelse(dat$BsmtFinType1=="Unf",0,
                                           ifelse(as.character(dat$BsmtFinType1)==
                                                    as.character(dat$BsmtFinType2),1,2)))

  #dat$perc_2nd_1st_floor <- dat$X2ndFlrSF/dat$X1stFlrSF
  #dat$perc_1st_tot_floor <- dat$X1stFlrSF/dat$GrLivArea
  #dat$perc_unf_bsmt <- ifelse(dat$TotalBsmtSF==0,-1,dat$BsmtUnfSF/dat$TotalBsmtSF)
  
  cols.ratings <- c("ExterQual","ExterCond","BsmtQual", "BsmtCond", "HeatingQC", "KitchenQual", 
                    "FireplaceQu","GarageQual", "GarageCond")
  dat$num_excellents <- apply(dat[,cols.ratings,with=F],1, function(x) length(which(x=="Ex")))
  dat[num_excellents<3,num_excellents:=0]
  dat[num_excellents>=3,num_excellents:=1]
  dat$num_excellents <- as.factor(dat$num_excellents)
  
  logTransformCol=c('LotFrontage','MasVnrArea','TotalBsmtSF','OpenPorchSF','EnclosedPorch','X3SsnPorch',
                    "BsmtFinSF1", "X1stFlrSF", 'ScreenPorch','GrLivArea')
  for(x in logTransformCol){
    dat[[x]] <- log(dat[[x]] + 1)
  }
  
  dat$FAR <- (dat$GrLivArea)/dat$LotArea
  dat$Partial_Old <- ifelse(((dat$SaleCondition=="Partial" | dat$SaleCondition=="Abnorml") &
                               dat$YearBuilt<=1990),1,0)
  
  dat$Partial_Quality <- ifelse(((dat$SaleCondition=="Partial" | dat$SaleCondition=="Abnorml") &
                                   (dat$OverallQual <=6 | dat$OverallCond<=5)),2,1)
  dat[!(dat$SaleCondition=="Partial" | dat$SaleCondition=="Abnorml"),Partial_Quality:=0]
  dat$Partial_Quality <- as.factor(dat$Partial_Quality)
  
  return(dat)
}

diagnostics.model <- function(dat,model,new_dat){
  require(ggplot2)
  
  # predicted vs actuals
  df <- subset(new_dat, select=c("SalePrice","predicted_price"))
  ret = genPlots(df, NULL, "Test data: Predicted vs Actuals")
  ggsave(ret,file="../MODEL/PLOTS/test_actuals_vs_preds.png")
  
  df2 <- subset(dat, select=c("SalePrice","predicted_price"))
  ret = genPlots(df2, NULL, "Train data: Predicted vs Actuals")
  ggsave(ret,file="../MODEL/PLOTS/train_actuals_vs_preds.png")
  
  # residuals vs predicted
  df[,residuals:=SalePrice-predicted_price]
  df2[,residuals:=SalePrice-predicted_price]
  
  p = ggplot(df2,aes(y=residuals, x=predicted_price)) + geom_point(size=1,color='#CC0000') + 
    ggtitle("Train: Predicted vs Residuals")
  ggsave(p, file="../MODEL/PLOTS/train_res_vs_preds.png")
  
  p <- ggplot(df,aes(y=residuals, x=predicted_price)) + geom_point(size=1,color='#CC0000') +
    ggtitle("Test: Predicted vs Residuals")
  ggsave(p, file="../MODEL/PLOTS/test_res_vs_preds.png")
  
  # residuals vs actuals
  p = ggplot(df2,aes(y=residuals, x=SalePrice)) + geom_point(size=1,color='#CC0000') + 
    ggtitle("Train: Actuals vs Residuals")
  ggsave(p, file="../MODEL/PLOTS/train_res_vs_actuals.png")
  
  p <- ggplot(df,aes(y=residuals, x=SalePrice)) + geom_point(size=1,color='#CC0000') +
    ggtitle("Test: Actuals vs Residuals")
  ggsave(p, file="../MODEL/PLOTS/test_res_vs_actuals.png")
}

genPlots <- function(dat, buckets=200, tt){
  if(nrow(dat)>0)
  {dat <- dat[order(predicted_price),]
  if(!is.null(buckets)){
    sq = seq(1, nrow(dat), length.out=buckets+1)
    sq = ceiling(sq)
    sq = unique(c(sq, nrow(dat)))
    
    for (i in 1:(length(sq)-1)){
      dat[sq[[i]]:sq[[i+1]], group:=i]
    }
  }else{
    dat[,group:=1:.N]
  }
  
  f = dat[,j=list(prediction = mean(predicted_price),
                  actuals = mean(SalePrice)
  ), by="group"]
  
  
  ret = ggplot(f, aes(x=prediction, y=actuals)) + geom_point(size=1,
                                                             color='#CC0000') +
    geom_smooth() + geom_abline() + ggtitle(tt)
  } else {text = "No actual data "
  ret=ggplot() + annotate("text", x=4, y=4, label=text) + ggtitle(tt)
  }
  return(ret)
  
}
