library(data.table)
train_orig <- copy(train)
set.seed(123)
indx = sample(1:nrow(train),0.3*nrow(train),replace = F)
val <- train[indx,]
train <- train[-indx,]
drop.cols = c("BsmtCond","ExterCond","GarageCond")
train <- subset(train,select = !(colnames(train)%in% drop.cols))
val <- subset(val,select = !(colnames(val)%in% drop.cols))

xgb_grid = expand.grid(nrounds = c(800,1000,1200,1500),
                       eta = c(0.001, 0.005, 0.008, 0.01),
                       max_depth = c(5,6,8),
                       colsample_bytree = c(0.5,0.75,0.9,1),
                       min_child_weight = c(5,10,20,50))

library(xgboost)
y_train <- log(train[["SalePrice"]]+1)
x_train = copy(train)

cols.fac <- names(which(sapply(train,class)=="factor"))
x_train <- x_train[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_train[,SalePrice:=NULL]

y_val <- log(val[["SalePrice"]]+1)
x_val = copy(val)

x_val <- x_val[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_val[,SalePrice:=NULL]


rmse_grid_search <- apply(xgb_grid, 1, function(parameterList){
  
  param <- list(max_depth = parameterList[3], 
                eta = parameterList[2], 
                silent = 1,
                objective="reg:linear",
                eval_metric="rmse",
                # subsample = 0.75,
                min_child_weight = parameterList[5],
                colsample_bytree = parameterList[4],
                base_score =0)

  model_xgb <- xgboost(data=as.matrix(x_train),label = y_train, nrounds = parameterList[1],
                       params = param, verbose = 0)
  preds_xgb_test = predict(model_xgb,newdata = as.matrix(x_val))
  rmse_xgb_test = RMSE(y_val,preds_xgb_test,wt=1)
  
  preds_xgb_train = predict(model_xgb,newdata = as.matrix(x_train))
  rmse_xgb_train = RMSE(y_train,preds_xgb_train,wt=1)
  
  
  return(data.frame(rmse_xgb_train=rmse_xgb_train,
                    rmse_xgb_test=rmse_xgb_test))
  
})

rmse_grid_search = rbindlist(rmse_grid_search)
xgb_grid = cbind(xgb_grid,rmse_grid_search)

write.csv(xgb_grid, file="../MODEL/xgb_grid_search_results.csv", row.names=F)


