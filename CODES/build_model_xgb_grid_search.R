library(data.table)
train_orig <- copy(train)
set.seed(123)
indx = sample(1:nrow(train),0.2*nrow(train),replace = F)
val <- train[indx,]
train <- train[-indx,]
# train <- subset(train,select = c(feat,"SalePrice"))
# val <- subset(val,select = c(feat,"SalePrice"))

xgb_grid = expand.grid(nrounds = c(100,300,500,600,800),
                       eta = c(0.005, 0.01, 0.02, 0.03, 0.05),
                       max_depth = c(5,8,10))

library(xgboost)
y_train <- train[["SalePrice"]]
x_train = copy(train)

cols.fac <- names(which(sapply(train,class)=="factor"))
x_train <- x_train[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_train[,SalePrice:=NULL]

y_val <- val[["SalePrice"]]
x_val = copy(val)

x_val <- x_val[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_val[,SalePrice:=NULL]


model_xgb <- xgboost(data=as.matrix(x_train),label = y_train, nrounds = 300,
                     params = param, verbose = 0)
imp_xgb = xgb.importance(model = model_xgb, feature_names = colnames(x_train))

preds_xgb = predict(model_xgb,newdata = as.matrix(x_val))
rmse_xgb = RMSE(y_val,preds_xgb,wt=1)
print(rmse_xgb)

rmse_grid_search <- apply(xgb_grid, 1, function(parameterList){
  
  param <- list(max_depth = parameterList[3], 
                eta = parameterList[2], 
                silent = 1,
                objective="reg:linear",
                eval_metric="rmse",
                # subsample = 0.75,
                min_child_weight = 10,
                colsample_bytree = 0.75,
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


