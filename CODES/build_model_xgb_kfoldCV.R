library(data.table)
library(caret)

train_orig <- copy(train)
test_orig <- copy(test)
target = train$SalePrice

set.seed(123)
indx = sample(1:nrow(train),nrow(train),replace = F)
train <- train[indx,]
train$kfolds <- rep(x = 1:5, length.out = nrow(train))

library(xgboost)
train_orig2 <- copy(train)
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- sqrt(mean(((log(labels^2.5+1) - log(preds^2.5+1)))^2, na.rm=T))
  return(list(metric = "error", value = err))
}

train_scored = NULL
rmse_xgb_test = c()
rmse_xgb_train = c()

for(i in 1:5){
  val <- train_orig2[kfolds==i,]
  train <- train_orig2[kfolds!=i,]
  
  # dim(train); dim(val);
  y_train <- log(train[["SalePrice"]]+1)
  y_train <- train[["SalePrice"]]^0.4
  x_train = copy(train)
  x_train[,":="(SalePrice=NULL, kfolds = NULL)]
  
  y_val <- log(val[["SalePrice"]]+1)
  y_val <- val[["SalePrice"]]^0.4
  x_val = copy(val)
  x_val[,":="(SalePrice=NULL, kfolds=NULL)]
  
  cols.fac <- names(which(sapply(train,class)=="factor"))
  x_train <- x_train[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
  x_val <- x_val[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
  
  param <- list(max_depth = 5, 
                eta = 0.01, 
                silent = 1,
                objective="reg:linear",
                eval_metric=evalerror,
                subsample = 0.75,
                min_child_weight = 2,
                colsample_bytree = 0.5,
                base_score =0)
  
  train.xg <- xgb.DMatrix(as.matrix((x_train)), label=y_train, missing=111222333)
  test.xg <- xgb.DMatrix(as.matrix((x_val)), label=y_val, missing=111222333)
  watchlist <- list(test = test.xg,train = train.xg)
  
  model_xgb <- xgb.train(data=train.xg, nrounds = 2000,
                         params = param, verbose = 1, missing = 111222333, 
                         early.stop.round = 500, 
                         watchlist = watchlist, 
                         maximize = F, print.every.n = 100)
  
  preds_xgb_test=predict(model_xgb,newdata = as.matrix(x_val))
  preds_xgb_train=predict(model_xgb,newdata = as.matrix(x_train))
  
  rmse_xgb_test=c(rmse_xgb_test, RMSE(log(y_val^2.5+1),log(preds_xgb_test^2.5+1),wt=1))
  rmse_xgb_train=c(rmse_xgb_train, RMSE(log(y_train^2.5+1),log(preds_xgb_train^2.5+1),wt=1))
  
  #val <- train_orig[indx,]
  val$predicted_price = preds_xgb_test^2.5
  train_scored <- rbind(train_scored,val)
}

train_scored_rmse=RMSE(log(train_scored$predicted_price+1),
                       log(train_scored$SalePrice+1),wt=1)
print(train_scored_rmse)