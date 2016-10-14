library(data.table)
train_orig <- copy(train)
indx = sample(1:nrow(train),0.2*nrow(train),replace = F)
val <- train[indx,]
train <- train[-indx,]

###############################
fit <- lm(SalePrice~., train)
rm.cols2 <- which(is.na(fit$coefficients))

preds = predict.lm(fit, val)
rmse_lm = RMSE(val[["SalePrice"]],preds,wt=1)

##############################
library(gbm)
model_gbm = gbm(SalePrice~.,data=train,n.trees = 100,
                interaction.depth = 5, n.minobsinnode = 10,
                train.fraction = 0.7,shrinkage = 0.01)
imp_gbm=summary(model_gbm)
preds_gbm = predict.gbm(model_gbm,newdata = val)
rmse_gbm = RMSE(val[["SalePrice"]],preds_gbm,wt=1)

#############################
library(xgboost)
y_train <- train[["SalePrice"]]
cols.fac <- names(which(sapply(train,class)=="factor"))
x_train <- train[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_train[,SalePrice:=NULL]

y_val <- val[["SalePrice"]]
x_val <- val[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_val[,SalePrice:=NULL]

param <- list(max_depth = 5, 
              eta = 0.02, 
              silent = 1,
              objective="reg:linear",
              eval_metric="rmse",
              # subsample = 0.75,
              min_child_weight = 10,
              colsample_bytree = 0.75,
              base_score =0)
model_xgb <- xgboost(data=as.matrix(x_train),label = y_train, nrounds = 100,
                     params = param, verbose = 0)
imp_xgb = xgb.importance(model = model_xgb, feature_names = colnames(x_train))

preds_xgb = predict(model_xgb,newdata = as.matrix(x_val))
rmse_xgb = RMSE(y_val,preds_xgb,wt=1)










