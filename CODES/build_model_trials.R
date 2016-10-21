library(data.table)
train_orig <- copy(train)
set.seed(123)
indx = sample(1:nrow(train),0.3*nrow(train),replace = F)
val <- train[indx,]
train <- train[-indx,]
drop.cols = c("BsmtCond","ExterCond","GarageCond")
train <- subset(train,select = !(colnames(train)%in% drop.cols))
val <- subset(val,select = !(colnames(val)%in% drop.cols))
###############################
fit <- lm(SalePrice~., train)
rm.cols2 <- which(is.na(fit$coefficients))

preds = predict.lm(fit, val)
rmse_lm = RMSE(val[["SalePrice"]],preds,wt=1)
print(rmse_lm)
##############################
library(gbm)
model_gbm = gbm(SalePrice~.,data=train,n.trees = 300,
                interaction.depth = 5, n.minobsinnode = 10,
                train.fraction = 0.7,shrinkage = 0.01)
imp_gbm=summary(model_gbm)
preds_gbm = predict.gbm(model_gbm,newdata = val)
rmse_gbm = RMSE(val[["SalePrice"]],preds_gbm,wt=1)
print(rmse_gbm)
#############################
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

param <- list(max_depth = 6, 
              eta = 0.01, 
              silent = 1,
              objective="reg:linear",
              eval_metric="rmse",
              # subsample = 0.75,
              min_child_weight = 10,
              colsample_bytree = 0.5,
              base_score =0)
model_xgb <- xgboost(data=as.matrix(x_train),label = y_train, nrounds = 1000,
                     params = param, verbose = 0)
imp_xgb = xgb.importance(model = model_xgb, feature_names = colnames(x_train))
write.csv(imp_xgb, file = "../MODEL/imp_xgb.csv", row.names=F)

preds_xgb = predict(model_xgb,newdata = as.matrix(x_val))
rmse_xgb = RMSE(y_val,preds_xgb,wt=1)
print(rmse_xgb)

preds_xgb_train = predict(model_xgb,newdata = as.matrix(x_train))
rmse_xgb_train = RMSE(y_train,preds_xgb_train,wt=1)
print(rmse_xgb_train)

train[,predicted_price:=(exp(preds_xgb_train)-1)]
val[,predicted_price:= (exp(preds_xgb)-1)]

write.csv(train, file="../DERIVED/train.csv", row.names = F)
write.csv(val, file="../DERIVED/validation.csv", row.names = F)

diagnostics.model(dat = train, model = model_xgb, 
                  new_dat = val)

###############################
library(randomForest)
y_train <- log(train[["SalePrice"]])
x_train = copy(train)
x_train[,SalePrice:=NULL]

y_val <- log(val[["SalePrice"]])
x_val = copy(val)
x_val[,SalePrice:=NULL]

model_rf = randomForest(x=x_train,y=y_train,xtest = x_val,ytest = y_val,
                        ntree = 300, importance = T, nodesize = 10, keep.forest = T)

preds_rf = predict(model_rf,newdata = x_val,type="response")
rmse_rf = RMSE(y_val,preds_rf,wt=1)
print(rmse_rf)


panel1 <- panel1[,flag:=(ifelse((End_Date-59)<min(Start_Date),Start_Date,(End_date-59)))<=Date & Date<=End_Date,by=Enrollment]
panel1 <- subset(panel1, flag==T)
