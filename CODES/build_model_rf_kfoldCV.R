library(data.table)
library(caret)

train_orig <- copy(train)
test_orig <- copy(test)
target = train$SalePrice
train[,SalePrice:=NULL]
fm <- formula(paste("~ ",paste(colnames(train),collapse = "+"), sep = ""))
dummyObj = dummyVars(formula = fm, data = train, sep = NULL)
train <- predict(dummyObj,train)
train <- data.table(train)
train$SalePrice = target

model=lm(SalePrice~.,subset(train,select = c(-Id)))
cols_to_keep <- gsub("`","",rownames(summary(model)$coeff))
cols_to_keep <- cols_to_keep[-1]
train <- subset(train, select = c(cols_to_keep,"SalePrice"))

set.seed(123)
indx = sample(1:nrow(train),nrow(train),replace = F)
train <- train[indx,]
train$kfolds <- rep(x = 1:5, length.out = nrow(train))

library(randomForest)
train_scored = NULL
train_orig2 <- copy(train)
rmse_rf_test = c()
rmse_rf_train = c()

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
  
  model_rf = randomForest(x=x_train,y=y_train,xtest = x_val,ytest = y_val,
                          ntree = 1000, importance = T, 
                          nodesize = 10, keep.forest = T)
  
  preds_rf_test=predict(model_rf,newdata = x_val,type="response")
  preds_rf_train=predict(model_rf,newdata = x_train,type="response")
  
  rmse_rf_test=c(rmse_rf_test, RMSE(log(y_val^2.5+1),log(preds_rf_test^2.5+1),wt=1))
  rmse_rf_train=c(rmse_rf_train, RMSE(log(y_train^2.5+1),log(preds_rf_train^2.5+1),wt=1))
  
  #val <- train_orig[indx,]
  val$predicted_price = preds_rf_test^2.5
  train_scored <- rbind(train_scored,val)
}

train_scored_rmse=RMSE(log(train_scored$predicted_price+1),
                       log(train_scored$SalePrice+1),wt=1)
print(train_scored_rmse)