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

library(glmnet)
train_orig2 <- copy(train)
train_scored = NULL
bestlam_all = c()
test_rmse_ridge = c()
train_rmse_ridge = c()

for(i in 1:5){
  val <- train_orig2[kfolds==i,]
  train <- train_orig2[kfolds!=i,]
  
  # dim(train); dim(val);
  y_train <- log(train[["SalePrice"]]+1)
  y_train <- train[["SalePrice"]]^0.4
  x_train = copy(train)
  x_train[,":="(SalePrice=NULL, kfolds = NULL)]
  
  y_train = y_train[x_train$GrLivArea<=27]
  x_train = subset(x_train, GrLivArea<=27)
  
  y_val <- log(val[["SalePrice"]]+1)
  y_val <- val[["SalePrice"]]^0.4
  x_val = copy(val)
  x_val[,":="(SalePrice=NULL, kfolds=NULL)]
  
  grid=seq(1,0,-0.001)
  set.seed(1)
  ridge.mod=glmnet(as.matrix(x_train),y_train,alpha=1)
  cv.out=cv.glmnet(as.matrix(x_train),y_train,alpha=1)
  #plot(cv.out)
  bestlam =cv.out$lambda.min #0.09013617
  bestlam_all = c(bestlam_all,bestlam)
  
  ridge.pred_test=predict (ridge.mod ,s=bestlam ,newx=as.matrix(x_val))
  ridge.pred_train=predict (ridge.mod ,s=bestlam ,newx=as.matrix(x_train))
  
  test_rmse_ridge=c(test_rmse_ridge,
                    RMSE(log(y_val^2.5+1),log(ridge.pred_test^2.5+1),wt=1))
  train_rmse_ridge=c(train_rmse_ridge,
                     RMSE(log(y_train^2.5+1),log(ridge.pred_train^2.5+1),wt=1))
  
  #val <- train_orig[indx,]
  val$predicted_price = ridge.pred_test^2.5
  train_scored <- rbind(train_scored,val)
}

train_scored_rmse=RMSE(log(train_scored$predicted_price+1),
                      log(train_scored$SalePrice+1),wt=1)
print(train_scored_rmse)



