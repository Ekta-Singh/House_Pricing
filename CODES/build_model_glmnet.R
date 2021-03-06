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
indx = sample(1:nrow(train),0.3*nrow(train),replace = F)
val <- train[indx,]
train <- train[-indx,]
# train = subset(train,train$GrLivArea<=4000)
# drop.cols = c("BsmtCond","ExterCond","GarageCond")
# train <- subset(train,select = !(colnames(train)%in% drop.cols))
# val <- subset(val,select = !(colnames(val)%in% drop.cols))

dim(train); dim(val);

library(glmnet)
y_train <- log(train[["SalePrice"]]+1)
y_train <- train[["SalePrice"]]^0.4
x_train = copy(train)
x_train[,":="(SalePrice=NULL, Id =NULL)]

y_val <- log(val[["SalePrice"]]+1)
y_val <- val[["SalePrice"]]^0.4
x_val = copy(val)
x_val[,":="(SalePrice=NULL, Id =NULL)]

grid=seq(1,0,-0.001)
set.seed(1)
ridge.mod=glmnet(as.matrix(x_train),y_train,alpha=1)
cv.out=cv.glmnet(as.matrix(x_train),y_train,alpha=1)
#plot(cv.out)
bestlam =cv.out$lambda.min #0.09013617
print(bestlam)

ridge.pred_test=predict (ridge.mod ,s=bestlam ,newx=as.matrix(x_val))
ridge.pred_train=predict (ridge.mod ,s=bestlam ,newx=as.matrix(x_train))

test_rmse_ridge=RMSE(log(y_val^2.5+1),log(ridge.pred_test^2.5+1),wt=1)
train_rmse_ridge=RMSE(log(y_train^2.5+1),log(ridge.pred_train^2.5+1),wt=1)

print(test_rmse_ridge)
print(train_rmse_ridge)

val <- train_orig[indx,]
val$predicted_price = ridge.pred_test^2.5
train <- train_orig[-indx]
train$predicted_price = ridge.pred_train^2.5
