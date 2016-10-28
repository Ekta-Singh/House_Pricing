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

model=lm(SalePrice~.,train)
cols_to_keep <- gsub("`","",rownames(summary(model)$coeff))
cols_to_keep <- cols_to_keep[-1]
train <- subset(train, select = c(cols_to_keep,"SalePrice"))

set.seed(123)
indx = sample(1:nrow(train),0.3*nrow(train),replace = F)
val <- train[indx,]
train <- train[-indx,]
# drop.cols = c("BsmtCond","ExterCond","GarageCond")
# train <- subset(train,select = !(colnames(train)%in% drop.cols))
# val <- subset(val,select = !(colnames(val)%in% drop.cols))

dim(train); dim(val);

library(glmnet)
y_train <- log(train[["SalePrice"]]+1)
x_train = copy(train)
x_train[,":="(SalePrice=NULL, Id =NULL)]

y_val <- log(val[["SalePrice"]]+1)
x_val = copy(val)
x_val[,":="(SalePrice=NULL, Id =NULL)]

grid=seq(1,0,-0.001)
set.seed(1)
ridge.mod=glmnet(as.matrix(x_train),y_train,alpha=1, lambda =grid)
cv.out=cv.glmnet(as.matrix(x_train),y_train,alpha=1)
#plot(cv.out)
bestlam =cv.out$lambda.min #0.09013617

ridge.pred_test=predict (ridge.mod ,s=bestlam ,newx=as.matrix(x_val))
ridge.pred_train=predict (ridge.mod ,s=bestlam ,newx=as.matrix(x_train))

test_rmse_ridge=RMSE(y_val,ridge.pred_test,wt=1)
train_rmse_ridge=RMSE(y_train,ridge.pred_train,wt=1)

print(test_rmse_ridge)
print(train_rmse_ridge)



