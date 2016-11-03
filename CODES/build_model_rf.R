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

library(randomForest)
y_train <- log(train[["SalePrice"]]+1)
y_train <- train[["SalePrice"]]^0.4
x_train = copy(train)
x_train[,":="(SalePrice=NULL, Id =NULL)]

y_val <- log(val[["SalePrice"]]+1)
y_val <- val[["SalePrice"]]^0.4
x_val = copy(val)
x_val[,":="(SalePrice=NULL, Id =NULL)]

model_rf = randomForest(x=x_train,y=y_train,xtest = x_val,ytest = y_val,
                        ntree = 1000, importance = T, 
                        nodesize = 5, keep.forest = T)

preds_rf_test = predict(model_rf,newdata = x_val,type="response")
rmse_rf_test = RMSE(log(y_val^2.5+1),log(preds_rf_test^2.5+1),wt=1)

preds_rf_train = predict(model_rf,newdata = x_train,type="response")
rmse_rf_train = RMSE(log(y_train^2.5+1),log(preds_rf_train^2.5+1),wt=1)

print(rmse_rf_test)
print(rmse_rf_train)

val <- train_orig[indx,]
val$predicted_price = preds_rf_test^2.5
train <- train_orig[-indx]
train$predicted_price = preds_rf_train^2.5