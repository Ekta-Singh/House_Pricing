train = copy(train_orig)
target = train$target
train[,target:=NULL]
train <- predict(dummyObj,train)
train <- data.table(train)
train$SalePrice = target
train <- subset(train, select = c(cols_to_keep,"SalePrice"))

y_train <- log(train[["SalePrice"]]+1)
x_train = copy(train)
x_train[,":="(SalePrice=NULL, Id =NULL)]

grid=seq(1,0,-0.001)
set.seed(1)
ridge.mod=glmnet(as.matrix(x_train),y_train,alpha=1, lambda =grid)
cv.out=cv.glmnet(as.matrix(x_train),y_train,alpha=1)
#plot(cv.out)
bestlam =cv.out$lambda.min

test_orig = copy(test)
fm <- formula(paste("~ ",paste(colnames(test),collapse = "+"), sep = ""))
dummyObj = dummyVars(formula = fm, data = test, sep = NULL)
test <- predict(dummyObj,test)
test <- data.table(test)
test <- subset(test, select = colnames(train))

x_test = copy(test)
x_test[,Id:= NULL]

ridge.pred_test=predict (ridge.mod ,s=bestlam ,newx=as.matrix(x_test))

test$SalePrice =exp(ridge.pred_test[,1]) - 1

write.csv(test[,c("Id","SalePrice"),with=F], file = "../MODEL/Submission_V1.csv")
