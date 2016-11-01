train = copy(train_orig)
target = train$SalePrice
train[,SalePrice:=NULL]

dat_all <- rbind(train,test)
fm <- formula(paste("~ ",paste(colnames(dat_all),collapse = "+"), sep = ""))
dummyObj = dummyVars(formula = fm, data = dat_all, sep = NULL)
dat_all <- predict(dummyObj,dat_all)
dat_all <- data.table(dat_all)
train <- dat_all[1:nrow(train_orig),]
test <- dat_all[(nrow(train_orig)+1):nrow(dat_all),]

train <- data.table(train)
test <- data.table(test)
train$SalePrice = target

model=lm(SalePrice~.,subset(train,select = c(-Id)))
cols_to_keep <- gsub("`","",rownames(summary(model)$coeff))
cols_to_keep <- cols_to_keep[-1]
train <- subset(train, select = c(cols_to_keep,"SalePrice"))

y_train <- log(train[["SalePrice"]]+1)
y_train <- train[["SalePrice"]]^0.4
x_train = copy(train)
x_train[,":="(SalePrice=NULL, Id =NULL)]

grid=seq(1,0,-0.001)
set.seed(1)
ridge.mod=glmnet(as.matrix(x_train),y_train,alpha=1)
cv.out=cv.glmnet(as.matrix(x_train),y_train,alpha=1)
#plot(cv.out)
bestlam =cv.out$lambda.min
print(bestlam)

x_test = copy(test)
x_test <- subset(x_test, select = colnames(x_train))
x_test[,Id:= NULL]

ridge.pred_test=predict (ridge.mod ,s=bestlam ,newx=as.matrix(x_test))

test$SalePrice =(ridge.pred_test[,1])^2.5

write.csv(test[,c("Id","SalePrice"),with=F], file = "../MODEL/Submission_V8.csv", row.names = F)


