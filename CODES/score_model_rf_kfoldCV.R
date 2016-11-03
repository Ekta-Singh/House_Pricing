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
test$SalePrice = 0

model=lm(SalePrice~.,subset(train,select = c(-Id)))
cols_to_keep <- gsub("`","",rownames(summary(model)$coeff))
cols_to_keep <- cols_to_keep[-1]
train <- subset(train, select = c(cols_to_keep,"SalePrice"))

set.seed(123)
indx = sample(1:nrow(train),nrow(train),replace = F)
train <- train[indx,]
train$kfolds <- rep(x = 1:5, length.out = nrow(train))

library(randomForest)
train_orig2 <- copy(train)
for(i in 1:5){
  
  train <- train_orig2[kfolds!=i,]
  
  # dim(train); dim(val);
  y_train <- log(train[["SalePrice"]]+1)
  y_train <- train[["SalePrice"]]^0.4
  x_train = copy(train)
  x_train[,":="(SalePrice=NULL, kfolds = NULL)]
  
  model_rf = randomForest(x=x_train,y=y_train,xtest = x_val,ytest = y_val,
                          ntree = 1000, importance = T, 
                          nodesize = 2, keep.forest = T)
  
  x_test = copy(test)
  x_test <- subset(x_test, select = colnames(x_train))
  #x_test[,Id:= NULL]
  
  preds_rf_test=predict(model_rf,newdata = x_test,type="response")
  
  test$SalePrice =(test$SalePrice + (preds_rf_test^2.5))
  
}

test$SalePrice = test$SalePrice/5

write.csv(test[,c("Id","SalePrice"),with=F], 
          file = "../MODEL/Submission_V11.csv", row.names = F)


