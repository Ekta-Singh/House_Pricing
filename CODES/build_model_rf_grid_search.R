library(data.table)
train_orig <- copy(train)
set.seed(123)
indx = sample(1:nrow(train),0.3*nrow(train),replace = F)
val <- train[indx,]
train <- train[-indx,]
# train <- subset(train,select = c(feat,"SalePrice"))
# val <- subset(val,select = c(feat,"SalePrice"))

rf_grid = expand.grid(nrounds = c(500,600,800,1000,1200,1500),
                       nodesize = c(5,10,15, 25, 50))

library(randomForest)
y_train <- log(train[["SalePrice"]])
x_train = copy(train)
x_train[,SalePrice:=NULL]

y_val <- log(val[["SalePrice"]])
x_val = copy(val)
x_val[,SalePrice:=NULL]


rmse_grid_search <- apply(rf_grid, 1, function(parameterList){
  
  model_rf = randomForest(x=x_train,y=y_train,xtest = x_val,ytest = y_val,
                          ntree = parameterList[1], importance = T, 
                          nodesize = parameterList[2], keep.forest = T)
  
  preds_rf_test = predict(model_rf,newdata = x_val,type="response")
  rmse_rf_test = RMSE(y_val,preds_rf_test,wt=1)
  
  preds_rf_train = predict(model_rf,newdata = x_train,type="response")
  rmse_rf_train = RMSE(y_train,preds_rf_train,wt=1)
 
  return(data.frame(rmse_rf_train=rmse_rf_train,
                    rmse_rf_test=rmse_rf_test))
  
})

rmse_grid_search = rbindlist(rmse_grid_search)
rf_grid = cbind(rf_grid,rmse_grid_search)

write.csv(rf_grid, file="../MODEL/rf_grid_search_results.csv", row.names=F)


