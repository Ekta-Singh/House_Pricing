diagnostics.model(dat = train, model = ridge.mod, new_dat = val)

vars <- c("GrLivArea","X1stFlrSF","BsmtQual","ExterQual","OverallQual","OverallCond",
          "LotArea","AgeofHouse","Time_Since_Remodel")

library(ggplot2)
flag = F
for(v in vars){
  train[,residuals:=SalePrice-predicted_price]
  if(is.numeric(train[,get(v)]) & length(unique(train[,get(v)]))>10){
    quan = quantile(train[,get(v)],probs = seq(0,1,by=.1))
    train[,(paste0(v,"_bin")):= cut(train[,get(v)],breaks = unique(quan),labels = F,include.lowest = T)]
    v=paste0(v,"_bin")
    flag=T
  }
  p = ggplot(train,aes(y=residuals, x=predicted_price, color= as.factor(get(v)))) + 
    geom_point(size=1) + ggtitle(paste0("Train: Predicted vs Residuals by ",v))
  ggsave(p, file=paste0("../MODEL/PLOTS/Residual_grouped_byPredictors/train_res_vs_preds_by",v,".png"))
  if(flag==T){
    train[,(v):=NULL]
    flag = F
  }
}