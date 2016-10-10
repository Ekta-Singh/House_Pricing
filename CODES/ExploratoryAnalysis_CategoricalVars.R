library(data.table)
library(ggplot2)

###############################
setwd("C:\\Users\\system5\\Documents\\StudyWork\\Kaggle\\House_Pricing\\CODES/")
train <- read.csv("../RAW/train.csv")
train$MSSubClass <- as.factor(as.character(train$MSSubClass))
train <- data.table(train)

head(train)
sapply(train,class)
summary(train)

############################## ANOVA
col.fac <- names(which(sapply(train,class)=="factor"))
df <- data.frame()
for(c in col.fac){
  if(any(is.na(train[,get(c)]))){
    train[is.na(get(c)),(c):="NA"]
  }
  f <- formula(paste0("SalePrice ~ ",c))
  fit <- lm(formula = f, data = train)
  df <- rbind(df,data.frame(Variable = c, Anove_PValue = anova(fit)$Pr[1]))
  
}
write.csv(df,file="../DERIVED/ANALYSES/anova_FactorVars2.csv",row.names = F)
f <- formula(paste0("SalePrice ~ PoolQC"))
fit <- lm(formula = f, data = train)
anova(fit)

############################## Summary
train_fac <- subset(train, select=col.fac)
s <- data.frame(summary(train_fac))
