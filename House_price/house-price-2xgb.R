#House prices
library(ggplot2)
library(randomForest)
library(caret)

#Data import
setwd("/home/sudhir/git/ML-R/House_price")
train<-read.csv('train.csv',na.strings = c("","-"," ","NA","na","Na","_"))
test<-read.csv('test.csv',na.strings = c("","-"," ","NA","na","Na","_"))
test$SalePrice<-'none'
table(test$SalePrice)
full<-rbind(train,test)
str(full)

numeric_var<-names(full)[which(sapply(full,is.numeric))]
cat_var<-names(full)[which(sapply(full,is.factor))] 

#finding missing values 
mis<-table(is.na(full))
mis
mis[2]*100/(nrow(full)*ncol(full))
colSums(is.na(full))
colSums(is.na(full[,numeric_var]))

#Replacing missing value with mean
table(is.na(full$LotFrontage))
full[sapply(full,is.numeric)]<-lapply(full[sapply(full,is.numeric)],function(x) ifelse(is.na(x),mean(x,na.rm=T),x))

# full$LotFrontage[is.na(full$LotFrontage)]=mean(full$LotFrontage,na.rm =TRUE)
# 
# table(is.na(full$MasVnrArea))
# full$MasVnrArea[is.na(full$MasVnrArea)]=mean(full$MasVnrArea,na.rm =TRUE)
# 
# table(is.na(full$GarageYrBlt))
# full$GarageYrBlt[is.na(full$GarageYrBlt)]=mean(full$GarageYrBlt,na.rm=TRUE)
# 
# table(is.na(full$BsmtFinSF1))
# full$BsmtFinSF1[is.na(full$BsmtFinSF1)]=mean(full$BsmtFinSF1,na.rm=TRUE)
# 
# table(is.na(full$BsmtFinSF2))
# full$BsmtFinSF2[is.na(full$BsmtFinSF2)]=mean(full$BsmtFinSF2,na.rm=TRUE)
# 
# table(is.na(full$TotalBsmtSF))
# full$TotalBsmtSF[is.na(full$TotalBsmtSF)]=mean(full$TotalBsmtSF,na.rm=TRUE)
# 
# table(is.na(full$BsmtFullBath))
# full$BsmtFullBath[is.na(full$BsmtFullBath)]=mean(full$BsmtFullBath,na.rm=TRUE)
# 
# table(is.na(full$BsmtHalfBath))
# full$BsmtHalfBath[is.na(full$BsmtHalfBath)]=mean(full$BsmtHalfBath,na.rm=TRUE)
# 
# table(is.na(full$BsmtUnfSF))
# full$BsmtUnfSF[is.na(full$BsmtUnfSF)]=mean(full$BsmtUnfSF,na.rm=TRUE)
# 
# table(is.na(full$GarageCars))
# full$GarageCars[is.na(full$GarageCars)]=mean(full$GarageCars,na.rm=TRUE)
# 
# table(is.na(full$GarageArea))
# full$GarageArea[is.na(full$GarageArea)]=mean(full$GarageArea,na.rm=TRUE)


#Finding missing value in catagorical variable
colSums(is.na(full[cat_var]))

summary(full$MSZoning)
full$MSZoning[is.na(full$MSZoning)]='RL'

summary(full$MasVnrType)
full$MasVnrType[is.na(full$MasVnrType)]='None'

summary(full$Utilities)
full$Utilities[is.na(full$Utilities)]='AllPub'

summary(full$Exterior1st)
full$Exterior1st[is.na(full$Exterior1st)]='VinylSd'

summary(full$Exterior2nd)
full$Exterior2nd[is.na(full$Exterior2nd)]='VinylSd'

summary(full$BsmtQual)
full$BsmtQual[is.na(full$BsmtQual)]='Gd'

summary(full$BsmtCond)
full$BsmtCond[is.na(full$BsmtCond)]='TA'

summary(full$BsmtExposure)
full$BsmtExposure[is.na(full$BsmtExposure)]='No'

summary(full$BsmtFinType1)
full$BsmtFinType1[is.na(full$BsmtFinType1)]='Unf'

summary(full$BsmtFinType2)
full$BsmtFinType2[is.na(full$BsmtFinType2)]='Unf'

summary(full$Electrical)
full$Electrical[is.na(full$Electrical)]='SBrkr'


summary(full$GarageType)
full$GarageType[is.na(full$GarageType)]='Attchd'

summary(full$GarageFinish)
full$GarageFinish[is.na(full$GarageFinish)]='Unf'

summary(full$GarageCond)
full$GarageCond[is.na(full$GarageCond)]='TA'

summary(full$GarageQual)
full$GarageQual[is.na(full$GarageQual)]='TA'

summary(full$KitchenQual)
full$KitchenQual[is.na(full$KitchenQual)]='TA'

summary(full$Functional)
full$Functional[is.na(full$Functional)]='Typ'

summary(full$SaleType)
full$SaleType[is.na(full$SaleType)]='WD'

summary(full$FireplaceQu)
table(as.factor(full$Fireplaces), useNA = "ifany") 
#Fireplaces is 
levels(full$FireplaceQu)<-c(levels(full$FireplaceQu),'none')
full$FireplaceQu[is.na(full$FireplaceQu)]<-rep('none')


#Missing value present in this varaible in more than 90%, so we can ignore this variable
summary(full$Alley)
summary(full$PoolQC)
summary(full$Fence)
summary(full$MiscFeature)

c('Alley','PoolQC','Fence','MiscFeature') #ignore

colSums(is.na(full))

full1=full[,-c(7,73,74,75)]

full1[]<-lapply(full1,as.numeric)

# for(i in 1:77){
#   if(is.factor(full1[,i])){
#     full1[,i]<-as.numeric(full1[,i])
#   }
# }
# for(i in 1:77){
#   if(is.integer(full1[,i])){
#     full1[,i]<-as.numeric(full1[,i])
#   }
# }

train1=full1[1:1460,]
test1=full1[1461:2919,1:76]

train1$SalePrice=as.numeric(train1$SalePrice)

#xgboost
library(xgboost)

xgtrain<-xgb.DMatrix(as.matrix(train1),label=train1[,"SalePrice"])
xgtest<-xgb.DMatrix(as.matrix(test1))

#default parameters

params <- list(booster = "gblinear", seeds=0,objective = "reg:linear", eta=0.001,alpha=1, gamma=2, 
               max_depth=12,base_score=7.76, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = xgtrain, nrounds = 200, nfold = 10, showsd = T, 
                 stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
plot(log(xgbcv$evaluation_log))
##best iteration = 33

xgbm<-xgboost(data=xgtrain,params = params,nrounds = 200,verbose = F)
summary(xgbm)
xgprd<-predict(xgbm,xgtest)

xgb.model.dt.tree(model=xgbm)
submit=data.frame(Id=test$Id,SalePrice=xgprd)
write.csv(submit,'xsubmithouseprice.csv',row.names = F)


##xgboost parameters
xgb_params = list(
  seed = 0,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.02, 
  objective = 'reg:linear',
  max_depth = 12,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)

xg_eval_mae <- function (yhat, xgtrain) {
  y = getinfo(xgtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}


#
library(data.table)

gb_dt=xgb.train(xgb_params,xgtrain,nrounds = 150)

xgb_prd<-predict(gb_dt,xgtest)


#default parameters

params <- list(booster = "gbtree", objective = "reg:linear", eta=0.3, gamma=0, 
               max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = xgtrain, nrounds = 100, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, early_stop_round = 20, maximize = F)
##best iteration = 33
xgb<-xgboost(data=xgtrain,label = train1[,"SalePrice"], 
             eta = 1,
             max_depth = 15, 
             nround=100, 
             subsample = 0.5,
             colsample_bytree = 0.5,
             seed = 1,
             eval_metric = "auc",
             objective = "reg:linear",
             num_class = 12,
             nthread = 3)

gb1 <- xgb.train (params = params, data = xgtrain, nrounds = 33, watchlist = list(val=xgtest,train=xgtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
#model prediction
xgbpred <- predict (xgb1,xgtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)


#Ind_var<-as.matrix(train1[,1:76],rownames.force = NA)
#Dep_var<-as.matrix(train[,77],rownames.force=NA)

#Ind_var<-as(Ind_var,'sparseMatrix')
#Dep_var<-as(Dep_var,'sparseMatrix')

