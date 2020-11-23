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
nzv<-nearZeroVar(full)

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

#full1[]<-lapply(full1,as.numeric)

for(i in 1:77){
  if(is.factor(full1[,i])){
    full1[,i]<-as.integer(full1[,i])
  }
}
  
train1=full1[1:1460,]
test1=full1[1461:2919,1:76]

train1$SalePrice=as.integer(train1$SalePrice)

#Linear regression

model=lm(SalePrice~.,data=train1)
summary(model)

model2=lm(formula = SalePrice ~ MSSubClass + LotArea + 
            Condition2 + OverallQual + OverallCond + 
            YearBuilt  + RoofMatl +  ExterQual + 
            BsmtQual + BsmtCond + BsmtFinSF1 + BsmtFinSF2 + 
            BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenAbvGr + 
            KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + 
            GarageYrBlt + GarageCars +  SaleCondition, data=train1)
summary(model2)
#prd=predict(model,test1)

#Random forest
rf.model=randomForest(SalePrice~.,data=train1,importance=T)
summary(rf.model)
#plot(rf.model)
rf.prd<-predict(rf.model,test1)

#submit=data.frame(Id=test1$Id,SalePrice=rf.prd)
#write.csv(submit,'submithouseprice.csv',row.names = F)
confusionMatrix(rf.model,test1)

#gbm
library(gbm)

model <- gbm(SalePrice ~., data = train1, distribution = "gaussian",
             shrinkage = 0.05,
             interaction.depth = 12,
             #bag.fraction = 0.6,
             #n.minobsinnode = 1,
             #cv.folds = 1,
             keep.data = F,
             verbose = F,
             n.trees = 200)
predict <- predict(model, test1, n.trees = 200)
submit=data.frame(Id=test1$Id,SalePrice=predict)
write.csv(submit,'submithouseprice.csv',row.names = F)

confusionMatrix(model,test1)

