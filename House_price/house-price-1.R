#House prices
library(ggplot2)
library(dplyr)
library(randomForest)

#Data import
setwd("/home/sudhir/git/ML-R/House_price")
train<-read.csv('train.csv',na.strings = c("","-"," ","NA","na","Na","_"))
test<-read.csv('test.csv',na.strings = c("","-"," ","NA","na","Na","_"))
test$SalePrice<-'none'
table(test$SalePrice)
full<-rbind(train,test)
str(full)


#finding missing values 
mis<-table(is.na(full))
mis
mis[2]*100/(nrow(full)*ncol(full))
colSums(is.na(full))
numeric_var<-names(full)[which(sapply(full,is.numeric))]
colSums(is.na(full[,numeric_var]))

#Replacing missing value with mean
table(is.na(full$LotFrontage))
full$LotFrontage[is.na(full$LotFrontage)]=mean(full$LotFrontage,na.rm =TRUE)

table(is.na(full$MasVnrArea))
full$MasVnrArea[is.na(full$MasVnrArea)]=mean(full$MasVnrArea,na.rm =TRUE)

table(is.na(full$GarageYrBlt))
full$GarageYrBlt[is.na(full$GarageYrBlt)]=mean(full$GarageYrBlt,na.rm=TRUE)

#Finding missing value in catagorical variable
cat_var<-names(full)[which(sapply(full,is.factor))] 
colSums(is.na(full[cat_var]))

#Missing value present in this varaible in more than 90%, so we can ignore this variable
summary(full$Alley)
summary(full$PoolQC)
summary(full$Fence)
summary(full$MiscFeature)

c('Alley','PoolQC','Fence','MiscFeature') #ignore
summary(full$MasVnrType)
full$MasVnrType[is.na(full$MasVnrType)]='None'

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

summary(full$FireplaceQu)

summary(full$GarageType)
full$GarageType[is.na(full$GarageType)]='Attchd'

summary(full$GarageFinish)
full$GarageFinish[is.na(full$GarageFinish)]='Unf'

summary(full$GarageCond)
full$GarageCond[is.na(full$GarageCond)]='TA'

summary(full$GarageQual)
full$GarageQual[is.na(full$GarageQual)]='TA'

train1<-full[,-c(7,73,74,75)]

str(train1)
train1$MSZoning<-as.integer(train1$MSZoning) 
train1=as.data.frame(sapply(train1,as.integer,F)) 

table(is.na(full))
#Apply principle component analysis
pri.com<-prcomp(train1,scale=T,na.rm=T)

corr=cor(train1)
plot(corr)


model=lm(SalePrice~.,data=train1,x=test)
summary(model)
