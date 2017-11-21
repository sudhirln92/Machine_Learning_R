# Project Predict Comprensive strength of 
library('moments') #skewness
library('pastecs') #Discriptive statistics
library('fmsb') #VIF
library('ggplot2')
library('diptest') # dip test for unimodality

# data mining
setwd("/home/sudhir/Data science/Supervised machine learning/OLS/project 1")
data=read.csv('train.csv')
# Checking missing value
table(is.na(data))
str(data)
head(data)

#Uni variate analysis
new=data
str(new)
table(new$Days)

boxplot( new$Cement, ylab='Cement(Kg)', main='Cement Boxpolt',col = 'red' )
hist(new$Cement, xlab='Cement(Kg)', main= 'Cement histogram', col='green')
plot(density(new$Cement), xlab='Cement(Kg)', main= 'Cement Density plot', col='green')
skewness(new$Cement)
dip.test(new$Cement)

boxplot( new$Slag, ylab='Slag(Kg)', main='Slag Boxpolt',col = 'red' )
hist(new$Slag, xlab='Slag(Kg)', main= 'Slag histogram', col='green')
plot(density(new$Slag), xlab='Slag(Kg)', main= 'Slag Density plot', col='green')
skewness(new$Slag)
dip.test(new$Slag)

boxplot( new$Ash, ylab='Ash(Kg)', main='Ash Boxpolt',col = 'red' )
hist(new$Ash, xlab='Ash(Kg)', main= 'Ash histogram', col='green')
plot(density(new$Ash), xlab='Ash(Kg)', main= 'Ash Density plot', col='green')
skewness(new$Ash)
dip.test(new$Ash)

boxplot( new$Water, ylab='Water(Kg)', main='Water Boxpolt',col = 'red' )
hist(new$Water, xlab='Water(Kg)', main= 'Water histogram', col='green')
plot(density(new$Water), xlab='Water(Kg)', main= 'Water Density plot', col='green')
skewness(new$Water)
dip.test(new$Water)

boxplot( new$Superplasticizer, ylab='Superplasticizer(Kg)', main='Superplasticizer Boxpolt',col = 'red' )
hist(new$Superplasticizer, xlab='Superplasticizer(Kg)', main= 'Superplasticizer histogram', col='green')
plot(density(new$Superplasticizer), xlab='Superplasticizer(Kg)', main= 'Superplasticizer Density plot', col='green')
skewness(new$Superplasticizer)
dip.test(new$Superplasticizer)

boxplot( new$CoarseAggregate, ylab='CoarseAggregate(Kg)', main='CoarseAggregate Boxpolt',col = 'red' )
hist(new$CoarseAggregate, xlab='CoarseAggregate(Kg)', main= 'CoarseAggregate histogram', col='green')
plot(density(new$CoarseAggregate), xlab='CoarseAggregate(Kg)', main= 'CoarseAggregate Density plot', col='green')
skewness(new$CoarseAggregate)
dip.test(new$FineAggregate)

boxplot( new$FineAggregate, ylab='FineAggregate(Kg)', main='FineAggregate Boxpolt',col = 'red' )
hist(new$FineAggregate, xlab='FineAggregate(Kg)', main= 'FineAggregate histogram', col='green')
plot(density(new$FineAggregate), xlab='FineAggregate(Kg)', main= 'FineAggregate Density plot', col='green')
skewness(new$FineAggregate)
dip.test(new$FineAggregate)

boxplot( new$CompressiveStrength, ylab='CompressiveStrength(Kg)', main='CompressiveStrength Boxpolt',col = 'red' )
hist(new$CompressiveStrength, xlab='CompressiveStrength(Kg)', main= 'CompressiveStrength histogram', col='green')
plot(density(new$CompressiveStrength), xlab='CompressiveStrength(Kg)', main= 'CompressiveStrength Density plot', col='green')
skewness(new$CompressiveStrength)
dip.test(new$CompressiveStrength)

# Transformed output applying BOX COX tranfermatio
library('caret')
c<- caret:: BoxCoxTrans(new$CompressiveStrength)
print(c)
new$CompressiveStrength1=new$CompressiveStrength^0.5

#Bivaraite analysis

cor(new[,-8])
plot(new$Cement,new$CompressiveStrength1,col=c('red','green'),pch=16, xlab = 'Cement in (kG)',ylab = 'CompressiveStrength1 (Mpa)', main = 'Cement vs CompressiveStrength1')
plot(new$Slag,new$CompressiveStrength1,col=c('red','green'),pch=16, xlab = 'Slag in (kG)',ylab = 'CompressiveStrength (Mpa)', main = 'Slag vs CompressiveStrength')
plot(new$Ash,new$CompressiveStrength1,col=c('red','green'),pch=16, xlab = 'Ash in (kG)',ylab = 'CompressiveStrength (Mpa)', main = 'Ash vs CompressiveStrength')
plot(new$Water,new$CompressiveStrength1,col=c('red','green'),pch=16, xlab = 'Water in (kG)',ylab = 'CompressiveStrength (Mpa)', main = 'Water vs CompressiveStrength')
plot(new$Superplasticizer,new$CompressiveStrength1,col=c('red','green'),pch=16, xlab = 'Superplasticizer in (kG)',ylab = 'CompressiveStrength (Mpa)', main = 'Superplasticizer vs CompressiveStrength')
plot(new$CoarseAggregate,new$CompressiveStrength1,col=c('red','green'),pch=16, xlab = 'CoarseAggregate in (kG)',ylab = 'CompressiveStrength (Mpa)', main = 'CoarseAggregate vs CompressiveStrength')
plot(new$FineAggregate,new$CompressiveStrength1, col=c('red','green'),pch=16, xlab='FineAggregate', ylab = 'CompressiveStrength',main='FineAggregate vs CompressiveStrength')

ag=aggregate(new$CompressiveStrength1, by=list(new$Days),mean)
ag
pairs(new[,-8],col=c('blue','yellow'))

# Building on train
new=new[,-9]
model=lm(CompressiveStrength1~., data=new1)
summary(model)
VIF(model)
new1=new[,-c(5,6,7)]
model1=lm(CompressiveStrength1~., data=new1)
summary(model1)
VIF(model1)

# Model validation
test=read.csv('test.csv')
#test=test[,-8]
str(test)
test$CompressiveStrength1=test$CompressiveStrength^0.5
test=test[,-9]
pred=predict(model1,test)
sse=sum((pred-test$CompressiveStrength1)^2)
sst=sum((test$CompressiveStrength1-mean(test$CompressiveStrength1))^2)
e=(sse/sst)
rsquare=1-e
rsquare

#test
plot(pred,test$CompressiveStrength1, main='test:Predicted vs Actual value', xlab = 'Predicted value',ylab = 'Actual value', col=c('blue','yellow'))
#train
plot(fitted.values(model1),new1$CompressiveStrength1, main='train: Predicted vs Actual value', xlab = 'Predicted value',ylab = 'Actual value', col=c('blue','yellow'))

#Model assumption
## Residual normality
plot(density(resid(model1)), main= 'Residual model', col='red')
skewness(resid(model1))
qqnorm(resid(model1))
qqline(resid(model1),col='red')
# Residual homoscedasticity
plot(fitted.values(model1),resid(model1),main='Fitted vs Residual value',xlab='fitted value',ylab='residual value', col=c('red','green'))

#Breuch pagan test for homoscedacity
lmtest::bptest(model1)

#Residual liniarity

new1$resid=resid(model1)
plot(new$Cement,resid(model1),xlab = 'Cement (kg)',ylab = 'Residual', main = 'Cement Residual linearity',col=c('red','purple'))
plot(new$Slag,resid(model1),xlab = 'Slag (kg)',ylab = 'Residual', main = 'Slag Residual linearity',col=c('red','purple'))
plot(new$Ash,resid(model1),xlab = 'Ash (kg)',ylab = 'Residual', main = 'Ash Residual linearity',col=c('red','purple'))
plot(new$Water,resid(model1),xlab = 'Water (kg)',ylab = 'Residual', main = 'Water Residual linearity',col=c('red','purple'))
plot(new$Superplasticizer,resid(model1),xlab = 'Superplasticizer (kg)',ylab = 'Residual', main = 'Superplasticizer Residual linearity',col=c('red','purple'))
plot(new$CoarseAggregate,resid(model1),xlab = 'CoarseAggregate (kg)',ylab = 'Residual', main = 'CoarseAggregate Residual linearity',col=c('red','purple'))
plot(new$FineAggregate,resid(model1),xlab = 'FineAggregate (kg)',ylab = 'Residual', main = 'FineAggregate Residual linearity',col=c('red','purple'))
plot(new$FineAggregate,resid(model1),xlab = 'FineAggregate (kg)',ylab = 'Residual', main = 'FineAggregate Residual linearity',col=c('red','purple'))


