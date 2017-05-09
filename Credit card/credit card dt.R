# Credit card fraud detection
library(pastecs)#descriptive stats
library(moments)#skewness
library(fmsb)#VIF
library(caret)# confusion matrix
library(e1071) # confusion matrix
library(ROCR)# ROC curve

library(rpart) # Decision tree
library(ROSE) # Imbalanced data

# Data import
setwd("/home/sudhir/R")
#card=read.csv('creditcard.csv')
card=readRDS('card.RDS')
head(card)
str(card)
card$Class=as.factor(card$Class)
#card$Time=as.numeric(card$Time)
#saveRDS(card,'card.RDS')

#Finding missing value
summary(is.na(card))
table(is.na(card))

# Data exploration
table(card$Class)

# Univariate analysis
plot(density(card$V1),xlab = 'V1',main="Credit card fraud",col='red')
hist(card$V1,xlab = 'V1',main="Histogram of Credit card fraud",col='blue')
boxplot(card$V1,xlab = 'V1',main="Boxplot of Credit card fraud",col='yellow')
skewness(card$V1)

plot(density(card$V2),xlab = 'V2',main="Credit card fraud",col='blue')
hist(card$V2,xlab = 'V2',main="Histogram of Credit card fraud",col='green')
boxplot(card$V2,xlab = 'V2',main="Boxplot of Credit card fraud",col='yellow')
skewness(card$V2)

plot(density(card$V3),xlab = 'V3',main="Credit card fraud",col='green')
hist(card$V3,xlab = 'V3',main="Histogram of Credit card fraud",col='red')

#Bivariate analysis


# Model building
fit1=rpart(Class~.,method = 'class',data=card)
summary(fit1)
plot(fit1)
text(fit1)

# Model validation
pred=predict(fit1,newdata=card)
#confusionMatrix(pred,card$Class)

# Handling imblanced data
 # Over sampling
data_over=ovun.sample(Class~.,data=card,method = 'over')$data
table(data_over$Class)

 # under sampling
data_under=ovun.sample(Class~., data=card, method = 'under',N=984)$data
table(data_under$Class)
 
 # data balanced both
data_both=ovun.sample(Class~.,data=card,method = 'both', p=0.5, N=1200,seed = 1 )$data
table(data_both$Class)

 # data ROSE
data_rose=ROSE(Class~., data=card, seed=1)$data
table(data_rose$Class)

# build decision tree model
tree.rose=rpart(Class~., data= data_rose)
tree.over=rpart(Class~., data= data_over)
tree.under=rpart(Class~., data=data_under)
tree.both=rpart(Class~., data=data_both)

# make prediction on unseen data
pred.tree.rose=predict(tree.rose, newdata=card)
pred.tree.over=predict(tree.over, newdata=card)
pred.tree.under=predict(tree.under, newdata=card)
pred.tree.both=predict(tree.both, newdata=card)

# finding AUC: area under curve
roc.curve(card$Class, pred.tree.rose[,2])
roc.curve(card$Class,pred.tree.over[,2])
roc.curve(card$Class, pred.tree.both[,2])
roc.curve(card$Class, pred.tree.under[,2])

