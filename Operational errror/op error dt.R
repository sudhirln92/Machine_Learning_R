# Operational error by Decision tree
library(xlsx) # Excel
library(fmsb) #Districriptive stat
library(moments) #Skewness
library(caret) #confusion matrix
library(ROCR) #ROC
library(e1071)# Confusion matrix
library(pscl) #R2

library(rpart) # Decision Tree
library(rpart.plot) # ploting
library(rattle)
library(RColorBrewer)

#Importing data
setwd('/home/sudhir/Data science/Supervised machine learning/Logistic regression/project 2')
opdata=read.xlsx('Operational error.xlsx', sheetIndex = 1)
str(opdata)
stat.desc(opdata)

#Data Preparation
opdataMain=opdata
opdata$Qualification=as.factor(opdata$Qualification)
opdata$Hierarchy =as.factor(opdata$Hierarchy)
opdata$Role =as.factor(opdata$Role )

opdata$Errors1=ifelse(opdata$Errors==2,1,opdata$Errors)
opdata$Errors1=ifelse(opdata$Errors==3,1,opdata$Errors1)
opdata$Errors1=ifelse(opdata$Errors==4,1,opdata$Errors1)

table(opdata$Errors1)
table(opdata$Gender)
table(opdata$Months)
#Finding missing value
table(is.na(opdata))
table(opdata$Location)
table(opdata$Staff)

#Univarte analysis
plot(density(opdata$Experience.in.months), main= 'Experience (month)',xlab='Experience',col='red')
skewness(opdata$Experience.in.months)
boxplot(opdata$Experience.in.months,main= 'Boxplot of Expience (month)',xlab='Experience', col='red')
hist(opdata$Experience.in.months,main= 'Boxplot of Expience (month)', xlab='Experience', col='darkred')

plot(density(opdata$Age.in.years),main='Age (year)',col='blue', xlab='Age')
hist(opdata$Age.in.years,main='Histogram Age (year)',col='blue', xlab='Age')
boxplot(opdata$Age.in.years,main='Boxplot Age (year)',col='blue', xlab='Age')
skewness(opdata$Age.in.years)

plot(density(opdata$Production.effort.in.minutes), main='Production Effort (minute)', xlab='Production Effort',col='red')
hist(opdata$Production.effort.in.minutes, main='Histogram of Production Effort (minute)', xlab='Production Effort',col='red')
boxplot(opdata$Production.effort.in.minutes, main='Boxplot of Production Effort (minute)', xlab='Production Effort',col='red')
skewness(opdata$Production.effort.in.minutes)

plot(density(opdata$Number.of.working.days), main='Number of working days', xlab='Number of wroking days', col='blue')
hist(opdata$Number.of.working.days, main='Histogram of Number of working days', xlab='Number of wroking days', col='blue')
boxplot(opdata$Number.of.working.days, main='Boxplot of Number of working days', xlab='Number of wroking days', col='blue')
skewness(opdata$Number.of.working.days)

hist(opdata$Overtime,main= 'Histogram of Overtime',xlab='Over time',col='red')
plot(density(opdata$Overtime),main= 'Overtime',xlab='Over time',col='red')
boxplot(opdata$Overtime,main= 'Overtime',xlab='Over time',col='red')
skewness(opdata$Overtime)

#decision tree
opdata1=opdata[,-c(1,2,3,12)]
dh=sample(nrow(opdata1),nrow(opdata1)*0.7)
train=opdata1[dh,]
test=opdata1[-dh,]
fit = rpart(Errors1 ~.,method="class", data=train)

printcp(fit) #Display the result
plotcp(fit)
summary(fit)

#Plot tree
plot(fit,  main='classification of errors')
text(fit)

fancyRpartPlot(fit)

#Pruning
ptree=prune(fit,cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree)
plot(ptree)

#Accuracy
pred=predict(ptree,test,type = "class")
accuracy_test=mean(pred==test$Survived)

#/*classification table/confusion matrix*/

confusionMatrix(pred,test$Survived)


