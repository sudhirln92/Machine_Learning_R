#Logistic Regression
library(pastecs)#descriptive stats
library(moments)#skewness
library("fmsb")#VIF
library(pscl)#R 2 of model
library(caret)# confusion matrix
library(e1071) # confusion matrix
library(ROCR)# ROC curve


#/*reading the train file*/
setwd('/home/sudhir/git/ML-R/Logistic regression/Titanic')
titanic=read.csv("titanic_logistic rgression.csv",header=TRUE,na.string=c(""))
str(titanic)

#/* Dropping insignificant variables  in the dataset*/

data=names(titanic)%in%c("PassengerId","Cabin","Ticket","Name")
titanic2=titanic[!data]
titanic2
str(titanic2)

#/*structure of dataset*/
str(titanic2)
stat.desc(titanic2)
titanic2$Pclass=as.factor(titanic2$Pclass)
titanic2$Survived=as.factor(titanic2$Survived)
str(titanic2)

#/*Checking missing values in dataset*/

table(is.na(titanic2))
summary(is.na(titanic2))
colSums(is.na(titanic2))

#Note: cabin has 687 missing values,Age has 177 and Embarked has 2 missing values.


#/*working with missing values*/

colSums(is.na(titanic2))

#Note: Age has 177 missing values,replacing values by mean
titanic2$Age[is.na(titanic2$Age)]=mean(titanic2$Age,na.rm=TRUE)
colSums(is.na(titanic2))
titanic4=na.omit(titanic2)
colSums(is.na(titanic4))
str(titanic4)

#/*outlier detetion*/

skewness(titanic4$Age)
hist(titanic4$Age,col.lab="blue",col="green")
boxplot(titanic4$Age)

skewness(titanic4$Fare)
hist(titanic4$Fare,col.lab="blue",col="red",lty=4)
boxplot(titanic4$Fare)

titanic5=titanic4


#/*corelation with o/p variable*/

table(titanic5$Survived,titanic5$Sex)
table(titanic5$Survived,titanic5$Pclass)
table(titanic5$Survived,titanic5$Embarked)

new=aggregate(titanic5$Age,by=list(titanic5$Survived),mean)
new1=aggregate(titanic5$Fare,by=list(titanic5$Survived),mean)


#/*creating dummy variable */
titanic5$sexM=as.factor(ifelse(titanic5$Sex=="male",1,0))
titanic5$sexF=as.factor(ifelse(titanic5$Sex=="female",1,0))
titanic5$Pclass1=as.factor(ifelse(titanic5$Pclass== 1,1,0))
titanic5$Pclass2=as.factor(ifelse(titanic5$Pclass== 2,1,0))
titanic5$Pclass3=as.factor(ifelse(titanic5$Pclass== 3,1,0))
titanic5$EmbarkedQ=as.factor(ifelse(titanic5$Embarked=="Q",1,0))
titanic5$EmbarkedS=as.factor(ifelse(titanic5$Embarked=="S",1,0))
titanic5$EmbarkedC=as.factor(ifelse(titanic5$Embarked=="C",1,0))

str(titanic5)


#/*train and test dataset*/

str(titanic5)
table(titanic5$Survived)/nrow(titanic5)
dt = sort(sample(nrow(titanic5), nrow(titanic5)*.7))
train=titanic5[dt,]
test=titanic5[-dt,]
str(train)
str(test)
table(test$Survived)/nrow(test)
table(train$Survived)/nrow(train)
str(train)

train=train[,-c(2,3,8)]
#/*logistic regression*/
model=glm(Survived~.,family=binomial(link='logit'),data=train)
summary(model)

#/*removing insignificant variables */

model1=glm(Survived~sexM+Age+Pclass1+Pclass2+SibSp,family=binomial(link='logit'),data=train)
summary(model1)



#/*Predicting ability Accuracy on test dataset */

newfitted=ifelse(fitted.values(model)>0.5,1,0)
accuracy=mean(newfitted==train$Survived)


newdata=test
fittedresults=predict(model1,newdata,type='response')
fittedresults1=ifelse(fittedresults>0.5,1,0)

accuracy_test=mean(fittedresults1==test$Survived)

#/*R value*/
pR2(model1)

#/*Validation against  test  dataset*/
#/*classification table/confusion matrix*/

confusionMatrix(fittedresults1,test$Survived)

#/*ROC curve*/

pr = prediction(fittedresults, test$Survived)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc = performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc

#Prediction
#a=cbind(as.numeric(newfitted), as.numeric(fittedresults1),deparse.level = 0)
#submit=data.frame(newfitted)
#submit=data.frame(test$Survived)
#names(submit)=c("PassengerId","Survived")
#output=write.csv(submit,file = survival.csv)



