library(pastecs)#descriptive stats
library(moments)#skewness
library("fmsb")#VIF
library(randomForest)
library(randomForestSRC)
library(caret)# confusion matrix
library(e1071) # confusion matrix
library(ROCR)# ROC curve

setwd("/home/sudhir/Data science/Supervised machine learning/Decision tree and Random forest")
#/*reading the train file*/
titanic=read.csv("titanic_logistic rgression.csv",header=TRUE,na.string=c(""))
titanic

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


#Skewness treatment/log transformation of Fare

titanic5=titanic4

#/*corelation with o/p variable*/

plot(titanic5$Survived,titanic5$Age)
plot(titanic5$Survived,titanic5$Fare)



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

#/*Random forest*/

fit1=randomForest(as.factor(Survived)~.,train,ntree=500,importance=T)	
summary(fit1)
plot(fit1)
#/*predicting accuracy test*/

prediction1 = predict(fit1,newdata=test)
misclassifierror=mean(prediction1!=test$Survived)
print(paste('Accuracy',1-misclassifierror))
table(prediction1,test$Survived)

#ROC Curve
roc.curve(card$Class, prediction1[,2])

#/*classification table/confusion matrix*/

confusionMatrix(prediction1,test$Survived)


pd=predict(fit1,newdata = titanic5)
submit <- data.frame(PassengerId=titanic$PassengerId, Survived=pd)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(titanic5[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()






