# Case sutdy of women voter in the Pearsonvill
library(randomForest) #Random forest

library(pastecs)#descriptive stats
library(moments)#skewness
library(fmsb)#VIF
library(pscl)#R 2 of model
library(rpart)# decision tree
library(caret)# confusion matrix
library(e1071) # confusion matrix
library(ROCR)# ROC curve

library(rpart.plot)#  tree plotting
library(RColorBrewer)# tree plotting
library(rattle)# tree plotting
library(ROSE) #roc.curve

# Data import
setwd("/home/sudhir/R/Case-study1")
train=read.csv('traindata_R.csv',header = TRUE,sep=",")
test=read.csv('testdata_R.csv',header=TRUE,sep=",")


# Data manipulation 
table(is.na(train)) #Finding missing values
table(is.na(test))

train$Wife_education=as.factor(train$Wife_education)
train$Husband_education=as.factor(train$Husband_education)
train$Wife_religion=as.factor(train$Wife_religion)
train$Wife_working=as.factor(train$Wife_working)
train$Husband_occupation=as.factor(train$Husband_occupation)
train$Standard_of_living_index=as.factor(train$Standard_of_living_index)
train$Media_exposure=as.factor(train$Media_exposure)
train$Party_voted_for=as.factor(train$Party_voted_for)
str(train)

test$Wife_education=as.factor(test$Wife_education)
test$Husband_education=as.factor(test$Husband_education)
test$Wife_religion=as.factor(test$Wife_religion)
test$Wife_working=as.factor(test$Wife_working)
test$Husband_occupation=as.factor(test$Husband_occupation)
test$Standard_of_living_index=as.factor(test$Standard_of_living_index)
test$Media_exposure=as.factor(test$Media_exposure)
test$Party_voted_for=as.factor(test$Party_voted_for)
str(test)

table(col(train)) # Number of peolple
mean(train$Wife_age) #Average age
mean(train$Number_of_children_ever_born) #Average number of children
table(train$Wife_working) # Percentage of working peolple
working= (729*100)/sum(table(train$Wife_working))
working

table(train$Standard_of_living_index)
St_4=(445*100)/982
St_4

# Univariate anaylsis

plot(density(train$Wife_age),xlab = 'Wife age',main="Density plot of women's age", col='red')
hist(train$Wife_age,xlab = 'Wife age',main="Histogram of women's age", col='blue')
boxplot(train$Wife_age, xlab='Wife age',main="Boxplot of women's age", col='purple')
skewness(train$Wife_age)

table(train$Wife_education)
table(train$Husband_education)
table(train$Number_of_children_ever_born)
table(train$Wife_religion)
table(train$Wife_working)
table(train$Husband_occupation)
table(train$Standard_of_living_index)
table(train$Media_exposure)
table(train$Party_voted_for)

#Model Building
 # logistic regression
#Creating dummy varaible
main=rbind(train,test)

main$Wife_education1=ifelse(main$Wife_education==1,1,0)
main$Wife_education2=ifelse(main$Wife_education==2,1,0)
main$Wife_education3=ifelse(main$Wife_education==3,1,0)
main$Wife_education4=ifelse(main$Wife_education==4,1,0)
main$Husband_education1=ifelse(main$Husband_education==1,1,0)
main$Husband_education2=ifelse(main$Husband_education==2,1,0)
main$Husband_education3=ifelse(main$Husband_education==3,1,0)
main$Husband_education4=ifelse(main$Husband_education==4,1,0)
main$Wife_religion0=ifelse(main$Wife_religion==0,1,0)
main$Wife_religion1=ifelse(main$Wife_religion==1,1,0)
main$Wife_working0=ifelse(main$Wife_working==0,1,0)
main$Wife_working1=ifelse(main$Wife_working==1,1,0)
main$Husband_occupation1=ifelse(main$Husband_occupation==1,1,0)
main$Husband_occupation2=ifelse(main$Husband_occupation==2,1,0)
main$Husband_occupation3=ifelse(main$Husband_occupation==3,1,0)
main$Husband_occupation4=ifelse(main$Husband_occupation==4,1,0)
main$Standard_of_living_index1=ifelse(main$Standard_of_living_index==1,1,0)
main$Standard_of_living_index2=ifelse(main$Standard_of_living_index==2,1,0)
main$Standard_of_living_index3=ifelse(main$Standard_of_living_index==3,1,0)
main$Standard_of_living_index4=ifelse(main$Standard_of_living_index==4,1,0)
main$Media_exposure0=ifelse(main$Media_exposure==0,1,0)
main$Media_exposure1=ifelse(main$Media_exposure==1,1,0)

#Model train and test
sp=sample(nrow(main),nrow(main)*0.7)
trainlr=main[sp,]
testlr=main[-sp,]

#Model building
model.glm1=glm(Party_voted_for~., family = binomial(link = "logit"), data=trainlr)
summary(model.glm1) # p value >0.05 should be ignored
trainlr1=trainlr[,c(1,4,10,13,14,30,32)]
model.glm2=glm(Party_voted_for~., family = binomial(link = "logit"), data=trainlr1)
summary(model.glm2) 

#Predicting accuracy on test dataset
newfitted=ifelse(fitted.values(model.glm2)>0.5,1,0)
accuracy=mean(newfitted==trainlr$Party_voted_for)
accuracy

fitresult=predict(model.glm2,newdata=testlr, type='response')
fitresult1=ifelse(fitresult>0.5,1,0)
accuracy_test=mean(fitresult1==testlr$Party_voted_for)

#Model assumption
pr = prediction(fitresult1, testlr$Party_voted_for)
prf.lr = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf.lr,col='red')

auc = performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc

# Decision tree
model.dc1=rpart(Party_voted_for~., method = 'class', data=train)
model.dc1$cptable
plot(model.dc1)
text(model.dc1)

fancyRpartPlot(model.dc1)

#Pruning of tree
prtr=prune(model.dc1,cp=model.dc1$cptable[which.min(model.dc1$cptable[,"xerror"]),'CP'])

plot(prtr)
text(prtr)

fancyRpartPlot(prtr)
pre.dct=predict(prtr,test,type = 'prob')
accuracy_test.dc=mean(pre.dct==test$Party_voted_for)
accuracy_test.dc

roc=prediction(model.dc1,test)
roc = performance(pre.dct, measure="tpr", x.measure="fpr")
confusionMatrix(pre.dct,test$Party_voted_for)

#Random Forest
model.rf=randomForest(as.factor(Party_voted_for)~.,data=train,ntree=150,importance=T)
summary(model.rf)
plot(model.rf)

#Prediction
prrf=predict(model.rf,test)
acc.rf=mean(prrf==test$Party_voted_for)

#Confusion matrix
confusionMatrix(prrf, test$Party_voted_for)

roc.curve(model.rf,test)
prf=performance(prrf,"tpr","fpr")

