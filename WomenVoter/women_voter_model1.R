# Case sutdy of women voter in the Pearsonvill
library(randomForest) #Random forest
library(rpart)# decision tree
library(caret)# confusion matrix
library(e1071) # confusion matrix
library(ROCR)# ROC curve
library(rpart.plot)#  tree plotting
library(ggplot2) #Grammer of graph
library(dplyr) #data Manipulation

#Data import
setwd("/home/sudhir/R/Case-study1")
train=read.csv('traindata_R.csv',header = TRUE,sep=",")
test=read.csv('testdata_R.csv',header=TRUE,sep=",")

# Data manipulation 
full=bind_rows(train,test)
table(is.na(full)) #Finding missing values
full%>%
  head()
str(full)

full%>%group_by(Wife_education)%>%
  summarise(avg_age=mean(Wife_age),
            children=mean(Number_of_children_ever_born),
            Working_women=percent_rank(Wife_working),
            Working_women_SLI4=percent_rank(Wife_working))


table(full$Wife_working) # Percentage of working peolple
working= (369*100)/count(full)
working #  %  #

full$Party_voted_for=as.factor(full$Party_voted_for)

#Data sampling
sp=sample(nrow(full),nrow(full)*.7)
train2=full[sp,]
test2=full[-sp,]


#Data modeling

  #Decision tree
model.dc=rpart(Party_voted_for~., method = 'class', data=train2)
model.dc$cptable
plot(model.dc)
text(model.dc)
rpart.plot(model.dc)

#Pruning of tree
prtr=prune(model.dc,cp=model.dc$cptable[which.min(model.dc$cptable[,"xerror"]),'CP'])

plot(prtr)
text(prtr)
rpart.plot(prtr)

pre.dct=predict(prtr,test2,type = 'class')
acc.dc=mean(pre.dct==test2$Party_voted_for)
acc.dc

confusionMatrix(pre.dct,test2$Party_voted_for)

#Random forest
model.rf=randomForest(as.factor(Party_voted_for)~.,data=train2,ntree =500,importance = T)
summary(model.rf)
plot(model.rf)

prd.rf=predict(model.rf,test2,method="prob")
acc.rf=mean(prd.rf==test2$Party_voted_for)

#Confusion matrix
confusionMatrix(prd.rf, test2$Party_voted_for)

roc.curve(model.rf,test2)
prf=performance(prd.rf,"tpr","fpr")


# logistic regression
#Creating dummy varaible
main=full

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
acc.lr=mean(fitresult1==testlr$Party_voted_for)
acc.lr

#Model assumption
pr = prediction(fitresult1, testlr$Party_voted_for)
prf.lr = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf.lr,col='red')

a=confusionMatrix(fitresult1,testlr$Party_voted_for)

auc = performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc

#Best model for prediction
plot(c(acc.lr, acc.dc,acc.rf),main="Accuracy")
 #Random forest will give more accuracy compared to other model

