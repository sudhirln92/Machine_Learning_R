#ITC Infotech project
library(dplyr) #Data manipulation
library(caret) #Confusion matrix
library(randomForest) #Random Forest
library(ROCR)
library(moments) #Skewness

#Data Import
setwd('/home/sudhir/R/ITCinfotech')
train=read.csv('train_data.csv',sep=",", na.strings = c("","NA","-"," "))
test=read.csv('test_data.csv',sep=",",na.strings = c("","NA","-"," "))

#Data manipulation
head(train)
str(train)
table(is.na(train))
summary(is.na(train))

table(is.na(test))

train%>%
  count(Loan_Status)
#64% people pay loan

train%>%
  count(Gender)
train$Gender = as.character(train$Gender)
train$Gender[is.na(train$Gender)]='M'
train$Gender=as.factor(train$Gender)
 
ggplot(train,aes(x=Gender,y=Loan_Status,fill=Loan_Status))+
  geom_bar(stat='identity')+
  labs(x="Gender",y="Loan status",title="Details")

train%>%
  count(Married)
  
ggplot(train,aes(x=Married,y=Loan_Status,fill=Loan_Status))+
  geom_bar(stat="identity")+
  labs(x="Married",y="Loan Status",title="Details")

train%>%
  count(Dependents)%>%
  group_by(Dependents)%>%
  mutate(Loan_Status=ordered(Loan_Status,c('Y','N')))

ggplot(train,aes(x=Dependents,y=Loan_Status,fill=Loan_Status))+
  geom_bar(stat="identity")+
  labs(x="Dependents",y="Loan Status",title="Details")

train%>%
  count(Education)

ggplot(train,aes(x=Education,y=Loan_Status,fill=Loan_Status))+
  geom_bar(stat="identity")+
  labs(x="Education",y="Loan Status",title="Details")

train%>%
  count(Self_Employed)
train$Self_Employed=as.character(train$Self_Employed)
train$Self_Employed[is.na(train$Self_Employed)]="NO"
train$Self_Employed=as.factor(train$Self_Employed)


train%>%
  group_by(ApplicantIncome)

ggplot(train,aes(x=ApplicantIncome,colour='red'))+
  geom_density()

train%>%
  group_by(CoapplicantIncome)
  
ggplot(train,aes(x=CoapplicantIncome,colors='blue'))+
  geom_density()
skewness(train$CoapplicantIncome) #skewness in the range +-3

train%>%
  group_by(LoanAmount)
train$LoanAmount[is.na(train$LoanAmount)]=mean(train$LoanAmount,na.rm = T)
ggplot(train,aes(x=LoanAmount,colours='green'))+
  geom_density()

train%>%
  count(Loan_Amount_Term)%>%
  group_by(Loan_Amount_Term)
#83% people loan term is 360, so replace "NA" with it by 360
train$Loan_Amount_Term[is.na(train$Loan_Amount_Term)]=360
#train$Loan_Amount_Term=as.factor(train$Loan_Amount_Term)

ggplot(train,aes(x=Loan_Amount_Term,y=Loan_Status,fill=Loan_Status))+
  geom_bar(stat='identity')

train%>%
  count(Credit_History)
train$Credit_History=as.factor(train$Credit_History)
train$Credit_History[is.na(train$Credit_History)]=1
ggplot(train,aes(x=Credit_History,y=Loan_Status,fill=Loan_Status))+
  geom_bar(stat='identity')

train%>%
  count(Property_Area)%>%
  group_by(Property_Area)

ggplot(train,aes(x=Property_Area,y=Loan_Status,fill=Loan_Status))+
  geom_bar(stat='identity')

test=na.roughfix(test)

#Data model
itcmodel.rf=randomForest(as.factor(Loan_Status)~.,importance=TRUE,data=train,ntree=500,xtest=test)
plot(itcmodel.rf)
itcmodel.rf$importance #Importance

#Predicting
submit=data.frame(Application_Id=test[,1],Loan_Status=itcmodel.rf$test$predicted)

itcmodel.rf$confusion #Confusion matrix

test$Loan_Status=submit
write.csv(submit,file="submit.itc.csv")
