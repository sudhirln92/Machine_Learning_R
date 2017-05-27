#titanic project
library(randomForest)
library(dplyr) #data manipulation
library(ggplot2) # Data visualization

#Data import
setwd("/home/sudhir/git/ML-R/Titanic")
train<-read.csv('train.csv')
test<-read.csv('test.csv')

#Data manipulation
str(train)

full<-bind_rows(train,test)
full$Survived=as.factor(full$Survived)
train$Survived=as.factor(train$Survived)
full$Pclass=as.factor(full$Pclass)
full$SibSp=as.factor(full$SibSp)
full$Parch=as.factor(full$Parch)

 #finding missing value
colSums(is.na(full))
full$Age[is.na(full$Age)]=mean(full$Age,na.rm = T)

full$Fare[is.na(full$Fare)]=mean(full$Fare,na.rm=T)

table(full$Embarked)
#thier is 2 missing vale in Embarked, replace it with S
full$Embarked[is.na(full$Embarked)]='S'
#ignor this variable Cabin

ggplot(full[1:891,],aes(x=Pclass,fill=Survived))+
  geom_bar(stat='count')+
  xlab('Passenger class')+ylab('Survival count')

#What is in the name
head(full$Name)
#grab title from passenger name
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex,full$Title)

#Title with low count combined to rare title
rare_title<-c('Dona','Don','Dr','Jonkheer','Lady','Rev','Sir','the Countess')
officer<-c('Capt','Col','Major')
full$Title[full$Title%in%c('Mlle','Ms')]<-'Miss'
full$Title[full$Title=='Mme']<-'Mrs'
full$Title[full$Title%in% rare_title ]<-'Rare Title'
full$Title[full$Title%in%officer]<-'Officer'
table(full$Sex,full$Title)
#Who all survived
ggplot(full[1:891,],aes(x=Title,y=Survived,fill=Survived))+
  geom_bar(stat='identity')+
  xlab('Title')+ylab('Survival count')

#the size of each passenger family count
full$family=as.factor(as.numeric(full$SibSp) + as.numeric(full$Parch) +1)

#Which family is survived
ggplot(full[1:891,],aes(x=family,y=Survived,fill=Survived))+
  geom_bar(stat='identity')+
  xlab('Family count')

str(full)
full$Title=as.factor(full$Title)

#Split data into test & train

train1=full[1:891,]
test1=full[892:1309,]
colSums(is.na(train1))

#Model building
model.rf=randomForest(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+
                        Fare+Embarked+Title+family,
                      data=train1,ntree=500,importance=TRUE)
model.rf$importance
plot(model.rf)
model.rf$confusion
#Model prediction
prd=predict(model.rf,test1)
test1$Survived=prd
table(test1$Survived)
#submit the solution
submit<-data.frame(PassengerId=test1$PassengerId, Survived=prd )
ggplot(test1,aes(x=Age,fill=Survived))+
  geom_histogram()+xlab('Age')
write.csv(submit,file = 'tt-submit.csv',row.names = F)
