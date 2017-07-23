#titanic project
library(randomForest)
library(dplyr) #data manipulation
library(ggplot2) # Data visualization

#Data import
setwd("/home/sudhir/git/ML-R/Titanic")
train<-read.csv('train.csv',na.strings = c("","na","NA","Na"," "),stringsAsFactors = F)
test<-read.csv('test.csv',na.strings = c("","na","NA","Na"," "),stringsAsFactors = F)

#Data manipulation
str(train)
str(test)

full<-bind_rows(train,test)
full$Survived=factor(full$Survived)
train$Survived=as.factor(train$Survived)
full$Sex=as.factor(full$Sex)
full$Pclass=as.factor(full$Pclass)
full$SibSp=as.factor(full$SibSp)
full$Parch=as.factor(full$Parch)
full$Embarked=as.factor(full$Embarked)
str(full)

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
#Tickect number, Name and cabin is not required
full1=full[,-c(4,9,11)]

#Outlier treatment
fulltemp<-full1
for(i in 1:ncol(fulltemp)){ 
  qnt <- quantile(fulltemp[,i], probs=c(.25, .75), na.rm = T)
  caps <- quantile(fulltemp[,i], probs=c(.01, .99), na.rm = T)
  H <- 1.5 * IQR(full[,i], na.rm = T)
  fulltemp[,i][fulltemp[,i] < (qnt[1] - H)] <- caps[1]
  fulltemp[,i][fulltemp[,i] > (qnt[2] + H)] <- caps[2]
}

train1=fulltemp[1:891,]
test1=fulltemp[892:1309,]
colSums(is.na(train1))

#Model building
model.rf=randomForest(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+
                        Fare+Embarked+Title+family,
                      data=train1,ntree=500,importance=TRUE)
model.rf$importance
plot(model.rf)
model.rf$confusion
#Model prediction
prd=predict(model.rf,test1,type= 'response')
test1$Survived=prd
table(test1$Survived)

submit<-data.frame(PassengerId=test1$PassengerId, Survived=prd )
write.csv(submit,file = 'tt-submit.csv',row.names = F)


#gbm
library(gbm)
library(xgboost)


#gbm
for(i in 2:11){
  if(is.factor(full1[,i])){
    full1[,i]<-as.integer(full1[,i])
  }
}
train1=full1[1:891,]
test1=full1[892:1309,]
model <- gbm(factor(Survived)~., data = train1, distribution = "gaussian",
             shrinkage = 0.01,
             interaction.depth = 5,
             bag.fraction = 0.6,
             #n.minobsinnode = 1,
             #cv.folds = 1,
             #keep.data = F,
             verbose = F,
             n.trees = 100)

summary(model)
predict <- predict(model, test1,type='response',n.tree=100)
fit=round(predict)
fit=ifelse(fit<0.48,0,1)
fit=as.factor(fit)
#prg_g<-predict.gbm(model, test2, n.trees = 300,type='link')


#submit the solution
test1$Survived=fit
ggplot(test2,aes(x=Age,fill=Survived))+
  geom_histogram()+xlab('Age')
submit<-data.frame(PassengerId=test1$PassengerId, Survived=factor(fit) )
write.csv(submit,file = 'tt-submit.csv',row.names = F)

#Data preparation for gbm
for(i in 2:11){
  if(is.factor(full1[,i])){
    full1[,i]<-as.integer(full1[,i])
  }
}


test2=full1[892:1309,]
train2=train1[1:891,]
xgtrain<-xgb.DMatrix(as.matrix(train2))
xgtest<-xgb.DMatrix(as.matrix(test2))
