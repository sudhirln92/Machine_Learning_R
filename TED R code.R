# Project Combined cycle power plant
library("moments")  #skewness
library("pastecs") #descriptive statistics stats.desc
library("diptest")# Hartigans' dip test for unimodality 
library("fmsb")#VIF
library("DAAG")#K-fold validation

#read the file
setwd("/home/sudhir/git/ML-R")
data=read.csv("TED_OLS.csv",header=TRUE,na.string=c(" "))
str(data)
colnames(data)=c("Temperature","Exhaustvacuum","Pressure","Relativehumidity","Energy")
str(data) 

#missing value
summary(is.na(data))#column wise summary
table(is.na(data))#Total summary

#outlier/uni variate analysis
summary(data)# M.o.CT
stat.desc(data)# M.o.CT +M.o.V

boxplot(data$Temperature,main="Temperature Box Plot",col="red")
hist(data$Temperature,col="blue")
plot(density(data$Temperature),col="blue")
skewness(data$Temperature)
dip.test(data$Temperature)

hist(data$Exhaustvacuum,col="red")
boxplot(data$Exhaustvacuum,main="Exhaust Vacuum",col="red")
plot(density(data$Exhaustvacuum),col='brown')
skewness(data$Exhaustvacuum)
dip.test(data$Exhaustvacuum)

boxplot(data$Pressure,main="Pressure",col="green")
hist(data$Pressure,col="yellow")
plot(density(data$Pressure),col='brown')
skewness(data$Pressure)
dip.test(data$Pressure)

boxplot(data$Relativehumidity,main="Relative humidity",col="green")
hist(data$Relativehumidity,col="green")
plot(density(data$Relativehumidity),col='brown')
skewness(data$Relativehumidity)
dip.test(data$Relativehumidity)

boxplot(data$Energy,main="Energy",col="green")
hist(data$Energy,col="brown")
plot(density(data$Energy),col='brown')
skewness(data$Energy)
dip.test(data$Energy)

#box cox transformation
c <- caret::BoxCoxTrans(data$Energy)
print(c)
data$newen=data$Energy^(-2)

#Co-relation between IV and DV
cor(data)
plot(data$Temperature,data$Energy,col=c("green","red"),main="Energy Vs. Temperature",font=10)
plot(data$Exhaustvacuum,data$Energy,col=c("yellow","red"),main="Energy Vs. Exhaustvacuum",font=10)
plot(data$Pressure,data$Energy,col=c("red","blue"),main="Energy Vs. Pressure",font=10)
plot(data$Relativehumidity,data$Energy,col=c("black","red"),main="Energy Vs. Relativehumidity",font=10)
pairs(data,col=c('red','blue'))

#cross validation
a=sample(nrow(data),nrow(data)*0.7)
train=data[a,]
test=data[-a,]

#Model run/VIF check
model1=lm(Energy~.,data=train[,-6])
summary(model1)
VIF(model1)
model=lm(Energy~Pressure+Relativehumidity+Exhaustvacuum,data=train)
summary(model) 
VIF(model)

model1=lm(newen~.,data=train[,-5])
summary(model1)
VIF(model1)
model=lm(newen~Pressure+Relativehumidity+Exhaustvacuum,data=train[,-5])
summary(model) 
VIF(model)

#model validation in test
test=test[,-5]
pred=predict(model,test)
SSE <- sum((test$newen - pred) ^ 2)
SST <- sum((test$newen - mean(test$newen)) ^ 2)
rsquare=1 - SSE/SST
test$pred=pred
rmse=(mean((test$Energy-test$pred)^2))^0.5
rmseTrain=(mean((train$Energy-model$fitted.values)^2))^0.5

#Model Assumption Testing
plot(fitted.values(model),resid(model),col=c("red","yellow"))#homoscedasticity

#Breusch-Pagan test for homoscedasticity

lmtest::bptest(model)

plot(density(resid(model)))#normality
qqnorm(resid(model))
qqline(resid(model))
plot(train$Pressure,resid(model),col=c("red","blue"))#Linearity
plot(train$Relativehumidity,resid(model),col=c("red","yellow"))#Linearity
plot(train$Exhaustvacuum,resid(model),col=c("red","green"))#Linearity

#Actual Vs. Fitted value
plot(fitted.values(model),train$Energy,col=c("red","blue"),main="Actual value vs. Fitted value")#Actual Vs. fitted

# Model validation
res = cv.lm(data=data, (form.lm =Energy~Pressure+Relativehumidity+Exhaustvacuum),m=5,dots=FALSE,seed=29, plotit = c("Observed", "Residual"), legend.pos="topleft")

##k fold summary
k=5
data=data[,-6]
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
for(i in 1:k){
  trainset <- subset(data,id %in% list[-i])
  testset <- subset(data,id %in% c(i))
  model <- lm(trainset$Energy~trainset$Pressure+trainset$Exhaustvacuum+trainset$Relativehumidity)
  predictions <- predict(model,testset[-c(4,5)])
  a <- summary(model)
   rmse <- sqrt(mean(model$residuals^2))
   print(model$coefficients)
   print(a$r.squared)
   print(rmse)
 }

# #Actual Vs. Fitted value
plot(fitted.values(model),data$Energy,col=c("red","blue"),main="Actual value vs. Fitted value")

