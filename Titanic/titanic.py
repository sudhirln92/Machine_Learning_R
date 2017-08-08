#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 14 16:16:34 2017

@author: sudhir
"""

#Titanic project

#Importing library
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

#Importing Data set 
import os
os.chdir('/home/sudhir/git/ML-R/Titanic')
train = pd.read_csv('train.csv')
test = pd.read_csv('test.csv')
train.shape #Number of col & row
test.shape

train.describe()
test.describe()

#info method provide information about data set like
train.info() 
test.info()

# Find missing values in respective column
train.isnull().sum()
test.isnull().sum()

train_len= len(train)
dataset = pd.concat(objs= [train,test],axis=0) #index(drop=True)
#Data Exploration
dataset.head()

#Fill empty and NaN values with NaN
dataset =dataset.fillna(np.nan)

dataset.isnull().sum()

#Replacing missing value
dataset.Embarked.value_counts()
dataset['Embarked'] = dataset['Embarked'].fillna('S')
dataset['Age']=dataset.Age.fillna(dataset.Age.mean())
dataset.Fare=dataset.Fare.fillna(dataset.Fare.mean())
dataset.drop(['Name','Cabin','Ticket'],inplace =True,axis=1)

#Data set 
dataset.hist(figsize=(9,9),grid=1)
dataset.boxplot()

#Conver Character variable to Numeric
from sklearn.preprocessing import LabelEncoder
le = LabelEncoder()
for column in ['Sex','Embarked']:
    le.fit(list(dataset[column].values))
    dataset[column]=le.transform(dataset[column])

#Splitting of data
x_train1=dataset.iloc[:891,:8]
y_train1 =dataset.iloc[:891,8:]
x_test1=dataset.iloc[891:,:8]
y_test1=dataset.iloc[891:,8:]

# Model building
from sklearn.ensemble import RandomForestClassifier
rf= RandomForestClassifier()
rf.fit(x_train1,y_train1)
rf.score(x_train1,y_train1)

#Predict the test set result
y_pred= rf.predict(x_test1)
le.fit(y_pred)
y_pred = le.transform(y_pred)
#Confusion matrix
#from sklearn.metrics import confusion_matrix
#cm = confusion_matrix()

#Gradient boosting 
from sklearn.ensemble import GradientBoostingClassifier
gbm = GradientBoostingClassifier(learning_rate=0.001,n_estimators=5000)

gbm.fit(x_train1,y_train1)
gbm.score(x_train1,y_train1)

#Predicting the test set result
y_pred= gbm.predict(x_test1)
le.fit(y_pred)
y_pred = le.transform(y_pred)

#Submit classifier
submit = pd.DataFrame({'PassengerId':test['PassengerId'],'Survived':y_pred})
submit.to_csv('titanic.csv',index=False)
