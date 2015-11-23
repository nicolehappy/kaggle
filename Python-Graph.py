import pandas as pd 
from pandas import Series, DataFrame
import pylab
pylab.show()

import numpy as np 
import matplotlib.pyplot as plt
import seaborn as sns


## machine learning 
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC, LinearSVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB

train = pd.read_csv('./kaggle/train.csv')
test = pd.read_csv('./kaggle/test.csv')

### Embarked cities 
train['Embarked'] = train['Embarked'].fillna('S')

#### plot 

sns.factorplot('Embarked', 'Survived',kind ='point', color = 'red', data = train, size = 4, aspect = 3)


pclass = sns.factorplot(x = 'Sex', y = 'Survived', col = 'Pclass',
data = train, saturation = .5, kind = 'bar',  ci = None, aspect = .6)

sns.barplot(x="Sex", y="Survived", hue="Pclass", data=train)


#sns.pointplot(x="Pclass", y="Survived", hue="Sex", data=train,
      palette={"male": "g", "female": "m"},)

pylab.show()


####  References 
#### https://stanford.edu/~mwaskom/software/seaborn/tutorial/categorical.html