import pandas as pd
import numpy as np

bank = pd.read_csv('/content/BankCustomers.csv')
bank.head()
X = bank.iloc[:, 3:13]
y = bank.iloc[:, 13]

X.head()

geo = pd.get_dummies(X.Geography,drop_first= True)
gen = pd.get_dummies(X.Gender,drop_first= True)

# drop the Geography and Gender
X.drop(['Geography','Gender'],axis=1,inplace=True)

X = pd.concat([X,geo,gen],axis=1)

from sklearn.model_selection import train_test_split

X_train,X_test,y_train,y_test = train_test_split(X,y,test_size =0.2,random_state = 1)

X_train.shape

# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

y_train.shape

# Now Lets make ANN

import keras
from keras.models import Sequential
from keras.layers import Dense

model = Sequential()

# classifier.add(Dense(activation="relu", input_dim=11, units=6, kernel_initializer="uniform"))
model.add(Dense(activation = 'relu',input_dim=11,units =6,kernel_initializer="uniform"))

model.add(Dense(activation = 'relu',units =6,kernel_initializer="uniform"))

model.add(Dense(activation = 'softmax',units =2,kernel_initializer="uniform"))

from keras.optimizers import Adam

# Compiling the ANN
model.compile(Adam(lr=0.01), loss='sparse_categorical_crossentropy', metrics = ['accuracy'])

model.fit(X_train,y_train, batch_size = 10, nb_epoch = 10)

y_pred = model.predict(X_test)
y_pred = (y_pred > 0.5)

# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix,accuracy_score
cm = confusion_matrix(y_test, y_pred)
accuracy=accuracy_score(y_test,y_pred)
accuracy




