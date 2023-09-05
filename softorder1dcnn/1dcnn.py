# Not working properly in RStudio or Visual Studio

# Resort to ipython and Spyder IDE

# This notebook is inspired by the [2nd place solution of Mechanisms of Action
# (MoA)Prediction](https://www.kaggle.com/c/lish-moa/discussion/202256).


import numpy as np   # linear algebra
import pandas as pd  # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list
# all files under the input directory

# import os
# for dirname, _, filenames in os.walk('/kaggle/input'):
#     for filename in filenames:
#         print(os.path.join(dirname, filename))

# You can write up to 20GB to the current directory (/kaggle/working/) that
# gets preserved as output when you create a version using "Save & Run All"
# You can also write temporary files to /kaggle/temp/, but they won't be saved
# outside of the current session

from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold
from sklearn.model_selection import train_test_split
from tensorflow import keras
from tensorflow.keras import layers


# train = pd.read_csv('../input/tabular-playground-series-jan-2021/train.csv')
# test = pd.read_csv('../input/tabular-playground-series-jan-2021/test.csv')

dataset_train = pd.read_csv("santander/train.csv")
dataset_test = pd.read_csv("santander/test.csv")

# numerical_cols = [f'cont{i}' for i in range(1, 15)]
target_col = 'target'

# for c in numerical_cols:
#     prep = StandardScaler()
#     train[c] = prep.fit_transform(train[[c]])
#     test[c] = prep.transform(test[[c]])

X_train = dataset_train.drop(['ID_code', 'target'], axis=1)
y_train = dataset_train['target']
X_test = dataset_test.drop('ID_code', axis=1)

X_train.head(2)
X_test.head(2)

# Just for now, use X_test as validation. Sort out later
X_val = X_test

# cv = KFold(n_splits=5, shuffle=True, random_state=7)
#
# y_preds = []
# models = []
# oof_train = np.zeros((len(X_train),))
#
# for fold_id, (train_index, valid_index) in enumerate(cv.split(X_train, y_train)):
#     X_tr = X_train.loc[train_index, :]
#     X_val = X_train.loc[valid_index, :]
#     y_tr = y_train.loc[train_index]
#     y_val = y_train.loc[valid_index]

model = keras.Sequential([
    #   layers.BatchNormalization(),
    #   layers.Dropout(0.1),
    layers.Dense(4096, activation='relu'),
    layers.Reshape((256, 16)),
    #   layers.BatchNormalization(),
    #   layers.Dropout(0.1),
    layers.Conv1D(filters=16, kernel_size=5, strides=1, activation='relu'),
    layers.MaxPooling1D(pool_size=2),
    layers.Flatten(),
    layers.Dense(16, activation='relu'),
    layers.Dense(1, activation='linear'),
])

model.compile(
  optimizer='adam',
  loss='mse',
  metrics=[keras.metrics.RootMeanSquaredError()]
  )

early_stopping = keras.callbacks.EarlyStopping(
    patience=10,
    min_delta=0.001,
    restore_best_weights=True,
)

model.fit(
    X_train, y_train,
    # validation_data=(X_val, y_val),
    batch_size=32,
    epochs=5,
    validation_split=0.2,
    callbacks=[early_stopping],
)

# oof_train[valid_index] = model.predict(X_val).reshape(1, -1)[0]
y_pred = model.predict(X_test).reshape(1, -1)[0]

y_pred.append(y_pred)
model.append(model)
