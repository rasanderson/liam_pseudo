# Not working properly in RStudio or Visual Studio

# Resort to ipython and Spyder IDE

# This notebook is inspired by the [2nd place solution of Mechanisms of Action
# (MoA)Prediction](https://www.kaggle.com/c/lish-moa/discussion/202256).


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

import matplotlib.pyplot as plt
import pandas as pd  # data processing, CSV file I/O (e.g. pd.read_csv)

from tensorflow import keras
from tensorflow.keras import layers


dataset_train = pd.read_csv("../data/pseudos2.csv")
target_col = 'target'


X_train = dataset_train.drop(['ID_Code', 'target'], axis=1)
y_train = dataset_train['target']

X_train.head(2)
y_train.head(2)

model = keras.Sequential([
    layers.BatchNormalization(),
    layers.Dropout(0.1),
    layers.Dense(4096, activation='relu'),
    layers.Reshape((256, 16)),
    layers.BatchNormalization(),
    layers.Dropout(0.1),
    layers.Conv1D(filters=16, kernel_size=5, strides=1, activation='relu'),
    layers.MaxPooling1D(pool_size=2),
    layers.Flatten(),
    layers.Dense(16, activation='relu'),
    layers.Dense(1, activation='linear'),
])

model.compile(
  optimizer='adam',
  loss='mse',
  metrics=["mae"]
  )

early_stopping = keras.callbacks.EarlyStopping(
    patience=10,
    min_delta=0.001,
    restore_best_weights=True,
)

history = model.fit(
    X_train, y_train,
    # validation_data=(X_val, y_val),
    batch_size=32,
    epochs=5,
    validation_split=0.2,
    callbacks=[early_stopping],
)


loss = history.history["mae"]
val_loss = history.history["val_mae"]
epochs = range(1, len(loss) + 1)
plt.figure()
plt.plot(epochs, loss, "bo", label="Training MAE")
plt.plot(epochs, val_loss, "b", label="Validation MAE")
plt.title("Training and validation MAE")
plt.legend()
plt.show()
