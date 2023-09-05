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
import numpy as np
import pandas as pd  # data processing, CSV file I/O (e.g. pd.read_csv)

from tensorflow import keras
from tensorflow.keras import layers


dataset_train = pd.read_csv("../data/pseudos3.csv")
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
    layers.Dense(3, activation='linear'),
])

model.compile(
  optimizer='rmsprop',
  loss='sparse_categorical_crossentropy',
  metrics=["accuracy"]
  )

# Randomise data
indices_permutation = np.random.permutation(len(X_train))
shuffled_inputs = X_train.iloc[indices_permutation]
shuffled_targets = y_train.iloc[indices_permutation]
# Split into 30:70
num_validation_samples = int(0.3 * len(X_train))
val_inputs = shuffled_inputs[:num_validation_samples]
val_targets = shuffled_targets[:num_validation_samples]
training_inputs = shuffled_inputs[num_validation_samples:]
training_targets = shuffled_targets[num_validation_samples:]

#
# early_stopping = keras.callbacks.EarlyStopping(
# patience=10,
# min_delta=0.001,
# restore_best_weights=True,
# )
#

history = model.fit(
    training_inputs, training_targets,
    validation_data=(val_inputs, val_targets),
    batch_size=32,
    epochs=100
    # callbacks=[early_stopping],
)

loss_and_metrics = model.evaluate(val_inputs, val_targets, batch_size=128)

accuracy = history.history["accuracy"]
val_accuracy = history.history["val_accuracy"]
loss = history.history["loss"]
val_loss = history.history["val_loss"]
epochs = range(1, len(loss) + 1)
plt.figure()
plt.plot(epochs, accuracy, "bo", label="Training accuracy")
plt.plot(epochs, val_accuracy, "b", label="Validation accuracy")
plt.title("Training and validation accuracy")
plt.legend()
plt.figure()
plt.plot(epochs, loss, "bo", label="Training loss")
plt.plot(epochs, val_loss, "b", label="Validation loss")
plt.title("Training and validation loss")
plt.legend()
plt.show()
