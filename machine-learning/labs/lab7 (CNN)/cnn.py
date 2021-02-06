import tensorflow as tf
from tensorflow import keras
from tensorflow.keras.utils import to_categorical
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix


def get_data():
    fashion_mnist = keras.datasets.fashion_mnist
    mnist = keras.datasets.mnist
    # (xs_test, ys_test), (xs_train, ys_train) = mnist.load_data()
    (xs_train, ys_train), (xs_test, ys_test) = fashion_mnist.load_data()

    xs_test = xs_test / 255.0
    xs_train = xs_train / 255.0

    xs_test = np.expand_dims(xs_test, axis=3)
    xs_train = np.expand_dims(xs_train, axis=3)

    return (xs_train, ys_train), (xs_test, ys_test)


def find_opt_filters_count(xs_test, ys_test):
    opt_filters_cnt = 0
    opt_acc = 0
    x_axis = []
    y_axis = []

    for filters_count in range(2, 29, 4):
        print('Training network with filters count = ' + str(filters_count) + '...')

        model = keras.Sequential([
            keras.layers.Conv2D(filters=filters_count, kernel_size=3, input_shape=(28, 28, 1)),
            keras.layers.MaxPooling2D(pool_size=2, padding='same'),
            keras.layers.Dropout(0.5),
            keras.layers.Flatten(),
            keras.layers.Dense(128, activation='relu'),
            keras.layers.Dense(10, activation='softmax')
        ])

        model.compile(optimizer='adam',
                      loss='sparse_categorical_crossentropy',
                      metrics=['accuracy'])

        model.fit(xs_train, ys_train, epochs=3)

        _, test_acc = model.evaluate(xs_test, ys_test, verbose=0)

        x_axis.append(filters_count)
        y_axis.append(test_acc)

        if test_acc > opt_acc:
            opt_acc = test_acc
            opt_filters_cnt = filters_count

    return opt_filters_cnt, x_axis, y_axis


def plot_graph(x_axis, y_axis):
    plt.xlabel("filters count")
    plt.ylabel("accuracy")
    plt.plot(x_axis, y_axis)
    plt.show()


def get_sim_matrix(xs_test, ys_test, predictions_soft, classes_cnt):
    m = []
    sim_matrix = []
    for i in range(classes_cnt):
        m.append([0] * classes_cnt)
        sim_matrix.append([0] * classes_cnt)

    for i in range(len(xs_test)):
        actual = ys_test[i]
        for j in range(classes_cnt):
            if predictions_soft[i][j] > m[actual][j]:
                m[actual][j] = predictions_soft[i][j]
                sim_matrix[actual][j] = i
    return sim_matrix


def draw_images(xs_test, sm, classes_cnt):
    figure = plt.figure(figsize=(8, 8))

    for ind in range(1, classes_cnt ** 2 + 1):
        i = (ind - 1) // classes_cnt
        j = (ind - 1) % classes_cnt

        index = sm[i][j]
        img = xs_test[index]

        figure.add_subplot(classes_cnt, classes_cnt, ind)
        plt.imshow(img)

    plt.show()


(xs_train, ys_train), (xs_test, ys_test) = get_data()
# opt_fc, x_axis, y_axis = find_opt_filters_count(xs_test, ys_test)
# print('\nOptimal filters count = ' + str(opt_fc))
# plot_graph(x_axis, y_axis)
opt_fc = 22

model = keras.Sequential([
    keras.layers.Conv2D(filters=opt_fc, kernel_size=3, input_shape=(28, 28, 1)),
    keras.layers.MaxPooling2D(pool_size=2, padding='same'),
    keras.layers.Dropout(0.5),
    keras.layers.Flatten(),
    keras.layers.Dense(128, activation='relu'),
    keras.layers.Dense(10, activation='softmax')
])
model.compile(optimizer='adam',
              loss='sparse_categorical_crossentropy',
              metrics=['accuracy'])
model.fit(xs_train, ys_train, epochs=10)
test_loss, test_acc = model.evaluate(xs_test, ys_test, verbose=2)


print('\nAccuracy on test data:', test_acc)

predictions_soft = model.predict(xs_test)
predictions = []
for i in range(len(predictions_soft)):
    predictions.append(np.argmax(predictions_soft[i]))

cm = confusion_matrix(ys_test, predictions)

print('confusion matrix:')
print(cm)

sm = get_sim_matrix(xs_test, ys_test, predictions_soft, classes_cnt=10)
draw_images(xs_test, sm, classes_cnt=len(predictions_soft[0]))

