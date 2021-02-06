import warnings

from matplotlib.colors import ListedColormap
from sklearn.tree import DecisionTreeClassifier
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

warnings.filterwarnings("ignore")


def min_max(matrix):
    mm = []
    for i in range(len(matrix[0])):
        mm.append([matrix[:, [i]].min(), matrix[:, [i]].max()])
    return mm


def normalize_minimax(matrix, mm):
    for i in range(len(matrix)):
        for j in range(len(matrix[i])):
            if mm[j][1] == mm[j][0]:
                matrix[i][j] = 0
            else:
                matrix[i][j] = (matrix[i][j] - mm[j][0]) / (mm[j][1] - mm[j][0])


def readNprepare_data(fname):
    dataset = pd.read_csv(fname)
    matrix = dataset.values
    np.random.shuffle(matrix)

    ys = pd.factorize(matrix[:, -1])[0]
    for i in range(0, len(ys)):
        if ys[i] == 0:
            ys[i] = -1

    xs = matrix[:, :-1]
    normalize_minimax(xs, min_max(xs))

    return xs, ys


def calcNP(clf, weights, xs, ys):
    N = 0
    P = 0

    for i in range(0, len(xs)):
        predicted = clf.predict([xs[i]])[0]
        if predicted == -ys[i]:
            N += weights[i]
        else:
            P += weights[i]

    return N, P


def boost(xs, ys, steps):
    w = []
    n = len(xs)
    for i in range(0, n):
        w.append(1 / n)

    alphas = []
    bs = []

    for i in range(0, steps):
        b_i = DecisionTreeClassifier(max_depth=max_depth)
        clf = b_i.fit(xs, ys, sample_weight=w)

        bs.append(clf)

        N, P = calcNP(clf, w, xs, ys)
        if N == 0:
            alpha_i = 0
        elif N == 1:
            alpha_i = 1
        else:
            alpha_i = 0.5 * math.log((1 - N) / N)

        alphas.append(alpha_i)

        for j in range(0, len(w)):
            predicted = clf.predict([xs[j]])[0]
            w[j] = w[j] * math.exp(-alpha_i * ys[j] * predicted)

        w_sum = np.sum(w)
        for j in range(0, len(w)):
            w[j] = w[j] / w_sum

    return alphas, bs


def predict(x, alphas, bs):
    sum = 0

    for i in range(0, len(alphas)):
        predicted = bs[i].predict([x])[0]
        sum += predicted * alphas[i]

    return np.sign(sum)


def k_fold(xs, ys, steps):
    batches_count = 10
    batch_size = len(xs) // batches_count

    left = 0
    right = batch_size

    accuracies = []

    while right < len(xs):
        correctly_predicted = 0

        xs_test = xs[left:right]
        ys_test = ys[left:right]

        xs_train = np.concatenate((xs[:left], xs[right:]))
        ys_train = np.concatenate((ys[:left], ys[right:]))

        alphas, bs = boost(xs_train, ys_train, steps)

        predicted = []
        for i in range(0, len(xs_test)):
            predicted.append(predict(xs_test[i], alphas, bs))

        for i in range(0, len(predicted)):
            if ys_test[i] == predicted[i]:
                correctly_predicted += 1

        accuracies.append(correctly_predicted / len(xs_test))
        left += batch_size
        right = min(len(xs), right + batch_size)

    return np.average(accuracies)


def plot(alphas, bs):
    abcisa = np.arange(0, 1.01, 0.01)
    ordinata = np.arange(0, 1.01, 0.01)

    for x1 in abcisa:
        for y1 in ordinata:
            x_test = [x1, y1]
            predicted = predict(x_test, alphas, bs)
            if predicted == -1:
                plt.plot(x1, y1, ',b')
            else:
                plt.plot(x1, y1, ',r')
    plt.show()


def draw(alphas, bs, xs, ys, figure_index, should_show=False):
    step = 1e-2
    minx, miny = np.amin(xs, 0)
    maxx, maxy = np.amax(xs, 0)

    mesh_x, mesh_y = np.meshgrid(np.arange(minx, maxx, step),
                         np.arange(miny, maxy, step))

    mesh_xy = np.c_[mesh_x.ravel(), mesh_y.ravel()]

    predicted = []
    for dot in mesh_xy:
        predicted.append(predict(dot, alphas, bs))
    predicted = np.array(predicted).reshape(mesh_x.shape)

    figsize = (8, 8)
    plt.figure(num=figure_index, figsize=figsize)

    plt.xlim(minx, maxx)
    plt.ylim(miny, maxy)

    x_0, y_0 = xs[ys == -1].T
    x_1, y_1 = xs[ys == 1].T


    color1 = '#FFAAAA'
    color2 = '#AAAAFF'

    plt.pcolormesh(mesh_x, mesh_y, predicted, cmap=ListedColormap([color1, color2]))
    plt.scatter(x_0, y_0, color='red', s=90)
    plt.scatter(x_1, y_1, color='blue', s=90)

    if should_show:
        plt.show()


def calc_accuracy(xs, ys, alphas, bs):
    predicted = []
    correctly_predicted = 0

    for i in range(0, len(xs)):
        predicted.append(predict(xs[i], alphas, bs))

    for i in range(0, len(predicted)):
        if ys[i] == predicted[i]:
            correctly_predicted += 1

    return  correctly_predicted / len(xs)


fname = '../../datasets/chips.csv'
# fname = '../../datasets/geyser.csv'

max_depth = 3 if fname.endswith('geyser.csv') else 2  # precalculated param

xs, ys = readNprepare_data(fname)

kfold_accuracy = k_fold(xs, ys, 55)
print('kfold accuracy = ' + str(kfold_accuracy))

alphas, bs = boost(xs, ys, 55)
print('accuracy = ' + str(calc_accuracy(xs, ys, alphas, bs)))

verbose_steps = [1, 2, 3, 5, 8, 13, 21, 34, 55]
for i in range(0, len(verbose_steps)):
    st = verbose_steps[i]
    alphas, bs = boost(xs, ys, st)
    draw(alphas, bs, xs, ys, i, st == 55)

