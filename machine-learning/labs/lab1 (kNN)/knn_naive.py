import numpy as np
import pandas as pd
import matplotlib.pyplot as mtp

h = 0.05
delta = 0.1


def kernel(x):  # param
    return 35 / 32 * (1 - x ** 2) ** 3

# def kernel(x):
#     return 70 / 81 * (1 - abs(x) ** 2) ** 3


# def kernel(x):
#     return np.exp(-(x * x) / 2) / np.sqrt(2 * np.pi)


def min_max(matrix):
    mm = []
    for i in range(len(matrix[0])):
        mm.append([matrix[:, [i]].min(), matrix[:, [i]].max()])
    return mm


def normalize_minimax(matrix, mm):
    for i in range(len(matrix)):
        for j in range(len(matrix[i])):
            matrix[i][j] = (matrix[i][j] - mm[j][0]) / (mm[j][1] - mm[j][0])


def dist(x, y):
    s = 0
    for i in range(len(x)):
        s += (x[i] - y[i]) ** 2
    return np.sqrt(s)


def weight(x, x_i):
    return kernel(dist(x, x_i) / h)


def nadaraya_watson(matrix, x, ys):
    weights = []
    weights_y = []

    for i in range(len(matrix)):
        weights.append(weight(x, matrix[i]))
        weights_y.append(np.array(ys[i]) * weight(x, matrix[i]))

    return np.array(weights_y).sum() / np.array(weights).sum()



def precision(tp, fp):
    if tp + fp == 0:
        return 0
    return tp / (tp + fp)


def recall(tp, p):
    if p == 0:
        return 0
    return tp / p


def f1measure(precision, recall):
    if precision + recall == 0:
        return 0
    return 2 * (precision * recall) / (precision + recall)


if __name__ == '__main__':
    ds = pd.read_csv('../../datasets/dataset_knn.csv')

    matrix = ds.values
    rows, cols = matrix.shape

    last = matrix[:, -1]
    last = pd.factorize(last)[0]
    classes = np.max(last) + 1

    ys = []

    for i in range(len(matrix)):
        ys.append(last[i])

    ys = np.array(ys)
    xs = matrix[:, :-1]

    normalize_minimax(xs, min_max(xs))

    points_y = []
    points_x = []
    while h < 5:
        res = []

        for i in range(len(matrix)):
            res.append(round(nadaraya_watson(np.concatenate((xs[:i], xs[i:])), xs[i], ys)))

        print(res)
        fp = 0
        fn = 0
        tp = 0
        tn = 0
        p = 0

        for i in range(len(res)):
            if ys[i] == 1:
                p += 1
            if res[i] == 1 and ys[i] == 1:
                tp += 1
            elif res[i] == 0 and ys[i] == 0:
                tn += 1
            elif res[i] == 1 and ys[i] == 0:
                fp += 1
            else:
                fn += 1

        f1 = f1measure(precision(tp, fp), recall(tp, p))

        points_y.append(f1)
        points_x.append(h)
        h += delta
        print(f1)

    mtp.xlabel("h")
    mtp.ylabel("F1")
    mtp.plot(points_x, points_y)
    mtp.show()
