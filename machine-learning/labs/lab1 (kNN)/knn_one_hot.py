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

    return np.sum(np.array(weights_y), axis=0) / np.array(weights).sum()


def one_hot(x, cnt):
    res = []
    for i in range(cnt):
        if i == x:
            res.append(1)
        else:
            res.append(0)
    return res


def round_one_hot(xs):
    res = []
    ind = np.argmax(xs)
    for i in range(len(xs)):
        if i == ind:
            res.append(1)
        else:
            res.append(0)
    return res


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
        ys.append(one_hot(last[i], classes))

    ys = np.array(ys)
    xs = matrix[:, :-1]

    normalize_minimax(xs, min_max(xs))

    points_y = []
    points_x = []
    while h < 5:
        res = []

        for i in range(len(matrix)):
            res.append(round_one_hot(nadaraya_watson(np.concatenate((xs[:i], xs[i:])), xs[i], ys)))
            # res.append(nadaraya_watson(np.concatenate((xs[:i], xs[i:])), xs[i], ys))

        fps = [0] * classes
        fns = [0] * classes
        tps = [0] * classes
        tns = [0] * classes
        ps = [0] * classes

        for i in range(len(res)):
            for j in range(len(res[i])):
                if ys[i][j] == 1:
                    ps[j] += 1
                if res[i][j] == 1 and ys[i][j] == 1:
                    tps[j] += 1
                elif res[i][j] == 0 and ys[i][j] == 0:
                    tns[j] += 1
                elif res[i][j] == 1 and ys[i][j] == 0:
                    fps[j] += 1
                else:
                    fns[j] += 1

        f1s = []
        for i in range(classes):
            f1s.append(f1measure(precision(tps[i], fps[i]), recall(tps[i], ps[i])))

        f1 = np.average(f1s)
        points_y.append(f1)
        points_x.append(h)
        h += delta
        print(f1)

    mtp.xlabel("h")
    mtp.ylabel("F1")
    mtp.plot(points_x, points_y)
    mtp.show()
