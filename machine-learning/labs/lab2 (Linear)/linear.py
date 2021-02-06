import numpy as np
import random
import matplotlib.pyplot as mtp


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


def calc_gradient(w, x, y):
    grad = []
    dot = np.dot(x, w)

    for i in range(0, len(x)):
        grad.append(2 * (dot - y) * x[i])

    return np.array(grad)


def gradient_descent(w_start, xs, ys, steps):
    w = np.copy(w_start)

    for k in range(1, steps):
        mu = 1 / k
        i = random.randint(0, n - 1)

        grad = calc_gradient(w, xs[i], ys[i])
        t = (np.dot(xs[i], w) - ys[i]) / np.dot(xs[i], grad)
        grad += t * w

        w = w * (1 - t * mu) - mu * grad
        # w -= mu * t * grad
        k += 1

    return w


def least_squares_method(m, y, t):
    mT = np.transpose(m)
    n, _ = mT.shape
    E = np.identity(n)
    return np.linalg.inv(mT @ m + t * E) @ mT @ y


def calc_smape(w, test_xs, test_ys):
    smape = 0

    for i in range(0, len(test_xs)):
        expected = test_ys[i]
        actual = np.dot(w, test_xs[i])

        smape += (np.abs(actual - expected)) / (np.abs(expected) + np.abs(actual))

    smape /= len(test_xs)

    return smape


def normalize_targets(ys):
    y_min = np.min(ys)
    y_max = np.max(ys)

    for i in range(0, len(ys)):
        ys[i] = (ys[i] - y_min) / (y_max - y_min)


with open("datasets/dataset_linear.txt") as file:
    m = int(next(file))
    n = int(next(file))
    xs = []
    ys = []

    for i in range(n):
        vec = list(map(int, next(file).split()))
        xs.append(vec[:-1])
        xs[i].append(1)
        ys.append(vec[-1])

    xs = np.array(xs).astype(float)
    ys = np.array(ys).astype(float)

    normalize_minimax(xs, min_max(xs))
    normalize_targets(ys)

    # gradient descent method

    left_bound = - 1 / (2 * n)
    right_bound = 1 / (2 * n)

    w_start = []
    for i in range(0, len(xs[0])):
        w_start.append(random.uniform(left_bound, right_bound))

    w_start = np.array(w_start)


    ksteps = 100
    t = int(next(file))
    test_xs = []
    test_ys = []

    for i in range(0, t):
        vec = list(map(int, next(file).split()))
        test_xs.append(vec[:-1])
        test_xs[i].append(1)

        test_ys.append(vec[-1])

    points_x = []
    points_y = []

    while ksteps <= 100000:
        w = gradient_descent(w_start, xs, ys, ksteps)

        smape = calc_smape(w, test_xs, test_ys)

        points_x.append(ksteps)
        points_y.append(smape)

        ksteps *= 10

    mtp.xlabel("steps")
    mtp.ylabel("SMAPE")

    mtp.plot(points_x, points_y)
    mtp.show()

    # least squares method

    w_ls = least_squares_method(xs, ys, 0.0000000001)
    smape_ls = calc_smape(w_ls, test_xs, test_ys)
    print('smape for least squares method = ' + str(smape_ls))
