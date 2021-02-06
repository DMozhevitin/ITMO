import numpy as np
import pandas as pd
from random import randint
from math import exp
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap

eps = 1e-9


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


def linear_kernel(a, b):
    return np.dot(a, b)


def poly_kernel(a, b, deg):
    return linear_kernel(a, b) ** deg


def gauss_kernel(a, b, beta):
    c = np.array(a) - np.array(b)
    return exp(-beta * linear_kernel(c, c))


name2kernel = dict(linear=poly_kernel, poly=poly_kernel, gauss=gauss_kernel)
kernel_name2params = dict(linear=[1], poly=[2, 3, 4, 5], gauss=[1, 2, 3, 4, 5])
C_values = [0.05, 0.1, 0.5, 1.0, 5.0, 10.0, 50.0, 100.0]


def sign(x):
    if x < 0:
        return -1
    return 1


def calc_f(ind, kernels, ys, a, b):
    res = 0

    for i in range(0, len(kernels)):
        res += a[i] * ys[i] * kernels[ind][i]

    return res + b


def calc_b(b, E, dp1, dp2, i, j, ai_old, aj_old, a):
    return b - E - ys[i] * (a[i] - ai_old) * dp1 - ys[j] * (a[j] - aj_old) * dp2


def SMO(C, max_it, kernels, ys, tol):
    n = len(kernels)
    a = [0] * n
    b = 0
    it = 0

    while it <= max_it:
        changed = 0

        for i in range(0, n):
            E_i = calc_f(i, kernels, ys, a, b) - ys[i]
            if (ys[i] * E_i < -tol and a[i] < C) or (ys[i] * E_i > tol and a[i] > 0):
                j = randint(0, n - 1)
                while j == i:
                    j = randint(0, n - 1)

                E_j = calc_f(j, kernels, ys, a, b) - ys[j]

                ai_old = a[i]
                aj_old = a[j]

                lb = 0
                h = 0

                if ys[i] != ys[j]:
                    lb = max(0, a[j] - a[i])
                    h = min(C, C + a[j] - a[i])
                else:
                    lb = max(0, a[i] + a[j] - C)
                    h = min(C, a[i] + a[j])

                if lb == h:
                    continue

                nu = 2 * kernels[i][j] - kernels[i][i] - kernels[j][j]

                if nu >= 0:
                    continue

                a[j] = a[j] - (ys[j] * (E_i - E_j)) / nu

                if a[j] > h:
                    a[j] = h
                elif a[j] < lb:
                    a[j] = lb

                if abs(a[j] - aj_old) < eps:
                    continue

                a[i] = a[i] + ys[i] * ys[j] * (aj_old - a[j])

                b1 = b - E_i - ys[i] * (a[i] - ai_old) * kernels[i][i] - ys[j] * (a[j] - aj_old) * kernels[i][j]
                b2 = b - E_j - ys[i] * (a[i] - ai_old) * kernels[i][j] - ys[j] * (a[j] - aj_old) * kernels[j][j]

                if 0 < a[i] < C:
                    b = b1
                elif 0 < a[j] < C:
                    b = b2
                else:
                    b = (b1 + b2) / 2

                changed += 1

            if changed == 0:
                it += 1
            else:
                it = 0

    return a, b


def train(kernels, ys, C):
    a, b = SMO(C=C, max_it=30, kernels=kernels, ys=ys, tol=0.00000000001)
    return a, b


# single x -> prediction
def predict(xs, ys, a, b, x, kernel, deg):
    sum = 0

    for i in range(0, len(xs)):
        sum += a[i] * ys[i] * kernel(xs[i], x, deg)

    return int(np.sign(sum + b))


# xs_test -> ys_predicted
def predict2(xs_train, ys_train, xs_test, a, b, kernel, deg):
    predicted = [0] * len(xs_test)

    for i in range(0, len(xs_test)):
        x = xs_test[i]

        for j in range(0, len(xs_train)):
            predicted[i] += a[j] * ys_train[j] * kernel(xs_train[j], x, deg)

        predicted[i] += b

    return predicted


def accuracy(predicted, actual):
    tp = 0
    fp = 0
    fn = 0
    tn = 0

    p = 0
    n = 0

    for i in range(0, len(predicted)):
        if predicted[i] == 1 and actual[i] == 1:
            tp += 1

        if predicted[i] == 1 and actual[i] == 0:
            fp += 1

        if predicted[i] == -1 and actual[i] == 1:
            fn += 1

        if predicted[i] == -1 and actual[i] == -1:
            tn += 1

        if actual[i] == 1:
            p += 1

        if actual[i] == -1:
            n += 1

    return (tp + tn) / (p + n)


def get_optimal_hyperparams(xs, ys):
    opt_kernel = ''
    opt_accuracy = -1
    opt_C = -1
    opt_param = -1


    batches_count = 10
    batch_size = len(xs) // batches_count

    for C in C_values:
        for kname in name2kernel.keys():
            params = kernel_name2params.get(kname)
            for k in range(0, len(params)):
                left = 0
                right = batch_size
                acc = []

                a = []
                b = -1
                while right < len(xs):
                    xs_test = xs[left:right]
                    xs_train = np.concatenate((xs[:left], xs[right:]))
                    ys_train = np.concatenate((ys[:left], ys[right:]))

                    kernel_values = []

                    for i in range(0, len(xs_train)):
                        kernel_values.append([])
                        for j in range(0, len(xs_train)):
                            kernel_values[i].append(name2kernel.get(kname)(xs_train[i], xs_train[j], params[k]))

                    a, b = train(kernel_values, ys_train, C)

                    predicted = []
                    actual = ys[left:right]

                    for i in range(0, len(xs_test)):
                        predicted.append(
                            predict(xs_train, ys_train, a, b, xs_test[i], name2kernel.get(kname), params[k]))

                    acc.append(accuracy(predicted, actual))

                    left += batch_size
                    right = min(n, right + batch_size)

                avg_acc = np.average(acc)
                print('kernel=' + kname + ' average accuracy = ' + str(avg_acc) + ' C=' + str(C) + ' param=' + str(
                    params[k]))

                if avg_acc > opt_accuracy:
                    opt_kernel = kname
                    opt_C = C
                    opt_accuracy = avg_acc
                    opt_param = params[k]

    return opt_kernel, opt_accuracy, opt_C, opt_param


def draw(xs, ys, a, b, kernel, deg):
    plt.scatter(xs[:, 0], xs[:, 1], c=ys, s=20, cmap=plt.cm.Paired)
    axis = plt.gca()

    x_lim = axis.get_xlim()
    y_lim = axis.get_ylim()

    lin_space_x = np.linspace(x_lim[0], x_lim[1], 30)
    lin_space_y = np.linspace(y_lim[0], y_lim[1], 30)

    (meshgrid_y, meshgrid_x) = np.meshgrid(lin_space_y, lin_space_x)

    print('meshgrid_x.shape = ' + str(meshgrid_x.shape))

    points = np.vstack([meshgrid_x.ravel(), meshgrid_y.ravel()]).T
    print('len(points)='+str(len(points)))
    precicted = np.array(predict2(xs_train=xs, ys_train=ys, xs_test=points, a=a, b=b, kernel=kernel, deg=deg))\
        .reshape(meshgrid_x.shape)

    axis.contourf(meshgrid_x, meshgrid_y, precicted, levels=[-100, 0, 100], alpha=0.2, colors=['#0000ff', '#ff0000'])
    axis.contour(meshgrid_x, meshgrid_y, precicted, levels=[-1, 0, 1], alpha=1,
               linestyles=['--', '-', '--'], colors='k')

    plt.show()


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


filename = '../../datasets/geyser.csv'  # also can be 'chips.csv'
xs, ys = readNprepare_data(filename)

n = len(xs)
m = len(xs[0])

# opt_kernel, opt_accuracy, opt_C, opt_param = get_optimal_hyperparams(xs, ys)

if filename == 'chips.csv':
    opt_kernel = 'gauss'
    opt_C = 100.0
    opt_param = 5
elif filename == 'geyser.csv':
    opt_kernel = 'poly'
    opt_C = 50.0
    opt_param = 2
else:
    opt_kernel, opt_accuracy, opt_C, opt_param = get_optimal_hyperparams(xs, ys)

# print('opt accuracy = ' + str(opt_accuracy))
print('opt C = ' + str(opt_C))
print('opt kernel = ' + opt_kernel + ' with param ' + str(opt_param))

opt_kernels = []

for i in range(0, len(xs)):
    opt_kernels.append([])
    for j in range(0, len(xs)):
        opt_kernels[i].append(name2kernel.get(opt_kernel)(xs[i], xs[j], opt_param))


opt_a, opt_b = train(opt_kernels, ys, opt_C)
draw(xs, ys, opt_a, opt_b, name2kernel.get(opt_kernel), opt_param)
