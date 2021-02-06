import os
import math
import numpy as np
import matplotlib.pyplot as plt

PATH = '../../datasets/messages/part'
SPAM_LABEL = 'spmsg'
LEGIT_LABEL = 'legit'
SUBJECT_LABEL = 'Subject:'
Q = 2


class NaiveBayesClassifier:
    def __init__(self, lambdas, alpha):
        self.lambdas = lambdas
        self.alpha = alpha
        self.class2word2prob = [dict(), dict()]
        self.class2objcnt = [0, 0]
        self.class2word2cnt = [dict(), dict()]
        self.words_distinct = set()
        self.D = 0

    def fit(self, xs_train, ys_train):
        self.D = len(xs_train)

        for i in range(0, len(xs_train)):
            self.class2objcnt[ys_train[i]] += 1.0

            ngrams = xs_train[i]
            self.words_distinct |= ngrams
            word2cnt = self.class2word2cnt[ys_train[i]]

            for item in ngrams:
                if item not in word2cnt:
                    word2cnt[item] = 0.0


                word2cnt[item] += 1

        for i in (0, 1):
            word2prob = self.class2word2prob[i]
            word2cnt = self.class2word2cnt[i]

            for (w, cnt) in word2cnt.items():
                nom = self.alpha + cnt
                denom = self.class2objcnt[i] + Q * self.alpha

                word2prob[w] = (nom, denom)

    def predict_soft(self, x):
        s = x
        probs = []

        for j in (0, 1):
            word2prob = self.class2word2prob[j]
            prob = math.log(self.lambdas[j]) + math.log(self.class2objcnt[j]) - math.log(self.D)

            for w in self.words_distinct:
                if w in word2prob:
                    a, b = word2prob[w]
                    pr_log = math.log(a) - math.log(b)
                else:
                    a = self.alpha
                    b = self.class2objcnt[j] + Q * self.alpha
                    pr_log = math.log(a) - math.log(b)

                if w in s:
                    prob = prob + pr_log
                else:
                    prob = prob + math.log(b - a) - math.log(b)

            probs.append(prob)

        return probs

    def predict(self, x):
        probs = self.predict_soft(x)
        if probs[0] > probs[1]:
            argmax = 0
        else:
            argmax = 1

        return argmax

    def calc_accuracy(self, xs_test, ys_test):
        correctly_predicted = 0
        for i in range(0, len(xs_test)):
            predicted = self.predict(xs_test[i])
            actual = ys_test[i]

            if predicted == actual:
                correctly_predicted += 1

        return correctly_predicted / len(xs_test)


def k_fold(n, lambdas, alpha):
    accuracies = []

    for test_part in range(1, 11):
        xs_train = []
        ys_train = []

        xs_test = []
        ys_test = []

        for train_part in range(1, 11):

            if test_part == train_part:
                continue

            cur_path = PATH + str(train_part)
            files = os.listdir(cur_path)

            for file in files:
                y = 0
                if LEGIT_LABEL in file:
                    y = 1

                ngrams = get_ngrams_from_file(os.path.join(cur_path, file), n)

                xs_train.append(ngrams)
                ys_train.append(y)

        clf = NaiveBayesClassifier(lambdas=lambdas, alpha=alpha)
        clf.fit(xs_train, ys_train)

        test_files = os.listdir(PATH + str(test_part))
        for file in test_files:
            y = 0
            if LEGIT_LABEL in file:
                y = 1

            s = get_ngrams_from_file(os.path.join(PATH + str(test_part), file), n)

            xs_test.append(s)
            ys_test.append(y)

        accuracies.append(clf.calc_accuracy(xs_test, ys_test))

    return np.average(accuracies)


def poly_hash(a):
    h = 0
    prime = 31
    mod = 1e9 + 7

    for i in range(0, len(a)):
        h = (h + (prime ** i) * a[i]) % mod

    return h


def get_ngrams_from_file(path, n):
    with open(path) as f:
        for line in f:
            # if not line.strip() or line.startswith(SUBJECT_LABEL):
            if not line.strip():
                continue

            s = set()
            if line.startswith(SUBJECT_LABEL):
                words = list(map(int, line.split()[1:]))
            else:
                words = list(map(int, line.split()))

            for i in range(n - 1, len(words)):
                window = words[i - n + 1:i + 1]
                s.add(poly_hash(window))

            return s


def get_full_dataset(n):
    xs = []
    ys = []

    for i in range(1, 11):
        cur_path = PATH + str(i)
        files = os.listdir(cur_path)

        for file in files:
            y = 0
            if LEGIT_LABEL in file:
                y = 1

            x = get_ngrams_from_file(os.path.join(cur_path, file), n)

            xs.append(x)
            ys.append(y)

    return xs, ys


def draw_roc_curve(clf, xs, ys):
    probs = list(map(clf.predict_soft, xs))
    probs = list(map(lambda zs: [zs[0] / (zs[0] + zs[1]), zs[1] / (zs[0] + zs[1])], probs))
    probs.sort(key=lambda x: -x[0])

    legit_count = len(list(filter(lambda x: x == 1, ys)))
    spam_count = len(ys) - legit_count

    step_y = 1 / legit_count
    step_x = 1 / spam_count

    x_axis = []
    y_axis = []

    x = 0
    y = 0

    for i in range(0, len(xs)):
        if probs[i][0] > probs[i][1]:
            y += step_y
        else:
            x += step_x

        x_axis.append(x)
        y_axis.append(y)

    plt.xlabel("false positive rate")
    plt.ylabel("true positive rate")
    plt.plot(x_axis, y_axis)
    plt.show()


def plot_accuracy(xs, ys):
    lambdas = [math.e]
    for i in range(1, 13):
        lambdas.append(10 ** i)

    x_axis = lambdas
    y_axis = []

    for lmbd in lambdas:
        clf = NaiveBayesClassifier([math.e, lmbd], 1e-4)
        clf.fit(xs, ys)
        acc = clf.calc_accuracy(xs, ys)
        y_axis.append(acc)

    plt.xlabel("λ legit")
    plt.ylabel("accuracy")
    plt.plot(x_axis, y_axis)
    plt.show()


def check_not_legit_as_spam(xs, ys, lambda_legit):
    clf = NaiveBayesClassifier([math.e, lambda_legit], 1e-4)
    clf.fit(xs, ys)
    legit_as_spam = 0
    for i in range(0, len(xs)):
        if clf.predict(xs[i]) == 0 and ys[i] == 1:
            legit_as_spam += 1

    return legit_as_spam == 0


xs, ys = get_full_dataset(2)

# clf = NaiveBayesClassifier([math.e, math.e], 1e-4)
# clf.fit(xs, ys)
# draw_roc_curve(clf, xs, ys)

# plot_accuracy(xs, ys)

print(check_not_legit_as_spam(xs, ys, 1e12))
