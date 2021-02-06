from sklearn.tree import DecisionTreeClassifier
import pandas as pd
import os
import matplotlib.pyplot as plt

DIR = '../../datasets/DT_csv'
TEST = 'test.csv'
TRAIN = 'train.csv'

PARTS_COUNT = 20


def read_dataset(n):
    str_num = str(n) + '_'
    if len(str_num) == 2:
        str_num = '0' + str_num

    train_filename = str_num + TRAIN
    test_filename = str_num + TEST

    train_path = os.path.join(DIR, train_filename)
    test_path = os.path.join(DIR, test_filename)

    train, test = pd.read_csv(train_path), pd.read_csv(test_path)
    xs_train = train.values[:, :-1]
    ys_train = train.values[:, -1]

    xs_test = test.values[:, :-1]
    ys_test = test.values[:, - 1]

    return xs_train, ys_train, xs_test, ys_test


def get_accuracy(xs_test, ys_test, classifier):
    correctly_predicted = 0.0
    for i in range(0, len(xs_test)):
        x = xs_test[i]
        actual = ys_test[i]
        predicted = classifier.predict([x])

        if predicted == actual:
            correctly_predicted += 1

    acc = correctly_predicted / len(xs_test)
    return acc


def train_hyperparams(xs_train, ys_train, xs_test, ys_test):
    splitters = ['best', 'random']
    criterions = ['gini', 'entropy']
    heights = list(range(1, 33))

    opt_height = heights[0]
    opt_splitter = splitters[0]
    opt_criterion = criterions[0]

    opt_accuracy = -1

    for height in heights:
        for splitter in splitters:
            for criterion in criterions:
                tree = DecisionTreeClassifier(max_depth=height, splitter=splitter, criterion=criterion)
                classifier = tree.fit(xs_train, ys_train)
                acc = get_accuracy(xs_test, ys_test, classifier)

                if acc > opt_accuracy:
                    opt_accuracy = acc
                    opt_height = height
                    opt_splitter = splitter
                    opt_criterion = criterion

    return opt_height, opt_splitter, opt_criterion, opt_accuracy


def plot(xs_train, ys_train, xs_test, ys_test):
    heights = list(range(1, 33))

    x_axis = []
    y_axis = []

    for height in heights:
        tree = DecisionTreeClassifier(max_depth=height)
        acc = get_accuracy(xs_test, ys_test, tree.fit(xs_train, ys_train))

        x_axis.append(height)
        y_axis.append(acc)

    plt.xlabel("height")
    plt.ylabel("accuracy")
    plt.plot(x_axis, y_axis)
    # plt.show()


def random_forest(xs_train, ys_train, xs_test, ys_test, forest_size=50):
    forest = []
    correctly_predicted = 0.0

    max_features = "sqrt"

    for i in range(0, forest_size):
        tree = DecisionTreeClassifier(max_features=max_features)
        forest.append(tree.fit(xs_train, ys_train))

    for i in range(0, len(xs_test)):
        x = xs_test[i]

        predicted2cnt = dict()

        for tree in forest:
            predicted = tree.predict([x])[0]

            if predicted not in predicted2cnt:
                predicted2cnt[predicted] = 0

            predicted2cnt[predicted] += 1

        mx = -1
        cls = -1

        for c, cnt in predicted2cnt.items():
            if cnt > mx:
                cls = c
                mx = cnt

        if cls == ys_test[i]:
            correctly_predicted += 1

    acc = correctly_predicted / len(xs_test)
    return acc


min_height = 100
max_height = -100

min_height_index = -1
max_height_index = 1

for i in range(1, PARTS_COUNT + 1):
    xs_train, ys_train, xs_test, ys_test = read_dataset(i)
    height, splitter, criterion, accuracy = train_hyperparams(xs_train, ys_train, xs_test, ys_test)

    print('optimal accuracy on single tree with height=' + str(height) + ', criterion=' + str(criterion) + ', splitter='
          + str(splitter) + ' = ' + str(accuracy))

    if height > max_height:
        max_height = height
        max_height_index = i

    if height < min_height:
        min_height = height
        min_height_index = i

print('max height = ' + str(max_height))
print('min height = ' + str(min_height))

print('min height dataset = ' + str(min_height_index))
print('max height dataset = ' + str(max_height_index))

xs_train_min, ys_train_min, xs_test_min, ys_test_min = read_dataset(min_height_index)
plot(xs_train_min, ys_train_min, xs_test_min, ys_test_min)

xs_train_max, ys_train_max, xs_test_max, ys_test_max = read_dataset(max_height_index)
plot(xs_train_max, ys_train_max, xs_test_max, ys_test_max)


for i in range(1, PARTS_COUNT + 1):
    xs_train, ys_train, xs_test, ys_test = read_dataset(i)
    print('random forest accuracy on test part #' + str(i) + ' is ' +
          str(random_forest(xs_train, ys_train, xs_test, ys_test)))
    print('random forest accuracy on train part #' + str(i) + ' is ' +
          str(random_forest(xs_train, ys_train, xs_train, ys_train)))

