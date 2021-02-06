import pandas as pd
import numpy as np
from sklearn.metrics import adjusted_rand_score
from sklearn.metrics import silhouette_score
from sklearn.metrics import calinski_harabasz_score
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA

DATASET_PATH = '../../datasets/wine.csv'
EPS = 1e-12


def min_max(matrix):
    mm = []
    for i in range(len(matrix[0])):
        mm.append([matrix[:, [i]].min(), matrix[:, [i]].max()])
    return mm


def normalize_minimax(matrix, mm):
    for i in range(len(matrix)):
        for j in range(len(matrix[i])):
            matrix[i][j] = (matrix[i][j] - mm[j][0]) / (mm[j][1] - mm[j][0])


def has_difference(a, b):
    return any(dist_sqr(a, b) > EPS)


def dist_sqr(a, b):
    return sum(map(lambda p: (p[0] - p[1]) ** 2, zip(a, b)))


def k_means(xs, clusters_cnt):
    n = len(xs)
    m = len(xs[0])
    prev_centers = []

    for i in range(clusters_cnt):
        prev_centers.append([0.0] * m)
    prev_centers = np.array(prev_centers)

    random_indices = np.random.choice(n, clusters_cnt, False)
    centers = xs[random_indices]
    clusters = [0] * n

    while has_difference(centers, prev_centers):
        prev_centers = np.copy(centers)

        for i in range(n):
            clusters[i] = np.array(list(map(lambda c: dist_sqr(xs[i], c), prev_centers))).argmin()

        clust2points = dict()

        for i in range(n):
            clust = clusters[i]
            x = xs[i]

            if clust not in clust2points:
                clust2points[clust] = []

            clust2points[clust].append(x)

        for (clust, points) in clust2points.items():
            centers[clust] = np.sum(points, axis=0) / len(points)

    return clusters


def calc_metrics(xs, ys, clusters_cnt):
    ys_predicted = k_means(xs, clusters_cnt)
    rand_index = adjusted_rand_score(ys, ys_predicted)
    silhouette = silhouette_score(xs, ys_predicted)

    return rand_index, silhouette


def plot_rand_index(ys):
    plt.xlabel("clusters count")
    plt.ylabel("rand index")
    plt.plot(list(range(2, 11)), ys)
    plt.show()


def plot_silhouette(ys):
    plt.xlabel("clusters count")
    plt.ylabel("silhouette")
    plt.plot(list(range(2, 11)), ys)
    plt.show()


def plot_clusters(xs, ys):
    pca = PCA(n_components=2)
    xs_new_dim = pca.fit_transform(xs)

    for i in range(len(ys)):
        if ys[i] == 0:
            plt.plot(xs_new_dim[i][0], xs_new_dim[i][1], 'bo')
        elif ys[i] == 1:
            plt.plot(xs_new_dim[i][0], xs_new_dim[i][1], 'ro')
        elif ys[i] == 2:
            plt.plot(xs_new_dim[i][0], xs_new_dim[i][1], 'go')
    plt.show()


def read_data(path):
    dataset = pd.read_csv(path)
    xs = dataset.values[:, :-1]
    ys = np.array(dataset['class']) - 1

    normalize_minimax(xs, min_max(xs))

    return xs, ys


xs, ys = read_data(DATASET_PATH)
clusters = k_means(xs, 3)

rand_index_ys = []
silhouette_ys = []

for i in range(2, 11):
    rand, sil = calc_metrics(xs, ys, i)
    rand_index_ys.append(rand)
    silhouette_ys.append(sil)

plot_rand_index(rand_index_ys)
plot_silhouette(silhouette_ys)

plot_clusters(xs, clusters)

ys = pd.factorize(ys)[0]
plot_clusters(xs, ys)
