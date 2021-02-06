from random import randint

eps = 1e-9


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


n = int(input())
kernels = []
ys = []

for i in range(0, n):
    row = list(map(int, input().split()))
    kernels.append(row[:-1])
    ys.append(row[-1])

C = int(input())

a, b = SMO(C=C, max_it=100, kernels=kernels, ys=ys, tol=0.00000000001)

for i in range(0, len(a)):
    print(max(a[i], 0))

print(b)