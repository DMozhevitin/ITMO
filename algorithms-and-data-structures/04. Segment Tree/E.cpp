#define _FORTIFY_SOURCE 0
#pragma GCC optimize("Ofast")
#pragma GCC optimize("no-stack-protector")
#pragma GCC optimize("unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,popcnt,abm,mmx,tune=native")
#pragma GCC optimize("fast-math")

#include <iostream>
#include <math.h>
#include <algorithm>
#include <iomanip>
#include <vector>
#include <set>
#include <map>
#include <deque>
#include <stack>
#include <unordered_map>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 4e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;
using namespace std;

int r, n, m;
struct Matrix {
    int a11;
    int a12;
    int a21;
    int a22;
};

const Matrix E = {1, 0, 0, 1};
Matrix a[N], t[N];

Matrix matrixMultiply(const Matrix &a,  const Matrix &b) {
    Matrix c;
    c.a11 = ((a.a11 * b.a11) % r + (a.a12 * b.a21) % r) % r;
    c.a12 = ((a.a11 * b.a12) % r + (a.a12 * b.a22) % r) % r;
    c.a21 = ((a.a21 * b.a11) % r + (a.a22 * b.a21) % r) % r;
    c.a22 = ((a.a21 * b.a12) % r + (a.a22 * b.a22) % r) % r;

    return c;
}

void build(int v, int left, int right) {
    if (left == right) {
        t[v] = a[left];
        return;
    }

    int mid = (left + right) / 2;
    build(v * 2 + 1, left, mid);
    build(v * 2 + 2, mid + 1, right);
    t[v] = matrixMultiply(t[v * 2 + 1], t[v * 2 + 2]);
}

Matrix query(int qleft, int qright, int v, int left, int right) {
    if (qright < left || qleft > right) {
        return E;
    }

    if (left >= qleft && right <= qright) {
        return t[v];
    }

    int mid = (left + right) / 2;
    Matrix left_res = query(qleft, qright, v * 2 + 1, left, mid);
    Matrix right_res = query(qleft, qright, v * 2 + 2, mid + 1, right);
    return matrixMultiply(left_res, right_res);
}

void printMatrix(const Matrix &m) {
    printf("%d %d \n%d %d \n \n", m.a11, m.a12, m.a21, m.a22);
}

int main() {
    freopen("crypto.in", "r", stdin);
    freopen("crypto.out", "w", stdout);

    scanf("%d%d%d", &r, &n, &m);

    for (int i = 0; i < n; i++) {
        scanf("%d%d%d%d", &a[i].a11, &a[i].a12, &a[i].a21, &a[i].a22);
    }

    build(0, 0, n - 1);

    for (int i = 0; i < m; i++) {
        int left, right;
        scanf("%d%d", &left, &right);
        left--, right--;

        printMatrix(query(left, right, 0, 0, n - 1));
    }

    fclose(stdin);
    fclose(stdout);
    return 0;
}