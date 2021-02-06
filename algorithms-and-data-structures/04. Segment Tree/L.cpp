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

const ll N = 5e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 123;
using namespace std;

#define  x first
#define  y second

int n;
int a[N];
ll pref[N];
ll t[150][150][150];

int F(int i) {
    return i & (i + 1);
}

/*ll query_sum(int i) {
    i--;
    ll s = 0;
    while (i >= 0) {
        s += t[i];
        i = F(i) - 1;
    }
    return s;
}*/

ll query_sum_3D(int x, int y, int z) {
    ll s = 0;

    for (int i = x; i >= 0; i = F(i) - 1) {
        for (int j = y; j >= 0; j = F(j) - 1) {
            for (int k = z; k >= 0; k = F(k)  - 1) {
                s += t[i][j][k];
            }
        }
    }

    return s;
}

/*void update(int i, int x) {
    int j = i;
    while (j < n) {
        t[j] += x;
        j |= j + 1;
    }
}*/

void update_3D(int x, int y, int z, int d) {
    for (int i = x; i < n; i = i | (i + 1)) {
        for (int j = y; j < n; j = j | (j + 1)) {
            for (int k = z; k < n; k = k | (k + 1)) {
                t[i][j][k] += d;
            }
        }
    }
}

int main() {
    /*RUSSKIE VPERED*/

    cin >> n;

    int m = 500;
    while (m != 3) {
        cin >> m;

        if (m == 2) {
            int x1, y1, z1, x2, y2, z2;
            cin >> x1 >> y1 >> z1 >> x2 >> y2 >> z2;
            cout << query_sum_3D(x2, y2, z2) - query_sum_3D(x2, y1 - 1, z2)  - query_sum_3D(x1 - 1, y2, z2)
                    - query_sum_3D(x2, y2, z1 - 1) + query_sum_3D(x2, y1 - 1, z1 - 1) + query_sum_3D(x1 - 1, y2, z1 - 1)
                    + query_sum_3D(x1 - 1, y1 - 1, z2) - query_sum_3D(x1 - 1, y1 - 1, z1 - 1) << endl;
            continue;
        }

        if (m == 1) {
            int x, y, z, d;
            cin >> x >> y >> z >> d;
            update_3D(x, y, z, d);
            continue;
        }


    }

    /*for (int i = 0; i < n; i++) {
        cin >> a[i];
        pref[i] = !i ? a[i] : pref[i - 1] + a[i];
    }

    for (int i = 0; i < n; i++) {
        t[i] = pref[i] - pref[F(i)] + a[F(i)];
    }

    int m;
    cin >> m;

    for (int i = 0; i < m; i++) {
        string query;
        cin >> query;

        if (query == "add") {
            int j, x;
            cin >> j >> x;
            j--;
            update(j, x);
        }

        if (query == "sum") {
            int j;
            cin >> j;
            cout << query_sum(j) << endl;
        }
    }*/
    return 0;
}