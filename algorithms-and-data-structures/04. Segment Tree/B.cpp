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

const ll N = 2e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

ll a[N], t[N];

void build(int v, int left, int right) {
    if (left == right) {
        t[v] = a[left];
        return;
    }

    int mid = (left + right) / 2;
    build(v * 2 + 1, left, mid);
    build(v * 2 + 2, mid + 1, right);
    t[v] = t[v * 2 + 1] + t[v * 2 + 2];
}

void update(int i, int x, int v, int left, int right) {
    if (left == right) {
        t[v] = x;
        return;
    }

    int mid = (left + right) / 2;

    if (i <= mid) {
        update(i, x, v * 2 + 1, left, mid);
    } else {
        update(i, x, v * 2 + 2, mid + 1, right);
    }

    t[v] = t[v * 2 + 1] + t[v * 2 + 2];
}

ll sum(int v, int l, int r, int qleft, int qright) {
    if (l >= qleft && r <= qright) {
        return t[v];
    }

    if (r < qleft || l > qright) {
        return 0;
    }

    int mid = (l + r) / 2;

    ll s1 = sum(v * 2 + 1, l, mid, qleft, qright);
    ll s2 = sum(v * 2 + 2, mid + 1, r, qleft, qright);
    return s1 + s2;
}

int main() {
    int n;
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    build(0, 0, n - 1);

    string query;
    while (cin >> query) {
        if (query == "sum") {
            int left, right;
            cin >> left >> right;
            cout << sum(0, 0, n - 1, left - 1, right - 1) << endl;
        }

        if (query == "set") {
            int i, x;
            cin >> i >> x;
            update(i - 1, x, 0, 0, n - 1);
        }
    }
    return 0;
}