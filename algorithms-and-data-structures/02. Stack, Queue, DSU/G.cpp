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

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

struct dsu {
    int max, min, size, parent;
};

dsu a[N];

void make_set(int x) {
    a[x].size = 1;
    a[x].max = a[x].min = a[x].parent = x;
}

int find_set(int x) {
    if (x == a[x].parent) {
        return x;
    }

    return a[x].parent = find_set(a[x].parent);
}

void union_sets(int x, int y) {
    x = find_set(x);
    y = find_set(y);

    if (a[x].size > a[y].size) {
        swap(x, y);
    }

    if (x != y) {
        a[x].parent = y;
        a[y].size += a[x].size;
        a[y].max = max(a[x].max, a[y].max);
        a[y].min = min(a[x].min, a[y].min);
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    for (int i = 1; i <= n; i++) {
        make_set(i);
    }

    string s;
    while (cin >> s) {
        int x, y;

        if (s == "union") {
            cin >> x >> y;
            union_sets(x, y);
        }

        if (s == "get") {
            cin >> x;
            x = find_set(x);
            cout << a[x].min << " " << a[x].max << " " << a[x].size << endl;
        }
    }
    return 0;
}