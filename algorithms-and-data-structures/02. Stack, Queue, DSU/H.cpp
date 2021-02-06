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
    int size, parent, add;
};

dsu a[N];
ll xp;

void make_set(int x) {
    a[x].size = 1;
    a[x].parent = x;
    a[x].add = 0;
}

int find_set(int x) {
    if (x == a[x].parent) {
        return x;
    }

    return find_set(a[x].parent);
}

void get_xp(int x) {
    xp += a[x].add;

    if (x == a[x].parent) {
        return;
    }

    get_xp(a[x].parent);
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
        a[x].add -= a[y].add;
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m;
    cin >> n >> m;

    for (int i = 1; i <= n; i++) {
        make_set(i);
    }

    string s;
    for (int i = 0; i < m; i++){
        int x, y;
        cin >> s;

        if (s == "join") {
            cin >> x >> y;
            union_sets(x, y);
        }

        if (s == "add") {
            cin >> x >> y;
            a[find_set(x)].add += y;
        }

        if (s == "get") {
            cin >> x;
            xp = 0;
            get_xp(x);
            cout << xp << endl;
        }
    }
    return 0;
}