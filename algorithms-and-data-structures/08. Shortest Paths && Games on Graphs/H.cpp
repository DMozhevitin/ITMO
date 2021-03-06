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
#include <unordered_set>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1 * 1e7 + 10;
const ll M = 2e3 + 100;
const ll INF = 1e18 + 100;
const ll NO_EDGE = 100000;

#define x first
#define y second
#define pb push_back

vector<vector<int>> g, gr;
vector<bool> used, win, lose;
vector<int> topsort, deg;

void dfs_topsort(int v) {
    used[v] = true;

    for (int to : g[v]) {
        if (!used[to]) {
            dfs_topsort(to);
        }
    }

    topsort.pb(v);
}

void top_sort() {
    for (int i = 0; i < g.size(); i++) {
        if (!used[i]) {
            dfs_topsort(i);
        }
    }

    reverse(topsort.begin(), topsort.end());
}

void dfs_color(int v) {
    used[v] = true;

    for (auto to : gr[v]) {
        if (!used[to]) {
            if (lose[v]) {
                win[to] = true;
            } else if (--deg[to] == 0) {
                lose[to] = true;
            } else {
                continue;
            }
            dfs_color(to);
        }
    }
}

int main() {
    int n;
    while (cin >> n) {
        int m;
        cin >> m;
        g.clear();
        gr.clear();
        used.clear();
        win.clear();
        lose.clear();
        deg.clear();

        g.resize(n);
        gr.resize(n);
        used.resize(n);
        win.resize(n);
        lose.resize(n);
        deg.resize(n);

        for (int i = 0; i < m; i++) {
            int x, y;
            cin >> x >> y;

            x--, y--;

            deg[x]++;
            g[x].pb(y);
            gr[y].pb(x);
        }


        used.clear();
        used.resize(n);

        for (int v = 0; v < n; v++) {
            if (g[v].size() == 0) {
                lose[v] = true;
                dfs_color(v);
            }
        }

        for (int i = 0; i < n; i++) {
            if (!lose[i] && !win[i]) {
                cout << "DRAW" << endl;
            } else if (lose[i]) {
                cout << "SECOND" << endl;
            } else {
                cout << "FIRST" << endl;
            }
        }
        cout << endl;
    }
}