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

const ll N = 2 * 1e5 + 10;
const ll M = 2e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define y second
#define pb push_back

vector<vector<pair<int, int>>> g;
bool is_bridge[N];
bool used[N];
int tin[N], up[N];
int t, bridges_cnt;
int color;
int clr[N];

void dfs(int v, int p) {
    used[v] = true;
    t++;
    tin[v] = up[v] = t;

    for (auto t : g[v]) {
        int to = t.x;
        if (to == p) {
            continue;
        }

        if (used[to]) {
            up[v] = min(tin[to], up[v]);
        } else {
            dfs(to, v);
            up[v] = min(up[v], up[to]);
            if (up[to] > tin[v]) {
                is_bridge[t.y] = true;
            }
        }
    }
}

void dfs2(int v) {
    used[v] = true;
    clr[v] = color;

    for (auto to : g[v]) {
        if (!used[to.x]) {
            dfs2(to.x);
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;
    g.resize(static_cast<unsigned int>(n));

    for (int i = 0, x, y; i < m; i++) {
        cin >> x >> y;
        x--, y--;
        g[x].pb({y, i});
        g[y].pb({x, i});
    }

    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            dfs(i, -1);
        }
    }

    for (int i = 0; i < n; i++) {
        used[i] = false;
    }

    vector<vector<pair<int, int>>> g1;
    g1.resize(n);

    for (int i = 0; i < n; i++) {
        for (auto j : g[i]) {
            if (!is_bridge[j.y]) {
                g1[i].pb({j.x, j.y});
                g1[j.x].pb({i, j.y});
            }
        }
    }

    g = g1;

    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            color++;
            dfs2(i);
        }
    }

    cout << color << endl;
    for (int i = 0; i < n; i++) {
        cout << clr[i] << " ";
    }
}