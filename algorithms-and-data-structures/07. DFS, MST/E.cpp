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

int comp[N];
bool used[N];
vector<vector<int>> g, h;
vector<int> a;
int color = 0;
set<pair<int, int>> connected;

void dfs1(int v) {
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to]) {
            dfs1(to);
        }
    }

    a.pb(v);
}

void dfs2(int v) {
    comp[v] = color;

    for (auto to : h[v]) {
        if (comp[to] == 0) {
            dfs2(to);
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;

    g.resize(static_cast<unsigned int>(n));
    h.resize(static_cast<unsigned int>(n));
    for (int i = 0; i < m; i++) {
        int x, y;
        cin >> x >> y;
        x--, y--;

        g[x].pb(y);
        h[y].pb(x);
    }

    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            dfs1(i);
        }
    }

    color = 1;
    for (int i = a.size() - 1; i >= 0; i--) {
        if (comp[a[i]] == 0) {
            dfs2(a[i]);
            color++;
        }
    }
    
    int ans = 0;


    for (int i = 0; i < n; i++) {
        for (int j = 0; j < g[i].size(); j++) {
            if (comp[i] != comp[g[i][j]] && !connected.count({min(comp[i], comp[g[i][j]]),
                                                              max(comp[i], comp[g[i][j]])})) {
                connected.insert({min(comp[i], comp[g[i][j]]),
                                  max(comp[i], comp[g[i][j]])});
                ans++;
            }
        }
    }

    cout << ans << endl;
}