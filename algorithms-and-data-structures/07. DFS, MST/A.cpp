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

const ll N = 1 * 1e7 + 10;
const ll M = 2e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define y second
#define pb push_back

vector<vector<int>> g;
map<pair<int, int>, bool> is_bridge;
bool used[N];
int tin[N], up[N];
int t, bridges_cnt;
vector<pair<int, int>> edges;

void dfs(int v, int p) {
    used[v] = true;
    t++;
    tin[v] = up[v] = t;

    for (int to : g[v]) {
        if (to == p) {
            continue;
        }

        if (used[to]) {
            up[v] = min(tin[to], up[v]);
        } else {
            dfs(to, v);
            up[v] = min(up[v], up[to]);
            if (up[to] > tin[v]) {
                is_bridge[{to, v}] = true;
                is_bridge[{v, to}] = true;
                bridges_cnt++;
            }
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
        edges.emplace_back(x, y);
        g[x].pb(y);
        g[y].pb(x);
    }

    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            dfs(i, -1);
        }
    }

    cout << bridges_cnt << endl;
    for (int i = 0; i < edges.size(); i++) {
        auto e = edges[i];
        if (is_bridge[e]) {
            cout << i + 1 << endl;
        }
    }
}