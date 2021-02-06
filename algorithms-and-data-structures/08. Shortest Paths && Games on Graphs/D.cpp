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

ll a[M][M];
vector<bool> used;
vector<vector<ll>> g;

void dfs(ll v) {
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to]) {
            dfs(to);
        }
    }
}

struct edge {
    ll a, b, cost;

    edge(ll a, ll b, ll cost) : a(a), b(b), cost(cost) {}
};

vector<edge> edges;
vector<ll> d, d1;

int main() {
    ll n, m, s;
    cin >> n >> m >> s;
    s--;
    used.resize(n);
    g.resize(n);

    for (int i = 0; i < m; i++) {
        ll x, y, z;
        cin >> x >> y >> z;
        x--, y--;
        g[x].pb(y);

        edges.pb(edge(x, y, z));
    }


    d.resize(n, INF);

    d[s] = 0;

    set<ll> noPath;
    for (int i = 0; i < n; i++) {
        for (auto edge : edges) {
            if (d[edge.a] != INF) {
                if (d[edge.b] > d[edge.a] + edge.cost) {
                     d[edge.b] = d[edge.a] + edge.cost;
                     if (i == n - 1) {
                         noPath.insert(edge.b);
                     }
                }
            }
        }
    }

    for (auto v : noPath) {
        if (!used[v]) {
            dfs(v);
        }
    }

    for (int i = 0; i < n; i++) {
        if (used[i]) {
            cout << '-' << endl;
        } else if (d[i] == INF) {
            cout << '*' << endl;
        } else {
            cout << d[i] << endl;
        }
    }
}