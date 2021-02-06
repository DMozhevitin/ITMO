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

int p[N], r[N];
vector<vector<pair<int, int>>> g;
vector<pair<int, pair<int, int>>> edges;

int dsu_get(int v) {
    if (v == p[v])
        return v;
    return p[v] = dsu_get(p[v]);
}

void dsu_union(int v, int u) {
    v = dsu_get(v);
    u = dsu_get(u);

    if (v != u) {
        if (r[v] > r[u]) {
            swap(v, u);
        }

        p[v] = u;
        r[u] += r[v];
    }
}

bool dsu_check(int v, int u) {
    v = dsu_get(v);
    u = dsu_get(u);
    return u == v;
}

int main() {
    int n, m;
    cin >> n >> m;

    g.resize(n);

    for (int i = 0; i < m; i++) {
        int x, y, z;
        cin >> x >> y >> z;
        x--, y--;

        g[x].pb({y, z});
        g[y].pb({x, z});
        edges.pb({z, {x, y}});
    }

    for (int i = 0; i < n; i++) {
        p[i] = i;
        r[i] = 1;
    }

    sort(edges.begin(), edges.end());
    ll weight = 0;

    for (int i = 0; i < m; i++) {
        auto e = edges[i].y;
        if (!dsu_check(e.x, e.y)) {
            dsu_union(e.x, e.y);
            weight += edges[i].x;
        }
    }

    cout << weight << endl;
}