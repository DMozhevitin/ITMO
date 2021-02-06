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


vector<vector<ll>> g;
vector<ll> degs, sizes, gd;
vector<bool> used, bamboo;
vector<pair<ll, ll>> edges;
map<pl, ll> nedge;

void dfs1(ll v) {
    used[v] = true;

    for (ll to : g[v]) {
        if (!used[to]) {
            degs[v]++;
            dfs1(to);
        }
    }
}

ll dfs2(ll v) {
    used[v] = true;
    ll mx = degs[v];

    for (ll to : g[v]) {
        if (!used[to]) {
            mx = max(mx, dfs2(to));
        }
    }

    if (mx <= 1) {
        bamboo[v] = true;
    }

    return mx;
}

ll dfs3(ll v) {
    used[v] = true;
    ll sz = 1;

    for (ll to : g[v]) {
        if (!used[to]) {
            sz += dfs3(to);
        }
    }

    sizes[v] = sz;
    return sz;
}

ll dfs4(ll v) {
    used[v] = true;

    if (bamboo[v]) {
        gd[v] = sizes[v] - 1;
        return gd[v];
    }

    ll grandi = 0;
    for (auto to : g[v]) {
        if (!used[to]) {
            ll gSubtree = dfs4(to);
            grandi ^= (gSubtree + 1);
        }
    }

    gd[v] = grandi;
    return grandi;
}

ll countGrandiInNeighbors(ll v) {
    ll grandi = 0;
    for (auto to : g[v]) {
        if (!used[to]) {
            grandi ^= (gd[to] + 1);
        }
    }

    return grandi;
}

ll findToCut(ll v, ll need, ll grandies) {
    for (ll to : g[v]) {
        if (!used[to]) {
            if ((grandies ^ (gd[to] + 1)) == need) {
                return to;
            }
        }
    }

    return -1;
}

set<ll> findMaxNeighbors(ll v) {
    set<ll> res;
    ll mx = -1;
    for (auto to : g[v]) {
        if (!used[to]) {
            if (gd[to] > mx) {
                mx = gd[to];
            }
        }
    }

    for (auto to : g[v]) {
        if (!used[to]) {
            if (gd[to] == mx) {
                res.insert(to);
            }
        }
    }

    return res;
}

void findWinMove(ll v, ll need, ll parent) {
    used[v] = true;

    ll grandi = countGrandiInNeighbors(v);
    ll to = findToCut(v, need, grandi);

    if (to == -1) {
        for (auto neighbor : g[v]) {
            if (used[neighbor]) continue;
            ll newNeed = (grandi ^ (gd[neighbor] + 1) ^ need);
            if (newNeed == 0) {
                cout << nedge[{v, parent}];
                exit(0);
            }
            findWinMove(neighbor, newNeed - 1, v);
        }
    } else {
        cout << nedge[{v, to}];
        exit(0);
    }
}

int main() {
    ll n, r;
    cin >> n >> r;

    r--;
    degs.resize(n);
    used.resize(n);
    bamboo.resize(n);
    g.resize(n);
    sizes.resize(n);
    gd.resize(n);

    for (int i = 0; i < n - 1; i++) {
        ll x, y;
        cin >> x >> y;
        x--, y--;

        edges.pb({x, y});
        nedge[{x, y}] = i + 1;
        nedge[{y, x}] = i + 1;
        g[x].pb(y);
        g[y].pb(x);
    }

    dfs1(r);
    used.clear();
    used.resize(n);
    dfs2(r);
    used.clear();
    used.resize(n);
    dfs3(r);
    used.clear();
    used.resize(n);

    ll ans = (dfs4(r) == 0 ? 2 : 1);
    cout << ans << endl;

    if (ans == 1) {
        used.clear();
        used.resize(n);
        findWinMove(r, 0, -1);
    }
}