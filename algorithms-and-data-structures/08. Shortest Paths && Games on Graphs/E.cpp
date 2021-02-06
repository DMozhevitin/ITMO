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
const ll INF = 1e17 + 100;

#define x first
#define y second
#define pb push_back

vector<vector<pl>> g;
map<ll, vector<ll>> ds;
bool used[N];
ll color[N];

ll clr;
ll n;

vector<ll> dijkstra(ll s) {
    vector<ll> d(n, INF);
    set<pl> q;
    d[s] = 0;
    q.insert({0, s});

    while (!q.empty()) {
        ll v = (*q.begin()).y;
        q.erase(q.begin());

        for (auto t : g[v]) {
            ll to = t.x;
            ll len = t.y;

            if (d[v] + len < d[to]) {
                q.erase({d[to], to});
                d[to] = d[v] + len;
                q.insert({d[to], to});
            }
        }
    }

    return d;
}

void dfs(ll v) {
    color[v] = clr;
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to.x]) {
            dfs(to.x);
        }
    }
}

ll tryCount(vector<ll> vs) {
    ll a = vs[0];
    ll b = vs[1];
    ll c = vs[2];

    ll res = INF;
    res = min(res, ds[a][b] + ds[b][c]);
    res = min(res, ds[a][b] + ds[b][a] + ds[a][c]);
    res = min(res, ds[a][c] + ds[c][a] +  ds[a][b]);
    res = min(res, ds[a][c] + ds[c][b]);

//    for (int i = 0; i < n; i++) {
//        res = min(res, ds[a][i] + 2 * ds[b][i] + ds[c][i]);
//        res = min(res, ds[a][i] + 2 * ds[c][i] + ds[b][i]);
//        res = min(res, ds[a][i] + ds[b][i] + ds[b][c]);
//        res = min(res, ds[a][i] + ds[c][i] + ds[c][b]);
//    }

    return res;
}

int main() {
    ll m;
    cin >> n >> m;
    g.resize(n);

    for (int i = 0; i < m; i++) {
        ll x, y, z;
        cin >> x >> y >> z;

        x--, y--;
        g[x].pb({y, z});
        g[y].pb({x, z});
    }


    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            clr++;
            dfs(i);
        }
    }

    ll a, b, c;
    cin >> a >> b >> c;
    a--, b--, c--;

    if (!((color[a] == color[b]) && color[b] == color[c])) {
        return cout << -1, 0;
    }

    ds[a] = dijkstra(a);
    ds[b] = dijkstra(b);
    ds[c] = dijkstra(c);

    vector<ll> vs = {a, b, c};

    sort(vs.begin(), vs.end());
    ll res = INF;
    res = tryCount(vs);
    while (next_permutation(vs.begin(), vs.end())) {
        ll tryCnt = tryCount(vs);
        res = min(res, tryCnt);
    }

    cout << (res == INF ? -1 : res) << endl;
}