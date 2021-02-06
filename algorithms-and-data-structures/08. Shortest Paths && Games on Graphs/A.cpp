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

#define x first
#define y second
#define pb push_back

vector<vector<pl>> g;
vector<ll> d;
set<pl> q;

int main() {
    ll n, m;
    cin >> n >> m;
    g.resize(n);
    d.resize(n, INF);

    for (int i = 0; i < m; i++) {
        ll x, y, z;
        cin >> x >> y >> z;

        x--, y--;
        g[x].pb({y, z});
        g[y].pb({x, z});
    }

    d[0] = 0;
    q.insert({0, 0});

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

    for (auto it : d) {
        cout << it << " ";
    }
}