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
#include <queue>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1 * 1e7 + 10;
const ll INF = 1e18 + 100;
const ll NO_EDGE = 100000;

#define x first
#define y second
#define pb push_back

set<pl> q;
vector<ll> d;
vector<vector<pl>> g, newg;
vector<vector<ll>> nums;
vector<bool> used;
ll M, n, t;
void dfs(ll v, ll ost) {
    if (used[nums[v][ost]]) {
        return;
    }

    used[nums[v][ost]] = true;

    for (auto it : g[v]) {
        ll to = it.x, len = it.y;
        ll newOst = (ost + len) % M;

        ll newv = nums[v][ost];
        ll newto = nums[to][newOst];

        newg[newv].pb({newto, len});

        if (to == n - 1 && newOst == t % M) {
            used[nums[n - 1][t % M]] = true;
            return;
        }

        if (!used[newto]) {
            dfs(to, newOst);
        }
    }
}

int main() {
    ll m;
    cin >> n >> m;

    M = INF;
    g.resize(n);
    for (ll i = 0, x, y, z; i < m; i++) {
        cin >> x >> y >> z;
        x--, y--;

        g[x].pb({y, z});
        g[y].pb({x, z});
    }

    cin >> t;

    if (g[n - 1].empty()) {
        return cout << "Impossible", 0;
    }

    M = 2 * g[n - 1][0].y;
    nums.resize(n, vector<ll>(M, -1));

    ll cnt = 0;
    for (ll i = 0; i < n; i++) {
        for (ll j = 0; j < M; j++) {
            nums[i][j] = cnt;
            cnt++;
        }
    }

    newg.resize(n * M);
    used.resize(newg.size());

    dfs(0, 0);
    ll s = nums[0][0];
    ll f = nums[n - 1][t % M];

    if (!used[f]) {
        return cout << "Impossible", 0;
    }

    d.resize(newg.size(), INF);
    q.insert({0, s});
    d[s] = 0;

    while (!q.empty()) {
        ll v = (*q.begin()).y;
        q.erase(q.begin());

        for (auto it : newg[v]) {
            ll to = it.x;
            ll len = it.y;

            if (d[v] + len < d[to]) {
                q.erase({d[to], to});
                d[to] = d[v] + len;
                q.insert({d[to], to});
            }
        }
    }


    if (d[f] > t) {
        return cout << "Impossible", 0;
    }

    cout << "Possible";
}

/*
5 5
1 2 1
2 3 1
3 4 1
4 5 1
1 5 1
2
 */