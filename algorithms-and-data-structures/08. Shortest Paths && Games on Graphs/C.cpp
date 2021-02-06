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

struct edge {
    ll a, b, cost;

    edge(ll a, ll b, ll cost) : a(a), b(b), cost(cost) {}
};

vector<edge> edges;
vector<ll> d, d1;

int main() {
    ll n, m, k, s;
    cin >> n >> m >> k >> s;
    s--;
    for (int i = 0; i < m; i++) {
        ll x, y, z;
        cin >> x >> y >> z;
        x--, y--;
        edges.pb(edge(x, y, z));
    }


    d.resize(n, INF);
    d1.resize(n, INF);

    d[s] = 0;

    for (int i = 0; i < k; i++) {
        vector<ll> d1(n, INF);
        for (auto edge : edges) {
            if (d[edge.a] != INF) {
                if (d1[edge.b] > d[edge.a] + edge.cost) {
                    d1[edge.b] = d[edge.a] + edge.cost;
                }
            }
        }
        d = d1;
    }

    for (auto it : d) {
        cout << (it == INF ? -1 : it)  << endl;
    }
}