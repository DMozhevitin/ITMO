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
vector<ll> d, p;

int main() {
    ll n;
    cin >> n;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            ll x;
            cin >> x;

            if (x != NO_EDGE) {
                edges.pb(edge(i, j, x));
            }
        }
    }

    ll m = edges.size();
    d.resize(n, INF);
    p.resize(n, -1);
    ll lastChanged;

    for (int i = 0; i < n; i++) {
        lastChanged = -1;
        for (auto edge : edges) {
            if (d[edge.b] > d[edge.a] + edge.cost) {
                d[edge.b] = max(-INF, d[edge.a] + edge.cost);
                p[edge.b] = edge.a;
                lastChanged = edge.b;
            }
        }
    }


    if (lastChanged != -1) {
        ll v = lastChanged;
        while (n--) v = p[v];
        vector<ll> ans;

        ll u = v;
        while (true) {
            if (ans.size() > 1 && u == v) break;
            ans.pb(u);
            u = p[u];
        }

        cout << "YES" << endl;
        cout << ans.size() << endl;
        for (auto it = ans.rbegin(); it != ans.rend(); it++) {
            cout << *it + 1 << " ";
        }
    } else {
        return cout << "NO", 0;
    }

}