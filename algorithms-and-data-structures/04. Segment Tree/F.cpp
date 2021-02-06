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

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 123;
using namespace std;

#define  x first
#define  y second

ll sparseTable[N][25];
ll a[N];
ll logs[N];

ll fl(ll x) {
    if (x == 1) {
        return 0;
    }

    return fl(x / 2) + 1;
}


ll query_min(ll left, ll right) {
    if (left == right) {
        return a[left];
    }

    ll d = right - left + 1;
    return min(sparseTable[left][logs[d]], sparseTable[right - (1 << logs[d]) + 1][logs[d]]);
}

int main() {

    ll n, m;
    cin >> n >> m >> a[0];

    for (ll i = 1; i < n; i++) {
        a[i] = (23 * a[i - 1] + 21563) % 16714589;
        logs[i] = fl(i);
    }


    for (ll i = 0; i < N; i++) {
        for (ll k = 0; k < 20; k++) {
            sparseTable[i][k] = INF;
        }
    }

    for (ll i = 0; i < n; i++) {
        sparseTable[i][0] = a[i];
    }

    ll pow = fl(n) + 2;
    for (ll k = 1; k < pow; k++) {
        for (ll i = 0; i < n; i++) {
            sparseTable[i][k] = min(sparseTable[i][k - 1], sparseTable[i + (1 << (k - 1))][k - 1]);
        }
    }

    ll u1, v1;
    cin >> u1 >> v1;

    ll ans = 0;

    ll u, v;
    for (ll i = 1; i <= m; i++) {
        if (v1 > u1) {
            ans = query_min(u1 - 1, v1 - 1);
        } else {
            ans = query_min(v1 - 1, u1 - 1);
        }


        if (i < m) {
            u = ((17 * u1 + 751 + ans + 2 * (i)) % n + 1);
            v = ((13 * v1 + 593 + ans + 5 * (i)) % n + 1);
            u1 = u, v1 = v;
        }


    }

    cout << u1 << " " << v1 << " " << ans << endl;
    return 0;
}