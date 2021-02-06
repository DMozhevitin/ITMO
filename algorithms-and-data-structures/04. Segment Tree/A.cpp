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

const ll N = 1e7 + 10;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

int a[N], c[2 * N];
ll sum[N];

int main() {
    int n, x, y;
    cin >> n >> x >> y >> a[0];
    sum[0] = a[0];

    for (int i = 1; i < n; i++) {
        a[i] = ((x * a[i - 1]) % (1 << 16) + y) % (1 << 16);
        sum[i] = sum[i - 1] + a[i];
    }

    int m, z, t, b0;
    cin >> m >> z >> t >> b0;

    c[0] = b0 < 0 ? ((1 << 30) + b0) % n : (b0 % (1 << 30)) % n;

    for (int i = 1; i < 2 * m; i++) {
        b0 = (z * b0 % (1 << 30)) + t;
        c[i] = b0 < 0 ? ((1 << 30) + b0) % n : (b0 % (1 << 30)) % n;
    }

    ll ans = 0;
    for (int i = 0; i < m; i++) {
        int left = min(c[2 * i], c[2 * i + 1]);
        ll right = max(c[2 * i], c[2 * i + 1]);
        ans += sum[right] - sum[left] + a[left];
    }

    cout << ans << endl;
    return 0;
}