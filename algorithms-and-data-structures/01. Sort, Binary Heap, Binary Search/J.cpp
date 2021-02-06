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

using namespace std;


typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;

#define endl '\n'

ld v1, v2, a;
ld dist(ld x1, ld y1, ld x2, ld y2) {
    return sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
}

ld f(ld x) {
    ld d1 = dist(0.0, 1.0, x, a), d2 = dist(x, a, 1.0, 0.0);
    return d1 / v1 + d2 / v2;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(false);
    cout.tie(false);

    cin >> v1 >> v2 >> a;
    ld left = 0, right = 1;

    for (int i = 0; i < N; i++) {
        ld m1 = (2 * left + right) / 3;
        ld m2 = (2 * right + left) / 3;

        if (f(m1) < f(m2)) {
            right = m2;
        } else {
            left = m1;
        }
    }

    cout << setprecision(6) << fixed;
    cout << right << endl;
    return 0;
}