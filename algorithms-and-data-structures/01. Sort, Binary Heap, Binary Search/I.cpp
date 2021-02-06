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

ll w, h, n;

ld f(ld x) {
    return x * x + sqrt(x);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(false);
    cout.tie(false);

    ld y;
    cin >> y;

    ld left = 1.0, right = (ld)INF;

    for (int i = 0; i < N; i++) {
        ld mid = (left + right) / 2;

        if (f(mid) < y) {
            left = mid;
        } else {
            right = mid;
        }
    }

    cout << setprecision(6) << fixed;
    cout << right << endl;
    return 0;
}