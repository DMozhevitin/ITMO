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

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define  y second

ll v[N], w[N];
ll n, k;
ld q[N];
vector <pair<ld, ll>> ans;

bool check(ld x) {
    for (int i = 0; i < n; i++) {
        q[i] = v[i] - x * w[i];
    }

    sort(q, q + n);

    ld sum = 0;
    for (int i = n - k; i < n; i++) {
        sum += q[i];
    }

    return (sum >= 0);
}

int main() {
    freopen("kbest.in", "r", stdin);
    freopen("kbest.out", "w", stdout);

    cin >> n >> k;

    for (int i = 0; i < n; i++) {
        cin >> v[i] >> w[i];
    }

    ld left = 0, right = INF;

    for (int i = 0; i < 100; i++) {
        ld mid = (left + right) / 2;
        if (check(mid)) {
            left = mid;
        } else {
            right = mid;
        }
    }

    ans.resize(n);
    for (int i = 0; i < n; i++) {
        ans[i] = {v[i] - left * w[i], i};
    }

    sort(ans.begin(), ans.end());

    vector <ll> ans2;

    for (ll i = n - k; i < n; i++) {
        ans2.push_back(ans[i].y + 1);
    }

    sort(ans2.begin(), ans2.end());

    for (int i = 0; i < ans2.size(); i++) {
        cout << ans2[i] << endl;
    }

    fclose(stdin);
    fclose(stdout);

    return 0;
}