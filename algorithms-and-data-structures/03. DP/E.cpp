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
const ll M = 1e3 + 100;
const ll INF = 1e18 + 100;
const ll P = 1e9;
const int dx[] = {-2, -1, 1, 2, 2, 1, -1, -2};
const int dy[] = {1, 2, 2, 1, -1, -2, -2, -1};

#define x first
#define  y second
#define pb push_back

ll dp[M][M];
int main() {
    string s, w;
    cin >> s >> w;

    if (s.empty() && !w.empty()) {
        return cout << w.size(), 0;
    }

    if (!s.empty() && w.empty()) {
        return cout << s.size(), 0;
    }

    ll n = s.size(), m = w.size();

    for (ll i = 0; i <= n; i++) {
        for (ll j = 0; j <= m; j++) {
            if (!i && !j) {
                dp[i][j] = 0;
            } else if (i > 0 && !j) {
                dp[i][j] = i;
            } else if (!i && j > 0) {
                dp[i][j] = j;
            } else if (i > 0 && j > 0 && s[i - 1] == w[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1];
            } else if (i > 0 && j > 0 && s[i - 1] != w[j - 1]) {
                dp[i][j] = min(min(dp[i][j - 1], dp[i - 1][j]), dp[i - 1][j - 1]) + 1;
            }
        }
    }

    cout << dp[n][m] << endl;
    return 0;
}