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
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

ll a[N], dp[N], p[N];
stack<ll> st;

void get_parents(ll v) {
    while (p[v] != -1) {
        st.push(v);
        v = p[v];
    }
    st.push(0);
}

int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    p[0] = -1;

    ll n, k;
    cin >> n >> k;

    for (ll i = 0; i <= n; i++) {
        dp[i] = -INF;
    }

    dp[0] = 0;
    for (ll i = 1; i <= n - 2; i++) {
        cin >> a[i];
    }


    for (ll i = 1; i < n; i++) {
        for (ll j = 1; j <= k; j++) {
            ll ind = max(i - j, 0ll);
            //dp[i] = max(dp[i], dp[ind] + a[i]);
            if (dp[ind] + a[i] > dp[i]) {
                dp[i] = dp[ind] + a[i];
                p[i] = ind;
            }
        }
    }

    cout << dp[n - 1] << endl;
    get_parents(n - 1);
    cout << st.size() - 1 << endl;

    while (!st.empty()) {
        cout << st.top() + 1 << " ";
        st.pop();
    }

    fclose(stdin);
    fclose(stdout);
    return 0;
}