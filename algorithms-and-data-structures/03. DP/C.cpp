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

#define x first
#define  y second
#define pb push_back

ll a[N], dp[N], p[N];
stack<ll> st;

void get_parents(ll ind) {
    while (ind != -1) {
        st.push(a[ind]
        );
        ind = p[ind];
    }
}

int main() {
    ll n;
    cin >> n;

    for (ll i = 0; i < n; i++) {
        cin >> a[i];
        dp[i] = 1;
        p[i] = -1;
    }


    for (ll i = 1; i < n; i++) {
        for (ll j = 0; j < i; j++) {
            if (a[j] < a[i] && dp[j] + 1 > dp[i]) {
                dp[i] = dp[j] + 1;
                p[i] = j;
            }
        }
    }

    ll mx = -INF, ind = -1;
    for (ll i = 0; i < n; i++) {
        if (dp[i] > mx) {
            mx = dp[i];
            ind = i;
        }
    }

    cout << mx << endl;
    get_parents(ind);
    while (!st.empty()) {
        cout << st.top() << " ";
        st.pop();
    }
    return 0;
}