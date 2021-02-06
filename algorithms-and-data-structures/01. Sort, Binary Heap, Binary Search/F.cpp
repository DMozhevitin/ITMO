#include <iostream>
#include <math.h>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18;

#define endl '\n'

ll a[N];

int main() {
    ios::sync_with_stdio(false);
    cin.tie(false);
    cout.tie(false);


    ll n, m;
    cin >> n >> m;

    for (ll i = 0; i < n; i++) {
        cin >> a[i];
    }

    for (ll i = 0; i < m; i++) {
        ll x;
        cin >> x;

        ll left = 0, right = n;
        while (right - left > 1) {
            ll m = left + (right - left) / 2;
            if (a[m] < x) {
                left = m;
            } else {
                right = m;
            }
        }

        ll res = a[right];

        left = 0, right = n;

        while (right - left > 1) {
            ll m = left + (right - left) / 2;
            if (a[m] <= x) {
                left = m;
            } else {
                right = m;
            }
        }

        if (abs(x - res) < abs(x - a[left])) {
            cout << res << endl;
        } else {
            cout << a[left] << endl;
        }
    }
    return 0;
}