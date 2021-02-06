#include <iostream>
#include <math.h>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair <ll, ll> pl;
typedef pair <ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18;

#define endl '\n'

ll a[N];

int main() {
    ios::sync_with_stdio(false);
    cin.tie(false);
    cout.tie(false);

    ll n;
    cin >> n;

    for (ll i = 0; i < n; i++) {
        cin >> a[i];
    }

    sort(a, a + n);

    ll k;
    cin >> k;

    for (ll i = 0; i < k; i++) {
        ll left, right;
        cin >> left >> right;

        ll l = -1, r = n;

        while (r - l > 1) {
            ll m = l + (r - l) / 2;
            if (a[m] < left) {
                l = m;
            } else {
                r = m;
            }
        }

        ll res1 = r;

        l = -1, r = n;
        while (r - l > 1) {
            ll m = l + (r - l) / 2;
            if (a[m] <= right) {
                l = m;
            } else {
                r = m;
            }
        }

        ll res2 = l;

        cout << res2 - res1 + 1 << " " ;
    }

    return 0;
}