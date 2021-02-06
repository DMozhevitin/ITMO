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

ll x, y, n;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(false);
    cout.tie(false);

    cin >> n >> x >> y;

    ll mn = min(x, y), mx = max(x, y), ans = mn;
    n--;

    ll left = 0, right = mx * n;

    while (right - left > 1) {
        ll mid = (left + right) / 2,
                time = mid / mn + mid / mx;

        if (time < n) {
            left = mid;
        } else {
            right = mid;
        }
    }

    cout << ans + right << endl;
    return 0;
}