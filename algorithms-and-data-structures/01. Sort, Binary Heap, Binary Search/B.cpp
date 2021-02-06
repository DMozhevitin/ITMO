#include <iostream>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair <ll, ll> pl;
typedef pair <ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18;

ll a[N];

void _merge(ll left, ll right, ll mid) {
    ll i = 0, j = 0;
    ll res[right + 1];
    while (left + i < mid && mid + j < right) {
        if (a[left + i] < a[mid + j]) {
            res[i + j] = a[left + i];
            i++;
        } else {
            res[i + j] = a[mid + j];
            j++;
        }
    }

    while (left + i < mid) {
        res[i + j] = a[left + i];
        i++;
    }

    while (mid + j < right) {
        res[i + j] = a[mid + j];
        j++;
    }

    for (ll cnt = 0; cnt < i + j; cnt++) {
        a[left + cnt] = res[cnt];
    }
}

void _sort(ll left, ll right) {
    if (right - left <= 1) {
        return;
    }

    ll mid = (left + right) / 2;
    _sort(left, mid);
    _sort(mid, right);
    _merge(left, right, mid);
}

int main() {

    ll x;
    for (;cin >> x;) {
        a[x]++;
    }

    for (ll i = 0; i < 101; i++) {
        for (ll j = 0; j < a[i]; j++) {
            cout << i << " ";
        }
    }
    return 0;
}