#include <iostream>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair <ll, ll> pl;
typedef pair <ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18;

int a[N];
ll ans;

void _merge(int left, int right, int mid) {
    int i = 0, j = 0;
    int res[right + 1];
    while (left + i < mid && mid + j < right) {
        if (a[left + i] < a[mid + j]) {
            res[i + j] = a[left + i];
            i++;
        } else {
            res[i + j] = a[mid + j];
            j++;
            ans += (mid - i - left);
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

    for (int cnt = 0; cnt < i + j; cnt++) {
        a[left + cnt] = res[cnt];
    }
}

void _sort(int left, int right) {
    if (right - left <= 1) {
        return;
    }

    int mid = (left + right) / 2;
    _sort(left, mid);
    _sort(mid, right);
    _merge(left, right, mid);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(false);
    cout.tie(false);

    int n;
    cin >> n;

    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    _sort(0, n);

    cout << ans << endl;
    return 0;
}