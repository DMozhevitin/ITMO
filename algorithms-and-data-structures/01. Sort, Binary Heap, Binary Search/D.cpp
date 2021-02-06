#include <iostream>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair <ll, ll> pl;
typedef pair <ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e9;

#define endl '\n'

int h[N];
int size = 0;

void Insert(int x) {
    h[size++] = x;
    int i = size - 1;

    while (x > h[(i - 1)/ 2] && i > 0) {
        swap(h[i], h[(i - 1) / 2]);
        i = (i - 1) / 2;
    }

}

int Extract() {
    swap(h[0], h[size - 1]);
    size--;

    int i = 0;

    for (;;) {
        int mx = -INF;
        if (i * 2 + 1 >= size) {
            break;
        }

        if (i * 2 + 2 < size) {
            mx = max(h[i * 2 + 1], h[i * 2 + 2]);
        } else {
            mx = h[i * 2 + 1];
        }

        if (h[i] >= mx) {
            break;
        }

        int ind = i * 2 + 1;
        if (ind + 1 < size && h[ind + 1] > h[ind]) {
            ind++;
        }

        swap(h[i], h[ind]);
        i = ind;
    }

    return h[size];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(false);
    cout.tie(false);


    int n;
    cin >> n;

    for (int i = 0; i < n; i++) {
        int x;
        cin >> x;

        if (!x) {
            int y;
            cin >> y;
            Insert(y);
        } else {
            cout << Extract() << endl;
        }
    }

    return 0;
}