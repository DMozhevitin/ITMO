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

const ll N = 5e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 123;
using namespace std;

#define  x first
#define  y second

struct Node {
    ll value;
    ll d = -INF;
};

int n;
Node t[N];

void push(int v) {
    if (t[v].d == -INF) {
        return;
    }

    if (t[v * 2 + 1].d < t[v].d) {
        t[v * 2 + 1].d = t[v].d;
    }

    if (t[v * 2 + 2].d < t[v].d) {
        t[v * 2 + 2].d = t[v].d;
    }

    if (t[v].d > t[v].value) {
        t[v].value = t[v].d;
    }
    t[v].d = -INF;
}

void update_on_segment(int qleft, int qright, ll x, int v, int left, int right) {
    push(v);

    if (left >= qleft && right <= qright) {
        if (x > t[v].value) {
            t[v].d = x;
        }
        push(v);
        return;
    }

    if (right < qleft || left > qright) {
        return;
    }

    int mid = (left + right) / 2;

    update_on_segment(qleft, qright, x, v * 2 + 1, left, mid);
    update_on_segment(qleft, qright, x, v * 2 + 2, mid + 1, right);

    t[v].value = min(t[v * 2 + 1].value, t[v * 2 + 2].value);
}

ll min(int v, int l, int r, int qleft, int qright) {
    push(v);

    if (l >= qleft && r <= qright) {
        return t[v].value;
    }

    if (r < qleft || l > qright) {
        return INF;
    }

    int mid = (l + r) / 2;
    push(v);
    ll s1 = min(v * 2 + 1, l, mid, qleft, qright);
    ll s2 = min(v * 2 + 2, mid + 1, r, qleft, qright);
    return min(s1, s2);
}

int find_min(int qleft, int qright, int value) {
    if (qleft == qright) {
        return qleft;
    }

    int mid = (qleft + qright) / 2;
    if (min(0, 0, n - 1, qleft, mid) == value) {
        find_min(qleft, mid, value);
    } else {
        find_min(mid + 1, qright, value);
    }
}

int main() {

    int m;
    cin >> n >> m;

    for (int i = 0; i < m; i++) {
        string query;
        int left, right;

        cin >> query;
        if (query == "defend") {
            ll x;
            cin >> left >> right >> x;
            update_on_segment(left - 1, right - 1, x, 0, 0, n - 1);
        }

        if (query == "attack") {
            cin >> left >> right;
            left--, right--;
            ll query_ans = min(0, 0, n - 1, left, right);
            cout << query_ans << " ";
            cout << find_min(left, right, query_ans) + 1  << endl;
        }
    }

    return 0;
}