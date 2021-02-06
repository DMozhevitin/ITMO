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

const ll N = 4e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 123;
using namespace std;

#define  x first
#define  y second

struct Node {
    ll value = 0;
    ll d = INF;
};

struct Query {
    ll left;
    ll right;
    ll value;
};

Query q[N];
Node t[N];

void push(int v) {
    if (t[v].d == INF) {
        return;
    }

    t[v * 2 + 1].d = t[v * 2 + 2].d = t[v].d;
    t[v].value = t[v].d;
    t[v].d = INF;
}


void update_on_segment(int qleft, int qright, ll x, int v, int left, int right) {
    push(v);

    if (left >= qleft && right <= qright) {
        t[v].d = x;
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

bool cmp(const Query &a, const Query &b) {
    if (a.value == b.value && a.left == b.left && a.right < b.right) {
        return true;
    }

    if (a.value == b.value && a.left < b.left) {
        return true;
    }

    return a.value < b.value;
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

int main() {

    freopen("rmq.in", "r", stdin);
    freopen("rmq.out", "w", stdout);


    ll n, m;
    cin >> n >> m;

    for (int i = 0; i < m; i++) {
        ll left, right, value;
        cin >> left >> right >> value;
        left--, right--;

        q[i].value = value;
        q[i].left = left;
        q[i].right = right;
    }

    sort(q, q + m, cmp);

    for (int i = 0; i < m; i++) {
        update_on_segment(q[i].left, q[i].right, q[i].value, 0, 0, n - 1);
    }

    for (int i = 0; i < m; i++) {
        if (q[i].value != min(0, 0, n - 1, q[i].left, q[i].right)) {
            return cout << "inconsistent", 0;
        }
    }


    cout << "consistent" << endl;
    for (int i = 0; i < n; i++) {
        cout << min(0, 0, n - 1, i, i) << " ";
    }

    fclose(stdin);
    fclose(stdout);
    return 0;
}