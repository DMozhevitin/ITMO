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

const ll N = 2e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

ll a[N];

void push(int, int, int);

struct Node {
    ll value;
    ll dSet = INF;
    ll dAdd = 0;
};

int n;
Node t[N];
ll b[N];

void build(int v, int left, int right) {
    if (left == right) {
        t[v].value = a[left];
        return;
    }

    int mid = (left + right) / 2;
    build(v * 2 + 1, left, mid);
    build(v * 2 + 2, mid + 1, right);
    t[v].value = min(t[v * 2 + 1].value, t[v * 2 + 2].value);
}

void update_on_segment(int qleft, int qright, ll x, int v, int left, int right) {
    push(v, left, right);

    if (left >= qleft && right <= qright) {
        t[v].dAdd = 0;
        t[v].dSet = x;
        push(v, left, right);
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

void push(int v, int left, int right) {
    if (left == right) {
        if (t[v].dAdd) {
            t[v].value += t[v].dAdd;
            t[v].dAdd = 0;
        } else if (t[v].dSet != INF) {
            t[v].value = t[v].dSet;
            t[v].dSet = INF;
        }

        return;
    }

    if (t[v].dAdd) {
        if (t[v * 2 + 1].dSet != INF) {
            t[v * 2 + 1].dSet += t[v].dAdd;
            t[v * 2 + 1].dAdd = 0;
        } else if (t[v * 2 + 1].dAdd) {
            t[v * 2 + 1].dAdd += t[v].dAdd;
            t[v * 2 + 1].dSet = INF;
        } else if(!t[v * 2 + 1].dAdd && t[v * 2 + 1].dSet == INF) {
            t[v * 2 + 1].dAdd = t[v].dAdd;
            t[v * 2 + 1].dSet = INF;
        }

        if (t[v * 2 + 2].dSet != INF) {
            t[v * 2 + 2].dSet += t[v].dAdd;
            t[v * 2 + 2].dAdd = 0;
        } else if (t[v * 2 + 2].dAdd) {
            t[v * 2 + 2].dAdd += t[v].dAdd;
            t[v * 2 + 2].dSet = INF;
        } else if(!t[v * 2 + 2].dAdd && t[v * 2 + 2].dSet == INF) {
            t[v * 2 + 2].dAdd = t[v].dAdd;
            t[v * 2 + 2].dSet = INF;
        }

        t[v].value += t[v].dAdd;
        t[v].dAdd = 0;
        t[v].dSet = INF;
        return;
    }

    if (t[v].dSet != INF) {
        t[v * 2 + 1].dSet = t[v].dSet;
        t[v * 2 + 2].dSet = t[v].dSet;
        t[v * 2 + 1].dAdd = 0;
        t[v * 2 + 2].dAdd = 0;

        t[v].value = t[v].dSet;
        t[v].dSet = INF;
        t[v].dAdd = 0;
        return;
    }
}

void add_on_segment(int qleft, int qright, ll x, int v, int left, int right) {
    push(v, left, right);
    if (left >= qleft && right <= qright) {
        t[v].dAdd += x;
        t[v].dSet = INF;
        push(v, left, right);
        return;
    }

    if (right < qleft || left > qright) {
        return;
    }

    int mid = (left + right) / 2;
    add_on_segment(qleft, qright, x, v * 2 + 1, left, mid);
    add_on_segment(qleft, qright, x, v * 2 + 2, mid + 1, right);
    t[v].value = min(t[v * 2 + 1].value, t[v * 2 + 2].value);
}

ll min(int v, int l, int r, int qleft, int qright) {
    push(v, l, r);

    if (l >= qleft && r <= qright) {
        return t[v].value;
    }

    if (r < qleft || l > qright) {
        return INF;
    }

    int mid = (l + r) / 2;
    push(v, l, r);
    ll s1 = min(v * 2 + 1, l, mid, qleft, qright);
    ll s2 = min(v * 2 + 2, mid + 1, r, qleft, qright);
    return min(s1, s2);
}

ll dumb_min(int left, int right) {
    ll mn = INF;
    for (int i = left; i <= right; i++) {
        mn = min(mn, b[i]);
    }

    return mn;
}

ll dumb_add(int left, int right, ll x) {
    for (int i = left; i <= right; i++) {
        b[i] += x;
    }
}

ll dumb_set(int left, int right, ll x) {
    for (int i = left; i <= right; i++) {
        b[i] = x;
    }
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> a[i];
        b[i] = a[i];
    }

    build(0, 0, n - 1);


    string query;

    while (cin >> query) {
        int i, j;
        cin >> i >> j;
        i--, j--;

        if (query == "min") {
            cout << min(0, 0, n - 1, i, j) << endl;
        }

        if (query == "set") {
            ll x;
            cin >> x;

            update_on_segment(i, j, x, 0, 0, n - 1);
        }

        if (query == "add") {
            ll x;
            cin >> x;

            add_on_segment(i, j, x, 0, 0, n - 1);
        }
    }

    return 0;
}