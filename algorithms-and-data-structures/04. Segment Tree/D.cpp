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
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

int maxRight;

struct Node {
    int value = 0;
    int d = -1;
    int ans = 0;
    int rightBit = 0;
    int leftBit = 0;
};


struct Query {
    int color;
    int left;
    int right;
};

int n;
Node t[N];
Query q[N];

void push(int v) {
    if (t[v].d == -1) {
        return;
    }

    t[v * 2 + 1].d = t[v * 2 + 2].d = t[v].d;
    t[v].d = -1;
}

int trueValue(int v, int left, int right) {
    if (t[v].d == 0) {
        return 0;
    } else if (t[v].d == 1) {
        return right - left + 1;
    } else
        return t[v].value;
}

int trueLeftBit(int v) {
    if (t[v].d != -1) {
        return t[v].d;
    }

    return t[v].leftBit;
}

int trueAns(int v) {
    if (t[v].d != -1) {
        return t[v].d;
    }

    return t[v].ans;
}

int trueRightBit(int v) {
    if (t[v].d != -1) {
        return t[v].d;
    }

    return t[v].rightBit;
}

void update_on_segment(int qleft, int qright, int x, int v, int left, int right) {
    if (left >= qleft && right <= qright) {
        t[v].d = x;
        return;
    }

    if (right < qleft || left > qright) {
        return;
    }

    int mid = (left + right) / 2;
    push(v);
    update_on_segment(qleft, qright, x, v * 2 + 1, left, mid);
    update_on_segment(qleft, qright, x, v * 2 + 2, mid + 1, right);

    t[v].value = trueValue(v * 2 + 1, left, mid) + trueValue(v * 2 + 2, mid + 1, right);
    t[v].leftBit = trueLeftBit(v * 2 + 1);
    t[v].rightBit = trueRightBit(v * 2 + 2);

    if (trueRightBit(v * 2 + 1) == trueLeftBit(v * 2 + 2) && trueRightBit(v * 2 + 1) == 1) {
        t[v].ans = trueAns(v * 2 + 1) + trueAns(v * 2 + 2) - 1;
    } else {
        t[v].ans = trueAns(v * 2 + 1) + trueAns(v * 2 + 2);
    }
}

int findPower(int x) {
    int p = 1;
    while (p < x) {
        p *= 2;
    }
    return p * 2;
}

int main() {
    cin >> n;

    maxRight = -1;
    int minNegative = 0;

    for (int i = 0; i < n; i++) {
        char color;
        int left, len;
        cin >> color >> left >> len;

        q[i].color = color == 'W' ? 0 : 1;
        q[i].left = left;
        q[i].right = left + len - 1;

        minNegative = min(minNegative, q[i].left);
        maxRight = max(maxRight, q[i].right);
    }

    if (minNegative < 0) {
        for (int i = 0; i < n; i++) {
            q[i].left -= minNegative;
            q[i].right -= minNegative;

            maxRight = max(maxRight, q[i].right);
        }
    }

    int treeSize = findPower(maxRight);

    for (
            int i = 0;
            i < n;
            i++) {
        update_on_segment(q[i]
                                  .left, q[i].right, q[i].color, 0, 0, treeSize - 1);
        cout << t[0].ans << " " << t[0].value <<
             endl;
    }

    return 0;
}