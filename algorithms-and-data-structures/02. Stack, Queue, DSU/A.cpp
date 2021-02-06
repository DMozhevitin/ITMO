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

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

pair<int, int> a[N];
int top;

void push(int val) {
    a[top].x = val;
    a[top].y = (!top ? val : min(val, a[top - 1].y));
    top++;
}

void pop() {
    top--;
}

int getMin() {
    return a[top - 1].y;
}

int main() {
    ios::sync_with_stdio(NULL);
    cin.tie(NULL);
    cout.tie(NULL);

    int n;
    cin >> n;

    for (int i = 0; i < n; i++) {
        int k;
        cin >> k;

        if (k == 1) {
            int x;
            cin >> x;
            push(x);
        } else if (k == 2) {
            pop();
        } else {
            cout << getMin() << endl;
        }
    }

    return 0;
}