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

int a[N];
int top, cnt;

void pop() {
    top--;
}

int getTop() {
    return a[top - 1];
}

void clean() {
    if (top >= 3 && a[top - 1] == a[top - 2] && a[top - 2] == a[top - 3]) {
        while (top > 1 && a[top - 1] == a[top - 2]) {
            pop();
            cnt++;
        }

        if (top > 0 ) {
            pop();
            cnt++;
        }
    }
}

void push(int val) {
    if (top > 0 && val != a[top - 1]) {
        clean();
    }

    a[top++] = val;
}

int main() {
    ios::sync_with_stdio(NULL);
    cin.tie(NULL);
    cout.tie(NULL);

    int n;
    cin >> n;

    for (int i = 0; i < n; i++) {
        int x;
        cin >> x;
        push(x);
    }

    clean();

    cout << n - top << endl;
    return 0;
}