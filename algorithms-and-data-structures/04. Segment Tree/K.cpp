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
const int INF = 1e9 + 123;
using namespace std;

#define  x first
#define  y second

int n;
int t[N];

void update(int index, int x, int v, int left, int right) {
    if (left == right) {
        t[v] = x;
        return;
    }

    int mid = (left + right) / 2;
    if (index <= mid) {
        update(index, x, v * 2 + 1, left, mid);
    } else {
        update(index, x, v * 2 + 2, mid + 1, right);
    }

    t[v] = min(t[v * 2 + 1], t[v * 2 + 2]);
}

int query_min(int qleft, int qright, int v, int left, int right) {
    if (right < qleft || left > qright) {
        return INF;
    }

    if (left >= qleft && right <= qright) {
        return t[v];
    }

    int mid = (left + right) / 2;
    int ans_left = query_min(qleft, qright, v * 2 + 1, left, mid);
    int ans_right = query_min(qleft, qright, v * 2 + 2, mid + 1, right);
    return min(ans_left, ans_right);
}

int findPlaceLeft(int qleft, int qright) {
    if (qleft == qright) {
        return qleft;
    }

    int mid = (qleft + qright) / 2;
    if (query_min(qleft, qright, 0, 0, n - 1) != 0) {
        return INF;
    } else {
        if (!query_min(qleft, mid, 0, 0, n - 1)) {
            findPlaceLeft(qleft, mid);
        } else {
            findPlaceLeft(mid + 1, qright);
        }
    }

}

int main() {

    freopen("parking.in", "r", stdin);
    freopen("parking.out", "w", stdout);

    int m;
    cin >> n >> m;

    for (int i = 0; i < m; i++) {
        string query;
        cin >> query;

        int place;
        cin >> place;
        place--;

        if (query == "enter") {
            int truePlace = findPlaceLeft(place, n - 1);
            if (truePlace == INF) {
                truePlace = findPlaceLeft(0, place);
            }
            cout << truePlace + 1 << endl;
            update(truePlace, 1, 0, 0, n - 1);

        } else {
            update(place, 0, 0, 0, n - 1);
        }
    }
    fclose(stdin);
    fclose(stdout);
    return 0;
}