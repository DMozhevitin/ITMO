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
#include <unordered_set>
#include <queue>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1 * 1e7 + 10;
const ll INF = 1e18 + 100;
const ll NO_EDGE = 100000;
const ll M = 5e3 + 100;

#define x first
#define y second
#define pb push_back

ll a[M][M];
deque<int> q;

int main() {
    freopen("fullham.in", "r", stdin);
    freopen("fullham.out", "w", stdout);

    int n;
    cin >> n;
    string s;
    getline(cin, s);

    for (int i = 0; i < n; i++) {
        q.pb(i);
        string s;
        getline(cin, s);
        a[i][i] = 0;
        for (int j = 0; j < s.size(); j++) {
            int x = s[j] - '0';
            a[i][j] = a[j][i] = x;
        }
    }

    for (int cnt = 0; cnt < n * (n - 1); cnt++) {
        if (!a[q[0]][q[1]]) {
            int j = 2;
            while (!a[q[0]][q[j]] || !a[q[1]][q[j + 1]]) {
                j++;
            }
            reverse(q.begin() + 1, q.begin() + j + 1);
        }
        q.pb(q[0]);
        q.pop_front();
    }

    for (const auto &it : q) {
        cout << it + 1 << " ";
    }

    fclose(stdin);
    fclose(stdout);
}