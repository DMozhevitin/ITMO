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

const ll N = 1e6 + 100;
const ll M = 1e3 + 100;
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

ll a[M][M], dp[M][M];
char p[M][M];
stack<char> st;
ll n, m;

bool check(ll i, ll j) {
    return (i >= 0 && i < n && j >= 0 && j < m);
}

void get_parents(ll x, ll y) {
    while (check(x, y) && p[x][y] != 'F') {
        if (p[x][y] == 'R') {
            st.push('R');
            y--;
        }

        if (p[x][y] == 'D') {
            x--;
            st.push('D');
        }
    }


}

int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    cin >> n >> m;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            cin >> a[i][j];
            dp[i][j] = -INF;
        }
    }

    p[0][0] = 'F';
    dp[0][0] = a[0][0];

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (check(i - 1, j) && dp[i - 1][j] + a[i][j] > dp[i][j]) {
                dp[i][j] = dp[i - 1][j] + a[i][j];
                p[i][j] = 'D';
            }

            if (check(i, j - 1) && dp[i][j - 1] + a[i][j] > dp[i][j]) {
                dp[i][j] = dp[i][j - 1] + a[i][j];
                p[i][j] = 'R';
            }
        }
    }

    cout << dp[n - 1][m - 1] << endl;
    get_parents(n - 1, m - 1);
    while (!st.empty()) {
        cout << st.top();
        st.pop();
    }

    fclose(stdin);
    fclose(stdout);
    return 0;
}