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
const ll P = 1e9;
const int dx[] = {-2, -1, 1, 2, 2, 1, -1, -2};
const int dy[] = {1, 2, 2, 1, -1, -2, -2, -1};

#define x first
#define  y second
#define pb push_back

ll a[M][M], dp[101][101][101];

bool check(int x, int y) {
    if (x == 3 && y != 1) {
        return false;
    }

    if (x == 3 && y == 1) {
        return true;
    }

    return (x >= 0 && x < 3 && y >= 0 && y < 3);
}

int main() {
    int n;
    cin >> n;

    for (int j = 0; j < 3; j++) {
        for (int i = 0; i < 3; i++) {
            a[j][i] = 3 * j + i + 1;
            dp[j][i][0] = 1;
        }
    }

    a[3][0] = -1;
    a[3][1] = 0;
    a[3][2] = -1;

    dp[3][0][0] = -1;
    dp[3][1][0] = 0;
    dp[3][2][0] = -1;
    dp[2][1][0] = 0;

    for (int k = 1; k < n; k++) {
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 3; j++) {
                if (!check(i, j)) {
                    continue;
                }

                for (int step = 0; step < 8; step++) {
                    int x = i + dx[step];
                    int y = j + dy[step];
                    if (!check(x, y)) {
                        continue;
                    }

                    dp[i][j][k] = (dp[i][j][k] + dp[x][y][k - 1]) % P;
                }
            }
        }
    }


    ll sum = 0;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            sum = (sum + dp[i][j][n - 1]) % P;
        }
    }

    sum = (sum + dp[3][1][n - 1]) % P;
    cout << sum << endl;
    return 0;
}