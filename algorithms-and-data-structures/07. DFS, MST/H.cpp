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

const ll N = 1 * 1e7 + 10;
const ll M = 2e3 + 100;
const int INF = 1e9 + 100;

#define x first
#define y second
#define pb push_back

int n;
vector<vector<pair<int, int>>> g, gr;
int clr = 0;
vector<int> used;

void dfs(int v, int p, int m, const vector<vector<pair<int, int>>>& g) {
    used[v] = clr;

    for (auto to : g[v]) {
        if (!used[to.x] && to.y <= m) {
            dfs(to.x, v, m, g);
        }
    }
}

void clear_used() {
    for (int i = 0; i < n; i++) used[i] = false;
}

bool check(int m) {
    clr = 0;
    clear_used();

    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            clr++;
            dfs(i, -1, m, g);
        }
    }

    int temp = clr;

    clr = 0;

    clear_used();

    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            clr++;
            dfs(i, -1, m, gr);
        }
    }

    return clr == 1 && temp == 1;
}

int main() {
    freopen("avia.in", "r", stdin);
    freopen("avia.out", "w", stdout);

    cin >> n;

    g.resize(n);
    gr.resize(n);
    used.resize(n);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            int x;
            scanf("%d", &x);
            g[i].pb({j, x});
            gr[j].pb({i, x});
        }
    }

    int left = -1, right = INF;
    while (right - left > 1) {
        int mid = (left + right) / 2;

        if (check(mid)) {
            right = mid;
        } else {
            left = mid;
        }
    }


    cout << right << endl;

   fclose(stdin);
   fclose(stdout);
}