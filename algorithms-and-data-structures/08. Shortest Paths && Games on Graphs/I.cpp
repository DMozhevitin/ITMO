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

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1 * 1e7 + 10;
const ll M = 2e3 + 100;
const ll INF = 1e18 + 100;
const ll NO_EDGE = 100000;

#define x first
#define y second
#define pb push_back

vector<vector<int>> g, gr;
vector<bool> used;
vector<int> grandi;

int mex(int n, vector<int> v) {
    vector<bool> b(g[n].size() + 1);

    for (auto it : v) {
        if (grandi[it] > b.size()) continue;
        b[grandi[it]] = true;
    }

    for (int i = 0; i < b.size(); i++) {
        if (!b[i]) {
            return i;
        }
    }
}

void count_grandi(int v) {
    if (g[v].empty()) {
        grandi[v] = 0;
    }

    grandi[v] = mex(v, g[v]);
}

void dfs(int v) {
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to]) {
            dfs(to);
        }
    }

    count_grandi(v);
}

int main() {
    int n, m;
    cin >> n >> m;
    g.resize(n);
    gr.resize(n);
    used.resize(n);
    grandi.resize(n);

    for (int i = 0; i < m; i++) {
        int x, y;
        cin >> x >> y;

        x--, y--;

        g[x].pb(y);
    }

    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            dfs(i);
        }
    }

    for (auto it : grandi) {
        cout << it << endl;
    }
}