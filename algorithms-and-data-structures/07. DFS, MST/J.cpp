#include <iostream>
#include <math.h>
#include <algorithm>
#include <iomanip>
#include <vector>
#include <set>
#include <deque>
#include <stack>
#include <map>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;
typedef unsigned int uint;

const ll N = 1 * 1e7 + 10;
const ll M = 2e3 + 100;
const int INF = 1e9 + 100;

#define x first
#define y second
#define pb push_back

short color;
ll res = 0;

void dfs1(short v, vector<vector<short>> &g,
        vector<short> &comp, vector<short> &a, vector<bool> &used) {
    used[v] = true;

        for (auto to : g[v]) {
            if (!used[to]) {
                dfs1(to, g, comp, a, used);
            }
        }

    a.pb(v);
}

void dfs2(short v, vector<vector<short>> &g,
        vector<short> &comp) {
    comp[v] = color;


    for (auto to : g[v]) {
        if (comp[to] == -1) {
            dfs2(to, g, comp);
        }
    }
}

void condensation(vector<vector<short>> &g,
                        vector<vector<short>> &gr, short n, vector<short> &res) {
    vector<short> a(n);
    vector<bool> used(n);

    for (short i = 0; i < n; i++) {
        if (!used[i]) {
            dfs1(i, g, res, a, used);
        }
    }

    color = 0;
    for (short i = a.size() - 1; i >= 0; i--) {
        if (res[a[i]] == -1) {
            dfs2(a[i], gr, res);
            color++;
        }
    }
}

void dfs(short v, vector<vector<short>> &g, vector<bool> &used) {
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to]) {
            dfs(to, g, used);
        }
    }
}

bool checkReachability(short root, vector<vector<short>> &g, short n) {
    vector<bool> used(n, false);
    dfs(root, g, used);

    for (bool b : used) {
        if (!b) {
            return false;
        }
    }

    return true;
}

struct edge {
    edge(short from, short to, int w) : from(from), to(to), w(w) {}
    short from;
    short to;
    int w;
};

void findMST(vector<edge> &edges, short n, short root) {
    int minEdge[n];
    for (short i = 0; i < n; i++) {
        minEdge[i] = INF;
    }

    for (auto e: edges) {
        minEdge[e.to] = min(e.w, minEdge[e.to]);
    }

    for (short i = 0; i < n; i++) {
        if (i == root) continue;
        res += minEdge[i];
    }

    vector<vector<short>> g(n);
    vector<vector<short>> gr(n);
    for (auto e : edges) {
        if (e.w == minEdge[e.to]) {
            g[e.from].pb(e.to);
            gr[e.to].pb(e.from);
        }
    }

    if (checkReachability(root, g, n)) {
        return;
    }

    vector<edge> newEdges;
    vector<short> newComponents(n, -1);
    condensation(g, gr, n, newComponents);

    for (auto e : edges) {
        if (newComponents[e.from] != newComponents[e.to]) {
            newEdges.push_back(edge(newComponents[e.from], newComponents[e.to], e.w - minEdge[e.to]));
        }
    }

    findMST(newEdges, color, newComponents[root]);
}

void dfsMain(short v, vector<vector<short>> &g, vector<bool> &used) {
    used[v] = true;

    for (auto to : g[v]) {
        if (to == v) {
            continue;
        }

        if (!used[to]) {
            dfsMain(to, g, used);
        }
    }
}

bool checkReachability2(short root, vector<vector<short>> &g, short n) {
    vector<bool> used(n, false);
    dfsMain(root, g, used);

    for (bool b : used) {
        if (!b) {
            return false;
        }
    }

    return true;
}

int main() {
    short n;
    short m;
    cin >> n >> m;
    vector<edge> edges;
    vector<vector<short>> g(n);
    for (short i = 0; i < n; i++) {
        g[i] = vector<short>();
    }

    short x, y;
    int z;
    for (short i = 0; i < m; i++) {
        cin >> x >> y >> z;
        x--, y--;
        edges.push_back(edge(x, y, z));
        g[x].pb(y);
    }

    if (!checkReachability2(0, g, n)) {
        return cout << "NO", 0;
    }

    g.clear();
    cout << "YES" << endl;
    findMST(edges, n, 0);
    cout << res << endl;
}