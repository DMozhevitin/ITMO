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
#include <random>
 
using namespace std;
 
typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;
typedef unsigned long long ull;
typedef unsigned int uint;
 
const ll N = 1e6 + 10;
const int INF = 1e9 + 100;
const ll NO_EDGE = 100000;
const ll M = 1e3 + 100;
const ull prime = 31;
 
#define x first
#define y second
#define pb push_back
 
vector<pair<pl, pl>> edges;
vector<ll> p, r;
 
ll dsu_get(ll v) {
    if (v == p[v])
        return v;
    return p[v] = dsu_get(p[v]);
}
 
void dsu_union(ll v, ll u) {
    v = dsu_get(v);
    u = dsu_get(u);
 
    if (v != u) {
        if (r[v] > r[u]) {
            swap(v, u);
        }
 
        p[v] = u;
        r[u] += r[v];
    }
}
 
bool dsu_check(ll v, ll u) {
    v = dsu_get(v);
    u = dsu_get(u);
    return u == v;
}
 
 
int main()
{
    freopen("destroy.in", "r", stdin);
    freopen("destroy.out", "w", stdout);
 
    ll n, m, s;
    cin >> n >> m >> s;
    p.resize(n);
    r.resize(n);
 
    for (ll i = 0, x, y, z; i < m; i++)
    {
        cin >> x >> y >> z;
        x--, y--;
 
        edges.push_back({{-z, i}, {x, y}});
    }
 
    for (size_t i = 0; i < n; i++)
    {
        p[i] = i;
        r[i] = 1;
    }
 
    sort(edges.begin(), edges.end());
    set<int> maxBase;
 
 
    for (size_t i = 0; i < m; i++) {
        auto e = edges[i].y;
        if (!dsu_check(e.x, e.y)) {
            dsu_union(e.x, e.y);
            maxBase.insert(edges[i].x.y);
        }
    }
 
    for (auto &edge : edges)
    {
        edge.x.x = -edge.x.x;
    }
 
    sort(edges.begin(), edges.end());
 
    vector<ll> ans;
    ll weight = 0;
 
    for (size_t i = 0; i < edges.size(); i++)
    {
        if (!maxBase.count(edges[i].x.y) && weight + edges[i].x.x <= s)
        {
            ans.push_back(edges[i].x.y);
            weight += edges[i].x.x;
        }
    }
 
    cout << ans.size() << endl;
    for (const auto &it : ans)
    {
        cout << it + 1 << " ";
    }
 
    fclose(stdin);
    fclose(stdout);
}