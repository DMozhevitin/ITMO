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
#include <cassert>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;
typedef unsigned long long ull;
typedef unsigned int uint;
typedef tuple<vector<ll>, vector<ll>, vector<ll>> tuple3v;

const ll N = 1e6 + 10;
const ll INF = 1e18 + 100;
const ll M = 3 * 1e3;

#define x first
#define y second
#define pb push_back

struct Edge
{
    ll c, from, to, num, revNum, f, cost;

    Edge() = default;

    Edge(const Edge &other) = default;

    Edge &operator=(const Edge &other) = default;

    Edge(ll c, ll f, ll to, ll num, ll cost) : c(c), f(f), to(to), num(num), cost(cost)
    {}
};

pair<pair<ll, bool>, vector<ll>>
dijkstra(const vector<vector<ll>> &g, const ll s, const ll &t, const vector<Edge> &edgesList)
{
    set<pair<ll, ll>> q;
    vector<ll> d(g.size(), INF);
    vector<pair<ll, ll>> parent(g.size());

    d[s] = 0;
    parent[s] = {-1, -1};
    q.insert({s, 0});

    while (!q.empty())
    {
        ll v = (*q.begin()).y;
        q.erase(q.begin());

        for (const ll it : g[v])
        {
            Edge e = edgesList.at(it);
            ll to = e.to;
            ll len = e.cost;

            if (d.at(v) + len < d.at(to) && e.f < e.c)
            {
                q.erase({d[to], to});
                d.at(to) = d.at(v) + len;
                q.insert({d.at(to), to});
                parent.at(to) = {v, it};
            }
        }
    }

    bool hasPath = (d.at(t) != INF);
    if (!hasPath)
    {
        return {{0, hasPath}, vector<ll>()};
    }

    vector<ll> edgesPath;
    pair<ll, ll> v = parent[t];
    while (v.first != -1)
    {
        edgesPath.push_back(v.second);
        v = parent.at(v.first);
    }
    reverse(edgesPath.begin(), edgesPath.end());
    ll minCapacity = INF;
    for (const ll it : edgesPath)
    {
        minCapacity = min(minCapacity, edgesList[it].c - edgesList[it].f);
    }
    return {{minCapacity, hasPath}, edgesPath};
}

void minCostFlow(vector<vector<ll>> &g, vector<Edge> &edgesList, const ll s, const ll t)
{
    while (true)
    {
        pair<pair<ll, bool>, vector<ll>> res = dijkstra(g, s, t, edgesList);
        ll minCapacity = res.first.first;
        bool hasPath = res.first.second;
        if (!hasPath)
        {
            break;
        }
        vector<ll> edgesInPath = res.second;

        for (const ll eInd : edgesInPath)
        {
            Edge e = edgesList.at(eInd);

            edgesList.at(eInd).f += minCapacity;
            edgesList.at(e.revNum).f -= minCapacity;
        }
    }
}

//miroslav - 0
//kamen' - 1, 4
//nozhnicy - 2, 5
//bumaga - 3, 6
//rostislav - 7
void createGraph(vector<vector<ll>> &g, vector<Edge> &edgesList, const vector<ll> &knb1, const vector<ll> &knb2)
{
    const vector<vector<ll>> edgeCosts{
            {0, 0, 1},
            {1, 0, 0},
            {0, 1, 0}
    };
    g.resize(8);


    Edge fromMiroslav;
    fromMiroslav.from = 0;
    fromMiroslav.f = 0;
    fromMiroslav.cost = 0;

    Edge toMiroslav;
    toMiroslav.to = 0;
    toMiroslav.f = 0;
    toMiroslav.c = 0;
    toMiroslav.cost = 0;

    for (int i = 0; i < 3; i++)
    {
        ll cnt = knb1.at(i);
        Edge e(fromMiroslav);
        e.num = edgesList.size();
        e.revNum = edgesList.size() + 1;
        e.to = i + 1;
        e.c = cnt;

        edgesList.push_back(e);
        g.at(0).push_back(e.num);

        Edge w(toMiroslav);
        w.revNum = e.num;
        w.num = e.revNum;
        w.from = e.to;

        edgesList.push_back(w);
        g.at(w.from).push_back(w.num);
    }


    Edge toRostislav;
    toRostislav.to = 7;
    toRostislav.f = 0;
    toRostislav.cost = 0;

    Edge fromRostislav;
    fromRostislav.from = 7;
    fromRostislav.f = 0;
    fromRostislav.c = 0;
    fromRostislav.cost = 0;

    for (int i = 0; i < 3; i++)
    {
        ll cnt = knb2.at(i);
        Edge e(toRostislav);
        e.from = i + 4;
        e.c = cnt;
        e.num = edgesList.size();
        e.revNum = edgesList.size() + 1;

        edgesList.push_back(e);
        g.at(e.from).push_back(e.num);

        Edge w(fromRostislav);
        w.to = e.from;
        w.num = e.revNum;
        w.revNum = e.num;

        edgesList.push_back(w);
        g.at(w.from).push_back(w.num);
    }

    for (int i = 1; i <= 3; i++)
    {
        for (int j = 4; j <= 6; j++)
        {
            Edge e;
            e.from = i;
            e.to = j;
            e.cost = edgeCosts.at(i - 1).at(j - 4);
            e.c = knb1.at(i - 1);
            e.f = 0;
            e.num = edgesList.size();
            e.revNum = edgesList.size() + 1;

            edgesList.push_back(e);
            g.at(e.from).push_back(e.num);

            Edge w;
            w.from = e.to;
            w.to = e.from;
            w.num = e.revNum;
            w.revNum = e.num;
            w.c = 0;
            w.cost = -e.cost;
            w.f = 0;

            edgesList.push_back(w);
            g.at(w.from).push_back(w.num);
        }
    }

}

int main()
{
    vector<ll> knb1(3), knb2(3);
    vector<vector<ll>> g;
    vector<Edge> edgesList;

    for (size_t i = 0; i < 3; i++)
    {
        cin >> knb1.at(i);
    }

    for (size_t i = 0; i < 3; i++)
    {
        cin >> knb2.at(i);
    }

    createGraph(g, edgesList, knb2, knb1);
    minCostFlow(g, edgesList, 0, 7);

    ll ans = 0;
    for (size_t i = 0; i < edgesList.size(); i += 2)
    {
        ans += (edgesList.at(i).f * edgesList.at(i).cost);
    }

    cout << ans << endl;
}