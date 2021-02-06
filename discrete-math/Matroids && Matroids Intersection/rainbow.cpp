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

size_t n, m;

struct Dsu
{
    explicit Dsu(size_t n)
    {
        p.resize(n);
        r.resize(n, 1);

        for (size_t i = 0; i < n; i++)
        {
            p[i] = i;
        }
    }

    int get(int v)
    {
        if (v == p[v])
            return v;
        return p[v] = get(p[v]);
    }

    void unionSets(int v, int u)
    {
        v = get(v);
        u = get(u);

        if (v != u)
        {
            if (r[v] > r[u])
            {
                swap(v, u);
            }

            p[v] = u;
            r[u] += r[v];
        }
    }

    bool check(int v, int u)
    {
        v = get(v);
        u = get(u);
        return u == v;
    }

    vector<int> p;
    vector<int> r;
};

struct edge
{
    int x, y, color;
};

vector<edge> edges;
set<int> S;

auto createExchangeGraph(set<int> I, vector<int> &leftPart, vector<int> &rightPart,
                         set<int> &x1, set<int> &x2)
{
    vector<vector<int>> exchangeGraph(S.size());

    for (const auto &it : I)
    {
        leftPart.push_back(it);
    }

    for (const auto &it : S)
    {
        if (I.find(it) == I.end())
        {
            rightPart.push_back(it);
        }
    }

    for (size_t i = 0; i < leftPart.size(); i++)
    {
        Dsu dsu(n);
        bool cycle = false;

        for (size_t j = 0; j < leftPart.size(); j++)
        {
            if (j == i) continue;
            auto edge = edges[leftPart[j]];

            if (dsu.check(edge.x, edge.y))
            {
                cycle = true;
                break;
            }

            dsu.unionSets(edge.x, edge.y);
        }

        if (cycle) continue;

        for (const auto v : rightPart)
        {
            auto edge = edges[v];
            if (!dsu.check(edge.x, edge.y))
            {
                exchangeGraph[leftPart[i]].pb(v);
            }
        }
    }

    Dsu dsu(n);
    bool cycle = false;
    for (const int it : leftPart)
    {
        auto edge = edges[it];
        if (dsu.check(edge.x, edge.y))
        {
            cycle = true;
        }
        dsu.unionSets(edge.x, edge.y);
    }

    if (!cycle)
    {
        for (const int it : rightPart)
        {
            auto edge = edges[it];
            if (!dsu.check(edge.x, edge.y))
            {
                x1.insert(it);
            }
        }
    }

    vector<int> colors(101);
    for (const auto it : I)
    {
        auto edge = edges[it];
        colors[edge.color]++;
    }

    for (size_t j = 0; j < rightPart.size(); j++)
    {
        bool removed = false;
        if (colors[edges[rightPart[j]].color] > 0)
        {
            removed = true;
            colors[edges[rightPart[j]].color]--;
        }

        for (size_t i = 0; i < leftPart.size(); i++)
        {
            auto edge = edges[leftPart[i]];
            if (!colors[edge.color])
            {
                exchangeGraph[rightPart[j]].push_back(leftPart[i]);
            }
        }

        if (removed)
        {
            colors[edges[rightPart[j]].color]++;
        }
    }

    for (size_t i = 0; i < rightPart.size(); i++)
    {
        auto edge = edges[rightPart[i]];

        if (!colors[edge.color])
        {
            x2.insert(rightPart[i]);
        }
    }

    return exchangeGraph;
}


set<int> findPath(vector<vector<int>> &exchangeGraph,
                  const vector<int> &leftPart, const vector<int> &rightPart,
                  set<int> &x1, set<int> &x2)
{
    //creating fake vertex
    int fake = exchangeGraph.size();
    exchangeGraph.emplace_back();

    vector<int> p(exchangeGraph.size(), -1);
    vector<bool> used(exchangeGraph.size(), false);

    for (const auto it : x1)
    {
        exchangeGraph.back().push_back(it);
    }

    queue<int> q;
    q.push(fake);

    bool x2Reached = false;
    int u = -1;

    while (!q.empty() && !x2Reached)
    {
        int v = q.front();
        q.pop();

        for (const auto to : exchangeGraph[v])
        {
            if (!used[to])
            {
                used[to] = true;
                q.push(to);
                p[to] = v;
            }

            if (x2.find(to) != x2.end())
            {
                x2Reached = true;
                u = to;
                break;
            }
        }
    }

    set<int> path;

    if (!x2Reached)
    {
        return path;
    }

    while (p[u] != -1)
    {
        path.insert(u);
        u = p[u];
    }
    path.insert(u);
    path.erase(fake);

    return path;
}

set<int> symmetricDifference(const set<int> &a, const set<int> &b)
{
    set<int> a1 = a, b1 = b;

    for (const auto &it : b)
    {
        a1.erase(it);
    }

    for (const auto &it : a)
    {
        b1.erase(it);
    }

    a1.insert(b1.begin(), b1.end());
    return a1;
}

int main()
{
    freopen("rainbow.in", "r", stdin);
    freopen("rainbow.out", "w", stdout);

    cin >> n >> m;
    edges.resize(m);

    for (size_t i = 0; i < m; i++)
    {
        S.insert(i);
    }

    for (int i = 0, x, y, z; i < m; i++)
    {
        cin >> x >> y >> z;
        x--, y--;
        edges[i] = {x, y, z};
    }

    set<int> J;
    while (true)
    {
        vector<int> leftPart, rightPart;
        set<int> x1, x2;
        auto exchangeGraph = createExchangeGraph(J, leftPart, rightPart, x1, x2);
        auto path = findPath(exchangeGraph, leftPart, rightPart, x1, x2);

        if (path.empty())
        {
            break;
        } else
        {
            J = symmetricDifference(J, path);
        }
    }

    cout << J.size() << endl;
    for (const auto &it : J)
    {
        cout << it + 1 << " ";
    }
}