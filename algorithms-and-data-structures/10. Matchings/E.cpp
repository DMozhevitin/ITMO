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
#include <cmath>

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
const size_t DAY = 1440;

#define x first
#define y second
#define pb push_back

vector<vector<int>> g;
vector<int> matching;
vector<bool> used;
size_t n;
double maxspeed;

struct vertex
{
    double time;
    pair<int, int> p;

    vertex() = default;

    vertex(const vertex &other) = default;

    vertex &operator=(const vertex &other) = default;

    vertex(int h, int m,
           pair<int, int> p)
    {
        this-> p = p;
        time = h * 60 + m;
    }


};

bool operator<(const vertex& a, const vertex& b)
{
    return a.time < b.time;
}

double dist(const pair<int, int>& a, const pair<int, int>& b)
{
    return hypot(a.x - b.x, a.y - b.y);
}

bool dfs(ll v)
{
    if (used[v])
    {
        return false;
    }

    used[v] = true;
    for (const auto to : g[v])
    {
        if (matching[to] == -1)
        {
            matching[to] = v;
            return true;
        } else
        {
            bool res = dfs(matching[to]);
            if (res)
            {
                matching[to] = v;
                return true;
            }
        }
    }

    return false;
}

vector<vertex> d;

void createGraph()
{
    for (size_t i = 0; i < n; i++)
    {
        for (size_t j = i + 1; j < n; j++)
        {
            auto v = d[i];
            auto u = d[j];

            if (v.time + dist(v.p, u.p) / maxspeed <= u.time)
            {
                g[i].pb(j);
            }
        }
    }
}

int main()
{
    freopen("ufo.in", "r", stdin);
    freopen("ufo.out", "w", stdout);
    cin >> n >> maxspeed;
    maxspeed /= 60;

    g.resize(n);
    d.resize(n);
    matching.resize(n, -1);

    for (size_t i = 0; i < n; i++)
    {
        int h, m;
        pair<int, int> p;

        scanf("%d:%d %d %d", &h, &m, &p.x, &p.y);
        vertex v(h, m, p);
        d[i] = vertex(v);
    }

    sort(d.begin(), d.end());
    createGraph();

    for (size_t i = 0; i < n; i++)
    {
        used.clear();
        used.resize(n, false);
        dfs(i);
    }


    vector<pair<int, int>> ans;
    unordered_set<int> left, right;
    map<int, int> leftToRight;


    for (size_t i = 0; i < n; i++)
    {
        if (matching[i] != -1)
        {
            left.insert(matching[i]);
            right.insert(i);
            ans.pb({matching[i], i});
            leftToRight[matching[i]] = i;
        }
    }

    used.clear();
    used.resize(n);

    int cnt = 0;

    for (const auto &it : ans)
    {
        auto jt = it;
        if (used[jt.x])
        {
            continue;
        }

        cnt++;
        while (!used[jt.x] && left.count(leftToRight[jt.x]))
        {
            jt = {leftToRight[jt.x], leftToRight[leftToRight[jt.x]]};
        }
    }

    cout << n - cnt << endl;


    fclose(stdin);
    fclose(stdout);
    return 0;
}