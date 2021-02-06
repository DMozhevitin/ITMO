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

vector<vector<ll>> g;
vector<ll> matching;
vector<bool> used;

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

int main()
{
    size_t n, m;
    cin >> n >> m;

    g.resize(n);
    matching.resize(m, -1);

    ll to;
    for (size_t i = 0; i < n; i++)
    {
        cin >> to;
        while (to != 0)
        {
            g[i].pb(to - 1);
            cin >> to;
        }
    }

    for (size_t i = 0; i < n; i++)
    {
        used.clear();
        used.resize(n, false);
        dfs(i);
    }


    vector<pl> ans;
    for (size_t i = 0; i < m; i++)
    {
        if (matching[i] != -1)
        {
            ans.pb({matching[i] + 1, i + 1});
        }
    }

    cout << ans.size() << endl;

    for (const auto &it : ans)
    {
        cout << it.x << " " << it.y << endl;
    }

}