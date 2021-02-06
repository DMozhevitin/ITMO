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
 
vector<pl> weight;
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
    freopen("matching.in", "r", stdin);
    freopen("matching.out", "w", stdout);
 
    size_t n;
    cin >> n;
 
    weight.resize(n);
    g.resize(n);
    matching.resize(n, -1);
 
    for (size_t i = 0; i < n; i++)
    {
        ll x;
        cin >> x;
        weight[i] = {x, i};
    }
 
    for (size_t i = 0, deg; i < n; i++)
    {
        cin >> deg;
        g[i].resize(deg);
 
        for (size_t j = 0; j < deg; j++)
        {
            cin >> g[i][j];
            g[i][j]--;
        }
    }
 
    sort(weight.rbegin(), weight.rend());
 
    for (size_t i = 0; i < n; i++)
    {
        used.clear();
        used.resize(n, false);
        dfs(weight[i].y);
    }
 
    vector<ll> ans(n, -1);
    for (size_t i = 0; i < matching.size(); i++)
    {
        if (matching[i] != -1)
        {
            ans[matching[i]] = i;
        }
    }
 
    for (const auto &it : ans)
    {
        cout << it + 1 << " ";
    }
 
    cout << endl;
 
    fclose(stdin);
    fclose(stdout);
}