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
const ll M = 6e3 + 100;
const ull prime = 31;

#define x first
#define y second
#define pb push_back

vector<vector<int>> gLeft, gRight;
vector<int> matchingLeft, matchigRight;
vector<bool> used;
vector<pair<int, int>> weightLeft, weightRight;
int edgesNum[M][M];

bool dfs(int v, const vector<vector<int>> &g, vector<int> &matching)
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
            bool res = dfs(matching[to], g, matching);
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
    size_t n, m, e;
    cin >> n >> m >> e;

    gLeft.resize(n);
    gRight.resize(m);

    weightLeft.resize(n);
    weightRight.resize(m);

    vector<int> wl(n), wr(m);

    matchingLeft.resize(m, -1);
    matchigRight.resize(n, -1);

    for (size_t i = 0; i < n; i++)
    {
        int x;
        cin >> x;

        weightLeft[i] = {x, i};
        wl[i] = x;
    }

    for (size_t j = 0; j < m; j++)
    {
        int x;
        cin >> x;
        weightRight[j] = {x, j};
        wr[j] = x;
    }

    for (size_t i = 0; i < e; i++)
    {
        int leftv, rightv;
        cin >> leftv >> rightv;
        leftv--, rightv--;

        edgesNum[leftv][rightv] = i;
        gLeft[leftv].pb(rightv);
        gRight[rightv].pb(leftv);
    }

    sort(weightLeft.rbegin(), weightLeft.rend());

    for (size_t i = 0; i < n; i++)
    {
        used.clear();
        used.resize(n, false);
        dfs(weightLeft[i].y, gLeft, matchingLeft);
    }

    set<int> leftInMatching;

    for (size_t i = 0; i < m; i++)
    {
        if (matchingLeft[i] != -1)
        {
            leftInMatching.insert(matchingLeft[i]);
        }
    }


    sort(weightRight.rbegin(), weightRight.rend());

    for (size_t i = 0; i < m; i++)
    {
        used.clear();
        used.resize(m, false);
        dfs(weightRight[i].y, gRight, matchigRight);
    }

    set<int> rightInMatching;

    for (size_t i = 0; i < n; i++)
    {
        if (matchigRight[i] != -1)
        {
            rightInMatching.insert(matchigRight[i]);
        }
    }


    vector<vector<int>> newg(n);

    for (size_t i = 0; i < n; i++)
    {
        for (size_t j = 0; j < gLeft[i].size(); j++)
        {
            if (leftInMatching.count(i) && rightInMatching.count(gLeft[i][j]))
            {
                newg[i].pb(gLeft[i][j]);
            }
        }
    }

    vector<int> matching(m, -1);
    vector<pair<int, int>> weight(n);

    for (size_t i = 0; i < n; i++)
    {
        if (leftInMatching.count(i))
        {
            weight[i] = {wl[i], i};
        }
    }

    sort(weight.rbegin(), weight.rend());

    for (size_t i = 0; i < n; i++)
    {
        used.clear();
        used.resize(n, false);
        dfs(weight[i].y, newg, matching);
    }


    ll maxWeight = 0;
    vector<int> maxMatchingEdgesNums;

    for (size_t i = 0; i < m; i++)
    {
        if (matching[i] != -1)
        {
            maxWeight += wl[matching[i]];
            maxWeight += wr[i];
            maxMatchingEdgesNums.pb(edgesNum[matching[i]][i]);
        }
    }


    cout << maxWeight << endl;
    cout << maxMatchingEdgesNums.size() << endl;

    for (const auto it : maxMatchingEdgesNums)
    {
        cout << it + 1 << " ";
    }
}