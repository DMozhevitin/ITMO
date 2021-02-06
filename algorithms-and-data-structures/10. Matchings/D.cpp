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
const int dx[] = {0, 1, 0, -1};
const int dy[] = {1, 0, -1, 0};

#define x first
#define y second
#define pb push_back

vector<vector<int>> g, rg, org;
vector<int> matching;
vector<bool> used, visited;
size_t n, m;
double maxspeed;
vector<vector<bool>> board;

bool free(pair<int, int> p)
{
    return p.x >= 0 && p.x < n && p.y >= 0 && p.y < m && !board[p.x][p.y];
}

void dfs1(int v)
{

    if (visited[v])
    {
        return;
    }

    visited[v] = true;

    for (const auto &to : org[v])
    {
        dfs1(to);
    }
}

bool dfs(int v)
{
    if (used[v])
    {
        return false;
    }

    used[v] = true;
    for (const auto to : rg[v])
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
    freopen("birthday.in", "r", stdin);
    freopen("birthday.out", "w", stdout);
    int q;
    cin >> q;

    for (size_t x = 0; x < q; x++)
    {
        cin >> n >> m;

        vector<vector<int>> num;

        g.clear();
        g.resize(n);
        rg.clear();
        rg.resize(n);
        matching.clear();
        matching.resize(m, -1);
        vector<vector<bool>> rev;
        rev.resize(n, vector<bool>(m, true));

        int to;
        for (size_t i = 0; i < n; i++)
        {
            cin >> to;
            while (to != 0)
            {
                g[i].pb(to - 1);
                rev[i][to - 1] = false;
                cin >> to;
            }
        }


        for (size_t i = 0; i < n; i++)
        {
            for (size_t j = 0; j < m; j++)
            {
                if (rev[i][j])
                {
                    rg[i].pb(j);
                }
            }
        }

        for (size_t i = 0; i < n; i++)
        {
            used.clear();
            used.resize(n, false);
            dfs(i);
        }

        vector<pair<int, int>> p;
        vector<vector<bool>> matchingMatrix(n, vector<bool>(m, false));

        set<int> matched;

        for (size_t i = 0; i < m; i++)
        {
            if (matching[i] != -1)
            {
                p.pb({matching[i], i});
                matchingMatrix[matching[i]][i] = true;
                matched.insert(matching[i]);
            }
        }

        org.clear();
        org.resize(n + m);

        for (size_t i = 0; i < n; i++)
        {
            for (size_t j = 0; j < rg[i].size(); j++)
            {
                if (matchingMatrix[i][rg[i][j]])
                {
                    org[rg[i][j] + n].pb(i);
                } else
                {
                    org[i].pb(rg[i][j] + n);
                }
            }
        }


        visited.clear();
        visited.resize(n + m);
        for (size_t i = 0; i < n; i++)
        {
            if (!matched.count(i))
            {
                dfs1(i);
            }
        }


        set<int> cover;

        for (size_t i = 0; i < n + m; i++)
        {
            if (i < n)
            {
                if (!visited[i])
                {
                    cover.insert(i);
                }
            } else
            {
                if (visited[i])
                {
                    cover.insert(i);
                }
            }
        }

        set<int> boys, girls;
        for (size_t i = 0; i < n + m; i++)
        {
            if (!cover.count(i))
            {
                if (i < n)
                {
                    boys.insert(i + 1);
                } else
                {
                    girls.insert(i - n + 1);
                }
            }
        }


        cout << girls.size() + boys.size() << endl;
        cout << boys.size() << " " << girls.size() << endl;
        for (const auto &it : boys)
        {
            cout << it << " ";
        }
        cout << endl;

        for (const auto &it : girls)
        {
            cout << it << " ";
        }
        cout << endl;
    }
    fclose(stdin);
    fclose(stdout);
    return 0;
}