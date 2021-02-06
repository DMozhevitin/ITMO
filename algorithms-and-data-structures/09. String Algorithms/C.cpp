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

const ll N = 1 * 1e7 + 10;
const ll INF = 1e18 + 100;
const ll NO_EDGE = 100000;
const ll M = 5e3 + 100;
const ull prime = 31;

#define x first
#define y second
#define pb push_back

vector<int> zf;

vector<int> zFunction(const string &s)
{
    vector<int> z(s.size());
    for (int i = 1, left = 0, right = 0; i < s.size(); i++)
    {
        if (i <= right) z[i] = min(right - i + 1, z[i - left]);
        while (i + z[i] < s.size() && s[i + z[i]] == s[z[i]]) z[i]++;
        if (i + z[i] - 1 > right)
        {
            left = i;
            right = i + z[i] - 1;
        }

    }

    return z;
}

int main()
{
    string s, t;
    cin >> s >> t;

    size_t start = s.size() + 1;
    s += "#" + t;
    zf = zFunction(s);

    vector<size_t> ans;
    for (size_t i = start; i < zf.size(); i++)
    {
        if (zf[i] == start - 1)
        {
            ans.pb(i - start + 1);
        }
    }

    cout << ans.size() << "\n";
    for (const auto &it : ans)
    {
        cout << it << " ";
    }
}