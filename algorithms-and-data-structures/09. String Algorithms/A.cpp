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

vector<int> pf;

vector<int> cmp(const string &s)
{
    vector<int> pf;
    pf.resize(s.size());
    pf[0] = 0;

    for (size_t i = 1; i < s.size(); i++)
    {
        int j = pf[i - 1];
        while (j && s[i] != s[j]) j = pf[j - 1];
        if (s[i] == s[j]) j++;
        pf[i] = j;
    }

    return pf;
}

int main()
{
    string s;
    cin >> s;

    pf = cmp(s);
    for (const auto &it : pf)
    {
        cout << it << " ";
    }
}