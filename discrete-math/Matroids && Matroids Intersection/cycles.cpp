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
 
namespace std
{
    template<>
    struct hash<set<int>>
    {
        size_t operator()(const set<int> &s) const
        {
            return s.size();
        }
    };
}
 
unordered_set<set<int>> cycles;
vector<pl> weight;
 
bool checkIndependence(const set<int> &base)
{
    for (const auto &cycle: cycles)
    {
        bool allCycle = true;
        for (const auto &elem : cycle)
        {
            allCycle &= (base.find(elem) != base.end());
        }
 
        if (allCycle)
        {
            return false;
        }
    }
 
    return true;
}
 
int main()
{
    freopen("cycles.in", "r", stdin);
    freopen("cycles.out", "w", stdout);
 
    size_t n, m;
    cin >> n >> m;
 
    weight.resize(n);
 
    for (ll i = 0; i < n; i++)
    {
        ll x;
        cin >> x;
        weight[i] = {x, i};
    }
 
    sort(weight.rbegin(), weight.rend());
 
    for (size_t i = 0; i < m; i++)
    {
        size_t sizeOfSet;
        cin >> sizeOfSet;
 
        set<int> s;
        for (size_t j = 0; j < sizeOfSet; j++)
        {
            int x;
            cin >> x;
            x--;
            s.insert(x);
        }
 
        cycles.insert(s);
    }
 
    set<int> maxBase;
    ll ans = 0;
    for (const auto &it : weight)
    {
        maxBase.insert(it.y);
        ans += it.x;
        if (!checkIndependence(maxBase))
        {
            ans -= it.x;
            maxBase.erase(it.y);
        }
    }
 
    cout << ans << endl;
     
    fclose(stdin);
    fclose(stdout);
}