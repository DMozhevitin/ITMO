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
    template<> struct hash<set<int>>
    {
        size_t operator()(const set<int> &s) const
        {
            return s.size();
        }
    };
}
 
unordered_set<set<int>, hash<set<int>>> independentSets, used;
 
bool checkSecondAxiom(set<int> &s)
{
    if (independentSets.find(s) == independentSets.end())
    {
        return false;
    }
 
    used.insert(s);
    set<int> s1 = s;
    for (auto elem : s)
    {
        s1.erase(elem);
        if (used.find(s1) == used.end() && !checkSecondAxiom(s1))
        {
            s1.insert(elem);
            return false;
        }
        s1.insert(elem);
    }
 
    return true;
}
 
bool checkThirdAxiom(set<int> &s1, set<int> &s2)
{
    for (auto elem : s2)
    {
        if (s1.count(elem))
        {
            continue;
        }
 
        s1.insert(elem);
        if (independentSets.count(s1))
        {
            s1.erase(elem);
            return true;
        }
        s1.erase(elem);
    }
 
    return false;
}
 
bool isMatroid()
{
    for (auto s : independentSets)
    {
        if (!checkSecondAxiom(s))
        {
            return false;
        }
    }
 
    for (auto s1 : independentSets)
    {
        for (auto s2 : independentSets)
        {
            if (s1.size() < s2.size())
            {
                if (!checkThirdAxiom(s1, s2))
                {
                    return false;
                }
            }
        }
    }
 
    return true;
}
 
 
int main()
{
    freopen("check.in", "r", stdin);
    freopen("check.out", "w", stdout);
 
    size_t n, m;
    cin >> n >> m;
 
    bool isEmptySet = false;
 
    for (size_t i = 0; i < m; i++)
    {
        size_t sizeOfSet;
        cin >> sizeOfSet;
 
        if (sizeOfSet == 0)
        {
            isEmptySet = true;
        }
 
        set<int> s;
        for (size_t j = 0; j < sizeOfSet; j++)
        {
            int x;
            cin >> x;
            s.insert(x);
        }
 
        independentSets.insert(s);
    }
 
    //first axiom
    if (!isEmptySet)
    {
        return cout << "NO\n", 0;
    }
 
    if (isMatroid())
    {
        cout << "YES\n";
    } else
    {
        cout << "NO\n";
    }
 
    fclose(stdin);
    fclose(stdout);
}