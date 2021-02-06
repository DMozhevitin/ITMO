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

vector<ull> hashes, pows;

vector<ull> calcHash(const string &s)
{
    vector<ull> h(s.size());
    ull p = 1;
    ull hash = 0;

    for (size_t i = 0; i < s.size(); i++)
    {
        hash += (s[i] - 'a' + 1) * p;
        pows[i] = p;
        p *= prime;
        h[i] = hash;
    }

    return h;
}

int main()
{
    string s;
    cin >> s;

    pows.resize(s.size());
    hashes = calcHash(s);

    int n;
    cin >> n;

    for (int i = 0; i < n; i++)
    {
        size_t i1, j1, i2, j2;
        cin >> i1 >> j1 >> i2 >> j2;
        i1--, j1--, i2--, j2--;

        if (j1 - i1 != j2 - i2)
        {
            cout << "No\n";
            continue;
        }

        ull hash1 = hashes[j1] - (i1 ? hashes[i1 - 1] : 0);
        ull hash2 = hashes[j2] - (i2 ? hashes[i2 - 1] : 0);

        if (i1 > i2)
        {
            swap(i1, i2);
            swap(j1, j2);
            swap(hash1, hash2);
        }

        if (hash1 * pows[i2 - i1] == hash2)
        {
            cout << "Yes\n";
        } else
        {
            cout << "No\n";
        }
    }
}