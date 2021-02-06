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
#include <cassert>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;
typedef unsigned long long ull;
typedef unsigned int uint;
typedef tuple<vector<ll>, vector<ll>, vector<ll>> tuple3v;

const size_t N = 2e7 + 100;
const ll INF = 1e18 + 100;
const ll M = 3 * 1e3;

#define x first
#define y second
#define pb push_back

bool sieve(vector<bool> &prime, size_t maxn)
{
    for (ll i = 2; i <= maxn; i++)
    {
        if (prime[i])
        {
            if (i * i > maxn) continue;

            for (ll j = i * i; j <= maxn; j += i)
            {
                prime[j] = false;
            }
        }
    }
}

vector<bool> prime;
int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    prime.resize(N, true);
    ll n;
    cin >> n;

    prime[0] = prime[1] = false;
    sieve(prime, N);

    for (size_t i = 0, x; i < n; i++)
    {
        cin >> x;
        cout << (prime[x] ? "YES\n" : "NO\n");
    }
}