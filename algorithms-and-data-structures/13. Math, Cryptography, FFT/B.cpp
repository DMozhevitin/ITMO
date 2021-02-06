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

const size_t N = 2e6 + 100;
const ll INF = 1e18 + 100;
const ll M = 2000;

#define x first
#define y second
#define pb push_back

vector<int> sieve_linear(size_t maxn)
{
    vector<int> primes;
    vector<int> minDiv(maxn);

    for (int i = 2; i * i <= maxn; i++)
    {
        if (!minDiv[i])
        {
            minDiv[i] = i;
            primes.push_back(i);
        }

        for (const int it : primes)
        {
            int j = i * it;
            if (it > minDiv[i] || j > N) break;
            minDiv[j] = it;
        }
    }

    return primes;
}

void factorize(int n, const vector<int> &primes)
{
    while (n > 1)
    {
        bool found_div = false;
        for (const int prime : primes)
        {
            if (prime * prime > n)
            {
                break;
            }

            if (n % prime == 0)
            {
                n /= prime;
                found_div = true;
                cout << prime << " ";
                break;
            }
        }

        if (!found_div)
        {
            cout << n << "\n";
            return;
        }
    }

    cout << "\n";
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    vector<int> primes = sieve_linear(N);

    for (size_t i = 0, x; i < n; i++)
    {
        cin >> x;
        factorize(x, primes);
    }
}