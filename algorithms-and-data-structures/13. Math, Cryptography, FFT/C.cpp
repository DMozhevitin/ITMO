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
const size_t N = 2e6 + 100;

#define x first
#define y second
#define ll __int128
#define pb push_back

ll gcd(ll a, ll b)
{
    if (b == 0)
    {
        return a;
    }

    return gcd(b, a % b);
}

// n -> (x, y) : n = 2^x * y
pair<ll, ll> to2Degs(ll n)
{
    ll cnt = 0;
    while (n % 2 == 0)
    {
        cnt++;
        n /= 2;
    }

    return {cnt, n};
}

ll bin_pow(ll a, ll n, ll mod)
{
    if (n == 0) return 1;

    if (n % 2 == 0)
    {
        ll b = bin_pow(a, n / 2, mod);
        return (b * b) % mod;
    } else
    {
        return (a * bin_pow(a, n - 1, mod)) % mod;
    }
}

bool miller_rabin(ll n, int times)
{
    if (n == 1) return false;
    if (n == 2 || n == 3) return true;
    pair<ll, ll> p = to2Degs(n - 1);
    ll s = p.x;
    ll t = p.y;

    for (int cnt = 0; cnt < times; cnt++)
    {
        ll a = rand() % (n - 1) + 1;
        while (a == 1 || a == n - 1) a = rand() % (n - 1) + 1;

        ll x = bin_pow(a, t, n);

        if (x == 1 || x == n - 1)
        {
            continue;
        }

        bool cont = false;
        for (long long i = 0; i < s - 1; i++)
        {
            x = (x * x) % n;

            if (x == 1)
            {
                return false;
            }

            if (x == n - 1)
            {
                cont = true;
                break;
            }
        }

        if (cont)
        {
            continue;
        } else
        {
            return false;
        }
    }

    return true;
}

vector<ll> sieve_linear(size_t maxn)
{
    vector<ll> primes;
    vector<ll> minDiv(maxn);

    for (ll i = 2; i * i <= maxn; i++)
    {
        if (!minDiv[i])
        {
            minDiv[i] = i;
            primes.push_back(i);
        }

        for (const ll it : primes)
        {
            int j = i * it;
            if (it > minDiv[i] || j > N) break;
            minDiv[j] = it;
        }
    }

    return primes;
}

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
    srand(time(nullptr));

    int n;
    cin >> n;

    for (int i = 0; i < n; i++)
    {
        long long x;
        cin >> x;

        cout << (miller_rabin(x, 10) ? "YES\n" : "NO\n");
    }

    return 0;
}