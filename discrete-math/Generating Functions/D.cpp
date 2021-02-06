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

const ll N = 1e6 + 10;
const ll INF = 1e4 + 100;
const ll NO_EDGE = 100000;
const ll M = 3 * 1e3;
const ull prime = 31;
const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, 1, 0, -1};
const ll MOD = 998244353;
const ll MOD2 = MOD - 2;

#define x first
#define y second
#define pb push_back

ll maxdeg = 201;

template<typename T>
inline void printv(const vector<T> &v)
{
    for (auto it : v)
    {
        printf("%lli ", it);
    }
    printf("\n");
}

template<typename T>
inline void printv(const vector<T> &v, ll r)
{
    for (ll i = 0; i < r; ++i)
    {
        cout << v[i] << " ";
    }

    cout << endl;
}

inline ll mulMod2(ll a, ll b)
{
    return (a * b) % MOD;
}

inline ll sumMod2(ll a, ll b)
{
    return (a + b) % MOD;
}

inline ll subMod2(ll a, ll b)
{
    return (a - b + MOD);
}

inline ll divMod2(ll a, ll b)
{
    return ((a % MOD) / (b % MOD)) % MOD;
}

inline ll binPow(ll a, ll n)
{
    if (n == 0) return 1;

    if (n & 1)
    {
        return binPow(a, n - 1) * a;
    } else
    {
        ll b = binPow(a, n / 2);
        return b * b;
    }
}

inline ll gcd(ll a, ll b)
{
    if (b == 0) return a;
    return gcd(b, a % b);
}

inline ll sign(ll x)
{
    return (x >= 0 ? 1 : -1);
}

inline void printPolynom(const vector<ll> &p)
{
    cout << p[0];
    for (size_t i = 1; i < p.size(); i++)
    {
        cout << " + " << "(" << p[i] << ")" <<"n^" << i;
    }
    cout << endl;
}

inline vector<ll> compressPolynom(vector<ll> &p)
{
    size_t deg = p.size() - 1;
    while (deg > 0 && !p[deg]) deg--;

    p.resize(deg + 1);
    return p;
}

vector<ll> polySum(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> sum(max(a.size(), b.size()), 0);

    for (ll i = 0; i < sum.size(); ++i)
    {
        ll x = (i < a.size() ? a[i] : 0);
        ll y = (i < b.size() ? b[i] : 0);

        sum[i] = x + y;
    }

    return sum;
}


inline ll getKthCoeffDiv(ll k, const vector<ll> &a, const vector<ll> &b, const vector<ll> &c)
{
    ll sum = 0;
    for (ll i = 1; i <= k; ++i)
    {
        sum = sumMod2(sum, mulMod2(b[i], c[k - i]));
    }

    return divMod2(subMod2(a[k], sum), b[0]);

}

inline vector<ll> polyMul(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> ans(a.size() + b.size() + 5, 0);

    for (int i = 0; i < ans.size(); ++i)
    {
        for (int j = 0; j < i + 1; ++j)
        {
            ll x = j >= a.size() ? 0 : a[j];
            ll y = (i - j) >= b.size() ? 0 : b[i - j];
            ans[i] += x * y;
        }
    }


    return compressPolynom(ans);
}

inline vector<ll> fpsDiv(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> div(maxdeg + 1, 0);

    for (ll k = 0; k <= maxdeg; k++)
    {
        div[k] = getKthCoeffDiv(k, a, b, div);
    }

    return div;
}

inline ll sqrMod(ll x)
{
    return mulMod2(x, x);
}

inline ll fact(ll x)
{
    ll res = 1;

    for (ll i = 2; i <= x; ++i)
    {
        res *= i;
    }

    return res;
}

int main()
{
    ll r, k;
    cin >> r >> k;

    vector<ll> p(static_cast<unsigned int>(k + 1));
    ll m = -1;

    for (size_t i = 0; i <= k; i++)
    {
        cin >> p[i];
    }

    for (int i = p.size() - 1; i >= 0; i--)
    {
        if (p[i] != 0)
        {
            m = i;
            break;
        }
    }

    vector<ll> sum {0};

    for (ll i = 0; i <= m; i++)
    {
        vector<ll> q {1};

        for (ll j = 1; j <= k; j++)
        {
            vector<ll> qq {j - i, 1};
            q = polyMul(q, qq);
        }

        ll rm_i = binPow(r, m - i);
        ll coeff = rm_i * p[i];

        for (long long &elem : q)
        {
            elem *= coeff;
        }

        sum = polySum(sum, q);
    }


    ll denom = fact(k) * binPow(r, m);

    vector<ll> nums(sum.size()), denoms(sum.size());

    for (size_t i = 0; i < sum.size(); i++)
    {
        ll nod = (sum[i] > 0 ? gcd(sum[i], denom) : gcd(-sum[i], denom));
        nums[i] = sum[i] / nod;
        denoms[i] = denom / nod;
    }


    for (size_t i = 0; i < sum.size(); i++)
    {
        cout << nums[i] << "/" << denoms[i]  << " ";
    }
    cout << endl;
}