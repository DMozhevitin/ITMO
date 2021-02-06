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

ll n, m;
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

inline ll gcdEx(ll a, ll b, ll &x, ll &y)
{
    if (a == 0)
    {
        x = 0;
        y = 1;
        return b;
    }

    ll x_, y_;
    ll d = gcdEx(b % a, a, x_, y_);
    x = y_ - (b / a) * x_;
    y = x_;

    return d;
}

inline ll binPow(ll a, ll n)
{
    if (n == 0) return 1;

    if (n & 1)
    {
        return mulMod2(binPow(a, n - 1), a);
    } else
    {
        ll b = binPow(a, n / 2);
        return mulMod2(b, b);
    }
}

inline ll reverseElement(ll a)
{
    return binPow(a, MOD2);
}

vector<ll> fpsSum(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> sum(maxdeg + 1, 0);

    for (ll i = 0; i <= maxdeg; ++i)
    {
        sum[i] = sumMod2(a[i], b[i]);
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

inline ll getKthCoeffMul(ll k, const vector<ll> &a, const vector<ll> &b)
{
    ll coeff = 0;

    for (ll i = 0; i <= k; ++i)
    {
        coeff = sumMod2(coeff, mulMod2(a[i], b[k - i]));
    }

    return coeff;
}

//multiply formal power series
inline vector<ll> fpsMul(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> mul(maxdeg, 0);

    for (ll i = 0; i < mul.size(); ++i)
    {
        mul[i] = getKthCoeffMul(i, a, b);
    }

    return mul;
}

inline vector<ll> compressPolynom(vector<ll> &p)
{
    size_t deg = p.size() - 1;
    while (deg > 0 && !p[deg]) deg--;

    p.resize(deg + 1);
    return p;
}

inline vector<ll> polyMul(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> ans(a.size() + b.size() + 3, 0);

    for (size_t i = 0; i < a.size(); ++i)
    {
        if (!a[i]) continue;

        for (size_t j = 0; j < b.size(); ++j)
        {
            if (i + j >= m + 1) break;
            ans[i + j] = sumMod2(ans[i + j], mulMod2(a[i], b[j]));
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
        res = mulMod2(res, i);
    }

    return res;
}

inline ll powModM(ll a, ll n, ll mod)
{
    ll res = 1;

    for (ll i = 1; i <= n; ++i)
    {
        res = mulMod2(res, a);
    }

    return res;
}

inline ll getKthCoeffTaylorSqrt(ll k)
{
    ll x = 1, y = 1;
    for (int i = 0; i < k; ++i)
    {
        x = mulMod2(x, subMod2(1, 2 * i));
        y = mulMod2(y, 2 * i + 2);
    }

    return mulMod2(x, reverseElement(y));
}

inline void taylor(const vector<ll> &p, const size_t dg)
{
    const ll minus1Mod = -1ll + MOD;

    ll coeffSqrt = 1;
    ll coeffExp = 1;
    ll coeffLn = 0;

    ll ansSqrt[dg];
    ll ansExp[dg];
    ll ansLn[dg];

    for (size_t i = 0; i < dg; i++)
    {
        ansSqrt[i] = ansExp[i] = ansLn[i] = 0;
    }

    vector<ll> ppow;
    ppow.pb(1ll);

    ansSqrt[0] = 1;
    ansExp[0] = 1;
    ansLn[0] = 0;

    ll f = 1;

    for (int i = 1; i < dg; ++i)
    {
        ppow = polyMul(ppow, p);

        coeffSqrt = getKthCoeffTaylorSqrt(i);

        f = mulMod2(f, i);
        coeffExp = reverseElement(f);
        coeffLn = mulMod2((i & 1 ? 1ll : minus1Mod), reverseElement(i));

        for (int j = 0; j < dg; ++j)
        {
            ll z = (j < ppow.size() ? ppow[j] : 0);

            ansSqrt[j] = sumMod2(ansSqrt[j], mulMod2(z, coeffSqrt));
            ansExp[j] = sumMod2(ansExp[j], mulMod2(z, coeffExp));
            ansLn[j] = sumMod2(ansLn[j], mulMod2(z, coeffLn));
        }
    }

    for (size_t i = 0; i < dg; i++)
    {
        cout << ansSqrt[i] << " ";
    }
    cout << '\n';

    for (size_t i = 0; i < dg; i++)
    {
        cout << ansExp[i] << " ";
    }
    cout << '\n';

    for (size_t i = 0; i < dg; i++)
    {
        cout << ansLn[i] << " ";
    }
    cout << '\n';
}

/*  __________
  /
 v 1 + P(t)   */
vector<ll> coeffsSqrt(const vector<ll> &p, const size_t dg)
{
    vector<ll> coeffs(dg);
    coeffs[0] = 1;
    for (ll i = 1; i < dg; ++i)
    {
        coeffs[i] = getKthCoeffTaylorSqrt(i);
    }

    return coeffs;
}

//exp(P(t))
vector<ll> coeffsExp(const vector<ll> &p, const size_t dg)
{
    vector<ll> coeffs(dg);
    coeffs[0] = 1;
    for (ll i = 1; i < dg; ++i)
    {
        ll f = fact(i);
        coeffs[i] = (reverseElement(f)) % MOD;
    }

    return coeffs;
}

//ln(1 + P(t))
vector<ll> coeffsLn(const vector<ll> &p, const size_t dg)
{
    vector<ll> coeffs(dg);
    coeffs[0] = 0;
    ll j = subMod2(0ll, 1ll);
    for (ll i = 1; i < dg; ++i)
    {
        j *= -1;
        j += MOD;
        coeffs[i] = mulMod2(j, reverseElement(i));
    }


    return coeffs;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n >> m;
    vector<ll> p(n + 1, 0ll);

    for (int i = 0; i <= n; ++i)
    {
        cin >> p[i];
    }

    taylor(p, static_cast<const size_t>(m));
}