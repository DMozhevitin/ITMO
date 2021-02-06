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

ll n, m, k;
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

inline ll fact(ll x)
{
    ll res = 1;

    for (ll i = 2; i <= x; ++i)
    {
        res *= i;
    }

    return res;
}

inline ll combinations(ll n, ll k)
{
    if (k == 0 || k == n) return 1;

    ll num = 1;

    for (ll i = 1; i <= k; i++)
    {
        num *= (n - i + 1);
    }

    return num / fact(k);
}

inline vector<ll> fpsSum(const vector<ll> &a, const vector<ll> &b)
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

inline ll powModM(ll a, ll n, ll mod)
{
    ll res = 1;

    for (ll i = 1; i <= n; ++i)
    {
        res = mulMod2(res, a);
    }

    return res;
}

inline vector<ll> emptyObj()
{
    return vector<ll>(7, 0ll);
}

inline vector<ll> getLast(vector<vector<ll>> &v)
{
    vector<ll> b = v.back();
    v.pop_back();
    return b;
}

inline vector<ll> buildPair(const vector<ll> &f, const vector<ll> &s)
{
    vector<ll> res = emptyObj();
    for (size_t i = 0; i < f.size(); i++)
    {
        if (f[i] == 0) continue;
        for (size_t j = 0; j < s.size(); j++)
        {
            if (i + j >= 7) break;
            res[i + j] += f[i] * s[j];
        }
    }

    return res;
}

inline vector<ll> buildMSet(const vector<ll> &obj)
{
    vector<vector<ll>> dp(7);
    for (size_t i = 0; i < dp.size(); i++)
    {
        if (!i)
        {
            dp[i].resize(7, 1);
        } else
        {
            dp[i].resize(7, 0);
        }
    }

    for (size_t i = 1; i < 7; i++)
    {
        for (size_t j = 1; j < 7; j++)
        {
            if (!obj[j])
            {
                dp[i][j] = dp[i][j - 1];
                continue;
            }
            for (size_t ind = 0; ind <= i / j; ind++)
            {
                ll n_ = obj[j] - 1 + ind;
                dp[i][j] += dp[i - j * ind][j - 1] * combinations(n_, ind);
            }
        }
    }

    vector<ll> res;
    for (size_t i = 0; i < 7; i++)
    {
        res.pb(dp[i][i]);
    }

    return res;
}

inline vector<ll> buildSeq(const vector<ll> &obj)
{
    vector<ll> res = emptyObj();

    for (size_t i = 0; i < 7; i++)
    {
        if (!i)
        {
            res[i] = 1;
            continue;
        }

        for (size_t j = 1; j <= i; j++)
        {
            res[i] += res[i - j] * obj[j];
        }
    }

    return res;
}

int main()
{
    string s;
    getline(cin, s);

    reverse(s.begin(), s.end());
    vector<vector<ll>> objects;

    for (size_t i = 0; i < s.size(); i++)
    {
        if (s[i] == 'S' || s[i] == 'L')
        {
            vector<ll> obj = getLast(objects);
            objects.pb(s[i] == 'S' ? buildMSet(obj) : buildSeq(obj));
        } else if (s[i] == 'P')
        {
            vector<ll> obj1 = getLast(objects);
            vector<ll> obj2 = getLast(objects);
            objects.pb(buildPair(obj1, obj2));
        } else if (s[i] == 'B')
        {
            vector<ll> B = emptyObj();
            B[1] = 1;
            objects.pb(B);
        }
    }

    printv(getLast(objects));
}
