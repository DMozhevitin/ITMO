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
        cout << it << " ";
    }
    cout << endl;
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

inline vector<ld> compressPolynom(vector<ld> &p)
{
    const ld eps = 1e5;
    size_t deg = p.size() - 1;
    while (deg > 0 && p[deg] == 0) deg--;

    p.resize(deg + 1);
    return p;
}

vector<ld> polySum(const vector<ld> &a, const vector<ld> &b)
{
    vector<ld> sum(max(a.size(), b.size()), 0);

    for (ll i = 0; i < sum.size(); ++i)
    {
        ld x = (i < a.size() ? a[i] : 0);
        ld y = (i < b.size() ? b[i] : 0);

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

inline vector<ld> polyMul(const vector<ld> &a, const vector<ld> &b)
{
    vector<ld> ans(a.size() + b.size() + 5, 0);

    for (int i = 0; i < ans.size(); ++i)
    {
        for (int j = 0; j < i + 1; ++j)
        {
            ld x = j >= a.size() ? 0 : a[j];
            ld y = (i - j) >= b.size() ? 0 : b[i - j];
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

template <typename T>
vector<vector<T>> transpose(const vector<vector<T>> &v)
{
    size_t n_ = v.size();
    size_t m_ = (v.empty() ? 0 : v[0].size());
    vector<vector<T>> res(m_, vector<T>(n_));

    for (size_t i = 0; i < n_; i++)
    {
        for (size_t j = 0; j < m_; j++)
        {
            res[j][i] = v[i][j];
        }
    }

    return res;
}

vector<ld> gauss(vector<vector<ld>> &a, vector<ld> &b)
{
    ll k = 0, ind;
    const ld eps = 1e-5;
    vector<ld> res(a.size());
    ld mx;

    while (k < a.size())
    {
        mx = abs(a[k][k]);
        ind = k;

        for (ll i = k + 1; i < a.size(); i++)
        {
            if (mx < abs(a[i][k]))
            {
                mx = abs(a[i][k]);
                ind = i;
            }
        }

        if (eps > mx) throw std::runtime_error("zero column!");

        for (ll j = 0; j < a.size(); j++)
        {
            swap(a[k][j], a[ind][j]);
        }

        swap(b[k], b[ind]);

        for (ll i = k; i < a.size(); i++)
        {
            ld z = a[i][k];
            if (eps > abs(z)) continue;

            for (ll j = 0; j < a.size(); j++)
            {
                a[i][j] /= z;
            }

            b[i] /= z;

            if (i == k) continue;

            for (ll j = 0; j < a.size(); j++)
            {
                a[i][j] -= a[k][j];
            }

            b[i] -= b[k];
        }
        k++;
    }

    for (k = a.size() - 1; k >= 0; k--)
    {
        res[k] = b[k];
        for (ll i = 0; i < k; i++)
        {
            b[i] -= a[i][k] * res[k];
        }
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

    vector<ld> sum {0};
    vector<vector<ld>> slau;
    vector<ld> fCoeffs;

    ll f = fact(k);
    ld denom = fact(k) * binPow(r, m);

    for (ll i = 0; i <= m; i++)
    {
        vector<ld> q;
        q.pb(1);

        for (ll j = 1; j <= k; j++)
        {
            vector<ld> qq;
            qq.pb(j - i);
            qq.pb(1);
            q = polyMul(q, qq);
        }

        slau.pb(q);
        fCoeffs.pb(denom * p[i]);
    }


    for (auto &it : slau)
    {
        reverse(it.begin(), it.end());
    }

    slau = transpose(slau);

    reverse(fCoeffs.begin(), fCoeffs.end());

    vector<ld> res = gauss(slau, fCoeffs);


    res = compressPolynom(res);

    vector<ll> P(res.size());
    for (size_t i = 0; i < res.size(); i++)
    {
        P[i] = static_cast<ll>(round(res[i] / binPow(r, m - i)));
    }

    cout << P.size() - 1 << endl;
    printv(P);


    vector<ld> q;
    q.pb(1);
    q.pb(-r);

    vector<ld> rs;
    rs.pb(1);

    for (size_t i = 0; i < k + 1; i++)
    {
        rs = polyMul(rs, q);
    }

    vector<ll> Q;
    for (const auto it : rs)
    {
        Q.pb(static_cast<ll>(it));
    }

    cout << Q.size() - 1 << endl;
    printv(Q);
}