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

const ll N = 1e6 + 10;
const ll INF = 1e4 + 100;
const ll NO_EDGE = 100000;
const ll M = 3 * 1e3;
const ull prime = 31;
const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, 1, 0, -1};
const ll MOD = 104857601;

#define x first
#define y second
#define pb push_back

ll k;
size_t maxdeg;

template <typename T>
void printv(const vector<T> &v)
{
    for (const auto &it : v)
    {
        cout << it << " ";
    }
    cout << endl;
}

template<typename T>
void printv(const vector<T> &v, size_t r)
{
    for (size_t i = 0; i < r; i++)
    {
        cout << v[i] << " ";
    }

    cout << endl;
}


ll mulMod2(ll a, ll b)
{
    return (a * b) % MOD;
}

ll sumMod2(ll a, ll b)
{
    return ((a % MOD) + (b % MOD)) % MOD;
}

ll subMod2(ll a, ll b)
{
    ll x = (a - b) % MOD;
    return x >= 0 ? x : x + MOD;
}

ll divMod2(ll a, ll b)
{
    return ((a % MOD) / (b % MOD)) % MOD;
}

tuple<ll, ll, ll> gcdEx(ll a, ll b)
{
    if (a == 0)
    {
        return make_tuple(b, 0ll, 1ll);
    }

    tuple<ll, ll, ll> t = gcdEx(b % a, a);
    ll x = subMod2(get<2>(t), mulMod2(b / a, get<1>(t)));
    ll y = get<1>(t);
    return make_tuple(get<0>(t), x, y);
}

ll reverseElement(ll a, ll mod)
{
    tuple<ll, ll, ll> res = gcdEx(a, mod);
    assert(get<0>(res) == 1);

    ll x = get<1>(res);
    return (x % mod + mod) % mod;
}

ll sqrMod(ll x)
{
    return mulMod2(x, x);
}

ll fact(ll x)
{
    ll res = 1;

    for (ll i = 2; i <= x; i++)
    {
        res = mulMod2(res, i);
    }

    return res;
}

ll powModM(ll a, ll n, ll mod)
{
    ll res = 1;

    for (ll i = 1; i <= n; i++)
    {
        res = mulMod2(res, a);
    }

    return res;
}

vector<ll> fpsSum(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> sum(maxdeg + 1, 0);

    for (size_t i = 0; i <= maxdeg; i++)
    {
        sum[i] = sumMod2(a[i], b[i]);
    }

    return sum;
}

ll getKthCoeffDiv(ll k, const vector<ll> &a, const vector<ll> &b, const vector<ll> &c)
{
    ll sum = 0;
    for (size_t i = 1; i <= k; i++)
    {
        sum = sumMod2(sum, mulMod2(b[i], c[k - i]));
    }

    return divMod2(subMod2(a[k], sum), b[0]);

}

ll getKthCoeffMul(ll k, const vector<ll> &a, const vector<ll> &b)
{
    ll coeff = 0;

    for (size_t i = 0; i <= k; i++)
    {
        coeff = sumMod2(coeff, mulMod2(a[i], b[k - i]));
    }

    return coeff;
}

vector<ll> fpsMul(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> mul(a.size() + b.size() + 10, 0);

    for (size_t i = 0; i < mul.size(); i++)
    {

        mul[i] = getKthCoeffMul(i, a, b);
    }

    return mul;
}

vector<ll> polyMul(const vector<ll> &a, const vector<ll> &b, size_t maxDeg)
{
    vector<ll> ans(a.size() + b.size() + 5, 0);

    for (size_t i = 0; i < ans.size(); i = i + 2)
    {
        for (size_t j = 0; j < i + 1; j++)
        {
            ll x = j >= a.size() ? 0 : a[j];
            ll y = (i - j) >= b.size() ? 0 : b[i - j];
            ans[i] = sumMod2(ans[i], mulMod2(x, y));
        }
    }

    return ans;
}

vector<ll> fpsDiv(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> div(maxdeg + 1, 0);

    for (size_t k = 0; k <= maxdeg; k++)
    {
        div[k] = getKthCoeffDiv(k, a, b, div);
    }

    return div;
}

ll calcNth(ll n, vector<ll>& a, vector<ll>& q)
{
    while (n >= k)
    {
        for (int i = k; i <= 2 * k - 1; i++)
        {
            a[i] = 0;
            for (ll j = 1; j <= k; j++)
            {
                a[i] = subMod2(a[i], mulMod2(q[j], a[i - j]));
            }
        }

        vector<ll> _q(q.size());
        for (int i = 0; i < k + 1; i++)
        {
            if (i % 2 == 0)
            {
                _q[i] = q[i];
            } else
            {
                _q[i] = subMod2(MOD, q[i]);
            }
        }

        vector<ll> r = polyMul(q, _q, maxdeg);
        for (int i = 0; i <= k; i++)
        {
            q[i] = r[i * 2];
        }

        int ind = static_cast<int>(n % 2);
        for (int i = ind; i <= 2 * k - 1; i += 2)
        {
            a[i / 2] = a[i];
        }

        n /= 2;
    }

    return a[n];
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n;
    cin >> k >> n;
    maxdeg = 2 * k + 1;
    n--;

    vector<ll> a(maxdeg);

    for (int j = 0; j < k; j++)
    {
        cin >> a[j];
    }

    vector<ll> q(k + 2);
    q[0] = 1ll;

    for (int i = 1; i <= k; i++)
    {
        ll z;
        cin >> z;
        q[i] = subMod2(MOD, z);
    }

    cout << calcNth(n, a, q);
}