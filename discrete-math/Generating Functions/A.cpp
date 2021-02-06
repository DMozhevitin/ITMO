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
const ll INF = 1e4 + 100;
const ll NO_EDGE = 100000;
const ll M = 1e3;
const ull prime = 31;
const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, 1, 0, -1};
const ll MOD = 998244353;

#define x first
#define y second
#define pb push_back

vector<ll> a, b;
size_t n, m, maxdeg;

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
    return ((a % MOD) * (b % MOD)) % MOD;
}

ll sumMod2(ll a, ll b)
{
    return ((a % MOD) + (b % MOD)) % MOD;
}

ll subMod2(ll a, ll b)
{
    ll x = ((a % MOD) - (b % MOD)) % MOD;
    return x > 0 ? x : x + MOD;
}

ll divMod2(ll a, ll b)
{
    return ((a % MOD) / (b % MOD)) % MOD;
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
    vector<ll> mul(maxdeg, 0);

    for (size_t i = 0; i < mul.size(); i++)
    {
        mul[i] = getKthCoeffMul(i, a, b);
    }

    return mul;
}

//first 1001
vector<ll> fpsDiv(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> div(maxdeg + 1, 0);

    for (size_t k = 0; k <= maxdeg; k++)
    {
        div[k] = getKthCoeffDiv(k, a, b, div);
    }

    return div;
}


int main()
{

    cin >> n >> m;
    maxdeg = 3000;

    a.resize(maxdeg + 1);
    b.resize(maxdeg + 1);


    for (size_t i = 0; i <= n; i++)
    {
        cin >> a[i];
    }

    for (size_t j = 0; j <= m; j++)
    {
        cin >> b[j];
    }

    for (size_t i = n + 1; i <= maxdeg; i++)
    {
        a[i] = 0;
    }

    for (size_t i = m + 1; i <= maxdeg; i++)
    {
        b[i] = 0;
    }

    auto sum = fpsSum(a, b);
    cout << max(n, m) << endl;
    printv(sum, max(n, m) + 1);

    auto mul = fpsMul(a, b);
    cout << n + m << endl;
    printv(mul, n + m + 1);


    auto div = fpsDiv(a, b);
    printv(div, 1000);
}