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
const ll M = 3 * 1e3;
const ull prime = 31;
const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, 1, 0, -1};
const ll MOD = 1e9 + 7;

#define x first
#define y second
#define pb push_back

vector<ll> a, b;
size_t maxdeg;

template<typename T>
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
        cout << "j = " << i << endl;
        coeff = sumMod2(coeff, mulMod2(a[i], b[k - i]));
    }

    return coeff;
}

vector<ll> fpsMul(const vector<ll> &a, const vector<ll> &b)
{
    vector<ll> mul(maxdeg, 0);

    for (size_t i = 0; i < mul.size(); i++)
    {
        cout << "i = " << i << endl;

        mul[i] = getKthCoeffMul(i, a, b);
    }

    return mul;
}

vector<ll> polyMul(const vector<ll> &a, const vector<ll> &b, size_t maxDeg)
{
    vector<ll> ans(maxDeg, 0);

    for (size_t i = 0; i < a.size(); i++)
    {
        for (size_t j = 0; j < b.size(); j++)
        {
            if (i + j >= maxDeg) continue;

            ans[i + j] += a[i] * b[j];
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


int main()
{
    size_t k, m;
    cin >> k >> m;

    vector<ll> weight(k, 0);
    vector<ll> dp(m + 1, 0);
    vector<ll> cntTrees(m + 1, 0);
    unordered_set<ll> weights;

    for (size_t i = 0; i < k; i++)
    {
        cin >> weight[i];
        weights.insert(weight[i]);
    }

    dp[0] = cntTrees[0] = 1;

    for (ll treew = 1; treew <= m; treew++)
    {
        for (const auto w : weight)
        {
            if (w > treew) continue;
            cntTrees[treew] = sumMod2(cntTrees[treew], dp[treew - w]);
        }

        for (ll rootw = 0; rootw <= treew; rootw++)
        {
            ll subtreesw = treew - rootw;

            dp[treew] = sumMod2(dp[treew], (mulMod2(cntTrees[rootw], cntTrees[subtreesw])));
        }
    }

    for (size_t i = 1; i < cntTrees.size(); i++)
    {
        cout << cntTrees[i] << " ";
    }
}